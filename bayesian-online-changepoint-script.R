### unknown mean, fixed variance

# step 1
hazard_fn <- function(lambda){
  return(1 / lambda)
}

# step 3
predictive_prob_norm <- function(x_t, mu, sigma, sigma_n){
  # explainer: https://www2.stat.duke.edu/courses/Fall21/sta601.001/slides/03-normal-predictive-distributions.html#6
  # bayesian updating for normal distribution
  update_variance <- sigma^2 + sigma_n^2
  dnorm(x_t, mean = mu, sd = sqrt(update_variance))
}

# step 4
growth_prob <- function(data_prob, run_length_prob, hazard){
  data_prob * run_length_prob * hazard
}

# step 5
cp_prob <- function(data_prob, run_length_prob, hazard){
  sum(data_prob * run_length_prob * hazard)
}

# step 6
calc_evidence_fn <- function(growth_prob, cp_prob){
  sum(growth_prob) + cp_prob # need to add when run length is 0 (step 5) and run length > 0 (step 4)
}

# step 7
run_length_dist <- function(growth_prob, cp_prob, evidence){
  update_rl_probs <- numeric(length(growth_prob) + 1) # need to add 1 to initialization to account for r = 0
  
  update_rl_probs[2:length(update_rl_probs)] <- growth_prob / evidence # probability of r_t = i > 0 
  update_rl_probs[1] <- cp_prob / evidence # probability of r_t = 0
  update_rl_probs
}

# step 8
update_stats <- function(t, data, new_rl_probs, mu_0, sigma_0, sigma){

  # t: current time index
  # data: observations up to time t
  # new_rl_probs: probability for each run length
  # mu_0, sigma_0: prior mean, standard dev
  # sigma: known standard deviation of data

  n_runs <- length(new_rl_probs)
  new_means <- numeric(n_runs)
  new_variances <- numeric(n_runs)

  # using priors to start for run length = 0
  new_means[1] <- mu_0
  new_variances[1] <- sigma_0^2

  # update for run lengths greater than 0

  if (n_runs > 1){
    for (i in 2:n_runs){
      r <- i - 1 # run length

      data_cur_run <- data[(max(1, t - r + 1)):t] # avoid negative combos of current time index t and run length r

      # update parameters directly using bayes
      n <- length(data_cur_run)
      sigma_n <- 1 / sqrt(1/sigma_0^2 + n/sigma^2)
      mu_n <- sigma_n^2 * (mu_0/sigma_0^2 + sum(data_cur_run) / sigma^2)

      new_means[i] <- mu_n
      new_variances[i] <- sigma_n^2
    }
  }

  return(list(means = new_means, variances = new_variances))

}

# step 9
calculate_predictions <- function(means, vars, probs) {
  pred_mean <- sum(means * probs)
  pred_var <- sum((vars + means^2) * probs) - pred_mean^2
  pred_std <- sqrt(pred_var)
  list(mean = pred_mean, std = pred_std)
}

bocd <- function(data,
                 mu_0 = 0,          # prior mean
                 sigma_0 = 1,       # prior std dev of mean
                 sigma = 1,         # prior sigma
                 lambda = 100) {    # expected run length
  
  # check that data is numeric
  if (!is.numeric(data)){
    print("Data must be numeric")
  }
  
  # check that there are no NA's in data
  if (any(is.na(data))) {
    print("Remove NA's")
  }

  # set up data
  n <- length(data)
  hazard <- hazard_fn(lambda)

  pred_means <- numeric(n)
  pred_stds <- numeric(n)
  run_length_probs <- list()

  # set priors
  probs <- 1
  means <- mu_0
  variances <- sigma_0^2

  # process each observation as it comes in (online portion)
  for(t in 1:n) {

    x_t <- data[t]

    # step 3: calculate predictive probabilities
    pred_probs <- predictive_prob_norm(x_t, means, sigma, sqrt(variances))

    # step 4: calculate growth probabilities
    growth_probs <- growth_prob(pred_probs, probs, 1 - hazard)  # 1-hazard for probability of not having a change point

    # step 5: calculate changepoint probability
    cp_prob <- cp_prob(pred_probs, probs, hazard)

    # step 6: calculate evidence
    evidence <- calc_evidence_fn(growth_probs, cp_prob)

    # step 7: update run length dist
    new_probs <- run_length_dist(growth_probs, cp_prob, evidence)

    # step 8: update sufficient statistics
    stats <- update_stats(t, data, new_probs, mu_0, sigma_0, sigma)

    # step 9: calculate predictions
    preds <- calculate_predictions(stats$means, stats$variances, new_probs)

    # store results
    pred_means[t] <- preds$mean
    pred_stds[t] <- preds$std
    run_length_probs[[t]] <- new_probs

    # update
    probs <- new_probs
    means <- stats$means
    variances <- stats$variances
  }

  list(
    pred_means = pred_means,
    pred_stds = pred_stds,
    run_length_probs = run_length_probs
  )
}

