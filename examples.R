# Simulated Data
set.seed(5)
data <- c(rnorm(50, mean = 0, sd = 1), rnorm(200, mean = 5, sd = 1))
b <- bocd(data)

# plot

max_length <- max(sapply(b$run_length_probs, length))

matrix_result <- do.call(rbind, lapply(b$run_length_probs, function(vec) {
  c(vec, rep(0, max_length - length(vec)))
}))
