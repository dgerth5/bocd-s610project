library(ggplot2)
library(reshape2)

source("bayesian-online-changepoint-script.R") #call in functions from script

# Simulated Data
set.seed(5)
data1 <- c(rnorm(50))
data2 <- c(rnorm(50), rnorm(50, mean = 5, sd = 1))
data3 <- c(rnorm(50), rnorm(50, mean = 2, sd = 1))

plot_function <- function(data, title){
  
  b <- bocd(data)
  
  max_length <- max(sapply(b$run_length_probs, length))
  
  # row = step of data
  # col = prob of run length 
  matrix_result <- do.call(rbind, lapply(b$run_length_probs, function(vec) {
    c(vec, rep(0, max_length - length(vec)))
  }))
  
  matrix_transposed <- t(matrix_result)
  df <- melt(matrix_transposed)
  
  # Plot using ggplot2
  ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      x = "Data",
      y = "Pun Length",
      title = title
    ) +
    theme_minimal()

}

plot_function(data1, "BOCD Test 1: No Changepoints")
plot_function(data2, "BOCD Test 2: Distinct Changepoint")
plot_function(data3, "BOCD Test 3: Unclear Changepoint")
