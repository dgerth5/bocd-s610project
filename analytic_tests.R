library(testthat)

source("bayesian-online-changepoint-script.R")

new_data <- 0
bocd_data <- bocd(new_data)


new_data2 <- rnorm(100)
bocd_data_2 <- bocd(new_data2)

sum_probs <- numeric(100)

for (i in 1:100){
  sum_probs[i] <- sum(bocd_data_2$run_length_probs[[1]])
}

length(sum_probs[sum_probs != 1])
