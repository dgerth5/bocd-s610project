library(testthat)
source("bayesian-online-changepoint-script.R")

test_that("Updated mean should be 0",{
  
  new_data <- 0
  bocd1 <- bocd(new_data)
  
  expect_true(bocd1$pred_means == 0,
              info = "Prior mean is 0, new data point is 0, updated mean should be 0")
  
})

test_that("Probabilities should sum to 1 for each time period for changepoint",{
  
  new_data2 <- rnorm(100)
  bocd_data_2 <- bocd(new_data2)
  
  sum_probs <- numeric(100)
  for (i in 1:100) {
    sum_probs[i] <- sum(bocd_data_2$run_length_probs[[1]])
  }
  
  expect_true(all(sum_probs == 1), 
              info = "Not all elements in sum_probs are equal to 1.")
})
