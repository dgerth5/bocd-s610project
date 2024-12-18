library(testthat)
source("bayesian-online-changepoint-script.R")

test_that("No errors",{
  d1 <- 1:5
  b1 <- bocd(d1)
  
  expect_no_error(b1, message = "Data is numeric, no error expected")
  
})

test_that("Should throw non-numeric error",{
  d2 <- c(1:5,"d")
  
  expect_error(bocd(d2),
               regexp = "Non-numeric argument to mathematical function",
               info = "Last element is not numeric, expect error")
               
})

test_that("Should throw NA error",{
  d3 <- c(1:5, NA)
  b3 <- bocd(d3)
  expect_true(b3 == "Remove NA's",
              info = "NA is data passed through")
  
})

