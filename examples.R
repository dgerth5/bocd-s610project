library(ggplot2)
library(reshape2)

source("bayesian-online-changepoint-script.R") #call in functions from script

#### check plots ####

# Simulated Data
set.seed(135)
data1 <- rnorm(100)
data2 <- c(rnorm(50), rnorm(50, mean = 5, sd = 1))
data3 <- c(rnorm(50), rnorm(50, mean = 2, sd = 1))

plot_function <- function(data, title){
  
  b <- bocd(data,
            mu = 0,
            sigma_0 = 1,
            sigma = 1,
            lambda = 100)
  
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
      y = "Run Length",
      title = title
    ) +
    theme_minimal()

}

plot_function(data1, "BOCD Test 1: No Changepoints")
plot_function(data2, "BOCD Test 2: Distinct Changepoint")
plot_function(data3, "BOCD Test 3: Unclear Changepoint")


#### check to make sure data is numeric and has no NA's ####

d1 <- 1:5
d2 <- c(1:5,"d")
d3 <- c(1:5, NA)

b1 <- bocd(d1)
b2 <- bocd(d2) # error as expected
b3 <- bocd(d3) # error as expected
