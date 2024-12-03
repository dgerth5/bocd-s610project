library(ggplot2)
library(reshape2)

# Simulated Data
set.seed(5)
data1 <- c(rnorm(250))
data2 <- c(rnorm(100), rnorm(150, mean = 5, sd = 1))
data3 <- c(rnorm(100), rnorm(150, mean = 1, sd = 1))

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



# stock data
# 
# library(tidyverse)
# library(reshape2)
# library(riingo)
# library(TTR)
# library(zoo)
# 
# RIINGO_TOKEN = "b90d53d93827cce6877317ddcb68cf8cbeeb2399"
# riingo_set_token(RIINGO_TOKEN)
# 
# sd = "2019-01-01"
# ed = Sys.Date()
# 
# spy = riingo_prices("spy", start_date = sd, end_date = ed) %>%
#   dplyr::select(date, adjClose) %>%
#   mutate(lr = c(0,diff(log(adjClose), lag=1)),
#          vol = runSD(lr, n = 20) * sqrt(252)) %>%
#   drop_na(vol)
# 
# plot(spy$vol, type = "l")
# 
# mean(spy$vol)
# sd(spy$vol)
# 
# b2 <- bocd(spy$vol, mu_0 = 0, sigma = 2, lambda = length(spy$vol))
# 
# if (is.null(b2$run_length_probs)) {
#   stop("Error: 'run_length_probs' is NULL")
# }
# 
# 
# max_length2 <- max(sapply(b2$run_length_probs, length))
# 
# matrix_result2 <- do.call(rbind, lapply(b2$run_length_probs, function(vec) {
#   c(vec, rep(0, max_length2 - length(vec)))
# }))
# 
# matrix_transposed2 <- t(matrix_result2)
# df2 <- melt(matrix_transposed2) 
# 
# df3 <- df2 %>%
#   group_by(Var2) %>%
#   summarise(max_var2 = max(value))
# 
# summary(df2)
# 
# # Plot using ggplot2
# ggplot(df2, aes(x = Var2, y = Var1, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient(low = "green", high = "red") +
#   ylim(c(0,500)) +
#   labs(
#     x = "Data",
#     y = "Pun Length",
#     title = "Bayesian Online Changepoint Detection"
#   ) +
#   theme_minimal()
# 
# l <- ocp::onlineCPD(spy$vol, 
#                     hazard_func = function(run_length) 1 / 250,
#                     init_params = list(list(m = 0.17, k = 17, a
#                                             = 83, b = .5)))
# plot(l)
# ?onlineCPD
# 
# print(lengths(b2$run_length_probs))
# print(max_length2)
