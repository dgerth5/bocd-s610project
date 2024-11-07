library(ggplot2)
library(reshape2)

# Simulated Data
set.seed(5)
data <- c(rnorm(50, mean = 0, sd = 1), rnorm(200, mean = 5, sd = 1))
b <- bocd(data)

# plot

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
    title = "Bayesian Online Changepoint Detection"
  ) +
  theme_minimal()


# stock data

library(tidyverse)
library(riingo)
library(TTR)
library(zoo)

RIINGO_TOKEN = "b90d53d93827cce6877317ddcb68cf8cbeeb2399"
riingo_set_token(RIINGO_TOKEN)

sd = "2019-01-01"
ed = Sys.Date()

spy = riingo_prices("spy", start_date = sd, end_date = ed) %>%
  dplyr::select(date, adjClose) %>%
  mutate(lr = c(0,diff(log(adjClose), lag=1)),
         vol = runSD(lr, n = 20) * sqrt(252)) %>%
  drop_na(vol)

plot(spy$vol, type = "l")

mean(spy$vol)
sd(spy$vol)

b2 <- bocd(spy$vol, mu_0 = .17, sigma = .11, lambda = length(spy$vol))

if (is.null(b2$run_length_probs)) {
  stop("Error: 'run_length_probs' is NULL")
}


max_length2 <- max(sapply(b2$run_length_probs, length))

matrix_result2 <- do.call(rbind, lapply(b2$run_length_probs, function(vec) {
  c(vec, rep(0, max_length2 - length(vec)))
}))

matrix_transposed2 <- t(matrix_result2)
df2 <- melt(matrix_transposed2)

summary(df2)

# Plot using ggplot2
ggplot(df2, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    x = "Data",
    y = "Pun Length",
    title = "Bayesian Online Changepoint Detection"
  ) +
  theme_minimal()


print(lengths(b2$run_length_probs))
print(max_length2)
