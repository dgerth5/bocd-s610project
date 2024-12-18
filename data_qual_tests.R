#### check to make sure data is numeric and has no NA's ####

d1 <- 1:5
d2 <- c(1:5,"d")
d3 <- c(1:5, NA)

b1 <- bocd(d1)
b2 <- bocd(d2) # error as expected
b3 <- bocd(d3) # error as expected
