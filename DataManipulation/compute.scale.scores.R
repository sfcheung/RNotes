# To illustrate how to compute scale scores in R

# Generate the data
set.seed(89071434)
n <- 100
k <- 20
x <- runif(n*k,1,5)
RawData <- matrix(x,n,k)
