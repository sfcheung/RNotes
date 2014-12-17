gen_beta_custom <- function(
  N,
  BMean,
  BSD,
  Low = 0,
  High = 1
) {

  #N: Number of random numbers to generate
  #BMean: Population mean of the beta distribution
  #BSD: Population SD of the beta distribution
  #  If BSD <=0, all random numbers generated is equal to BMean
  #Low: Minimum of the beta distribution
  #High: Maximum of the beta distribution

  if (BSD > 0) {
    Range <- High - Low
    Mean1 <- (BMean - Low)/Range
    Var1 <- (BSD^2)/(Range^2)
    D <- (1-Mean1)/Mean1
    Alpha <- Mean1*(Mean1*(1-Mean1)/Var1 - 1)
    Beta <- Alpha * D
    Results <- rbeta(N,Alpha,Beta)*Range + Low
  }
  
  if (BSD <= 0) {
    Results <- rep(BMean, N)
  }
  
  return(invisible(Results))
}

#Example
#tmp <- gen_beta_custom(1000,.80,.10,.00,.95)
