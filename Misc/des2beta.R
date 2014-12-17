des2beta <- function(
  vmean,
  vsd,
  lo = 0,
  hi = 1
) {

  # Compute the parameters of a beta distribution 
  # with the same mean and SD of a variable with 
  # a finite range.
  # Return the parameters, and the skewness and kurtosis
  # of this beta distribution.

  #vmean:  Sample mean
  #vsd:    Sample standard deviation
  #low:   Minimum possible value of the variable
  #hi:    Maximum possible value of the variable

  vrange <- hi - lo 
  bmean <- (vmean - lo)/vrange
  bv <- (vsd^2)/(vrange^2)
  d <- (1-bmean)/bmean
  balpha <- bmean*(bmean*(1-bmean)/bv - 1)
  bbeta <- balpha * d
  
  bskew <- 2*(bbeta - balpha)*sqrt(bbeta + balpha + 1)
  bskew <- bskew/((bbeta + balpha + 2)*sqrt(bbeta*balpha))
  a1 <- ((balpha - bbeta)^2)*(balpha + bbeta + 1)
  a2 <- bbeta*balpha*(bbeta + balpha + 2)
  b1 <- balpha*bbeta*(balpha + bbeta + 2)*(balpha + bbeta + 3)
  bkurtosis <- 6*(a1 - a2)/b1

  if (balpha <= 0 || bbeta <= 0) {
      balpha <- bbeta <- bmean <- bv <- bskew <- bkurtosis <- NA
    }
  results <- list(alpha=balpha, beta=bbeta, mean=bmean, sd=sqrt(bv), 
                  skewness=bskew, kurtosis=bkurtosis)
  results
}

#Example
#vmean <- 3; vsd <- 1; lo <- 1; hi <- 5;
#tmp <- des2beta(vmean=vmean,vsd=vsd,lo=lo,hi=hi); tmp
#tmp.data <- rbeta(5000,tmp$alpha,tmp$beta)*(hi-lo) + lo
#mean(tmp.data)
#sd(tmp.data)
#hist(tmp.data)