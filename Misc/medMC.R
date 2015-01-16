# Test indirect effect in mediation using Monte Carlo method.
# Shu Fai Cheung
# Last modified: 2015-01-17
# Adapted from the R code in Preacher and Selig (2012).
# Preacher, K. J., & Selig, J. P. (2012). Advantages of 
#   Monte Carlo confidence intervals for indirect effects.
#   Communication Methods and Measures, 6, 77-98.

medMC <- function(a, b, ase, bse, nrep=1000, conf=.95,
                  seed=NULL, printout=TRUE, 
                  sd_iv=NULL, sd_dv=NULL,
                  mednames=NULL, ...) {
  # Input:
  #   a: A vector of one or more sample regression 
  #      coefficient estimates from the independent 
  #      variable to the mediator(s).
  #   b: A vector of one or more sample regression 
  #      coefficient estimates from the mediator(s)
  #      to the dependent variable.
  #   ase: A vector of the standard error(s) of a.
  #   bse: A vector of the standard error(s) of b.
  #   nrep: Number of sets of random numbers to generate.
  #   conf: The proportion of coverage of the confidence
  #         interval.
  #   seed: Random seed. Used to make the simulation reproducible.
  #   printout: If TRUE, the results will be printed.
  #   mednames: Optional. A vector of the names of the mediators.
  #   sd_iv, sd_dv: The standard deviations of the independent
  #                 variable and dependent variable. If
  #                 provided, can compute the completely standardized
  #                 indirect effect.
  #   ...: Optional arguments to be passed to print().
  
  require(MASS)
  
  k <- length(ase)
  
  # Generate the random numbers
  if (!is.null(seed)) set.seed(seed)
  av_s <- if (k > 1) diag(ase^2) else ase^2
  bv_s <- if (k > 1) diag(bse^2) else bse^2
  a_mc <- mvrnorm(nrep, a, av_s)
  b_mc <- mvrnorm(nrep, b, bv_s)
  ab_mc <- a_mc * b_mc
  total_mc <- apply(ab_mc, 1, sum)
  
  # Compute the indirect effect
  
  ab <- a * b
  total <- sum(ab)
  
  # Construct the confidence intervals
  conf_lo <- (1-conf)/2
  conf_hi <- 1-(1-conf)/2
  ab_ci <- t(apply(ab_mc, 2, quantile, 
           probs=c(conf_lo, conf_hi)))
  colnames(ab_ci) <- c(paste((conf*100),"% Lower", sep=""),
                       paste((conf*100),"% Upper", sep=""))
  rownames(ab_ci) <- paste("Specific Indirect Effect", 1:k, 
                           sep=" ")
  if (!is.null(mednames)) rownames(ab_ci) <- mednames
  total_ci <- matrix(quantile(total_mc, probs=c(conf_lo, conf_hi)),
                     1, 2)
  colnames(total_ci) <- colnames(ab_ci)
  rownames(total_ci) <- "Total Indirect Effect"
  
  # Compute the completely standardized indirect effect
  
  if (!is.null(sd_iv) & !is.null(sd_dv)) {
      ab_std <- ab*sd_iv/sd_dv
      total_std <- total*sd_iv/sd_dv
    } else {
      ab_std <- rep(NA, k)
      total_std <- NA
    }
  
  if (printout) {
      tmp <- cbind(c(ab,total),rbind(ab_ci, total_ci),
                   c(ab_std,total_std))
      colnames(tmp) <- c("Estimate", colnames(ab_ci),
                         "Completely Standardized")
      print(tmp, ...)
    }
  results <- list(specific_est=ab, total_est=total,
                  specific_ci=ab_ci, total_ci=total_ci,
                  ab_simualted=ab_mc,
                  specific_std=ab_std, total_std=total_std,
                  total_simulated=total_mc)
  invisible(results)
}

# Examples:
# In real analysis, nrep should be at least 20000
# medMC(a=c(.5, .7), b=c(.4, .6), 
#       ase=c(.1, .2), bse=c(.2, .1), nrep=1000, print=TRUE, digits=4)
# medMC(a=.5, b=.4, ase=.1, bse=.2, nrep=1000, print=TRUE, digits=4)