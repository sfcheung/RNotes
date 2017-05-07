#' Explore how a list is updated in a loop.

library(pryr)

#' Case 1

f1 <- function(nrep = 10, p = 20, printout = FALSE) {
    out <- vector("list", nrep)
    if (printout) cat("Before the loop: ", address(out), "\n")
    for (i in seq_len(nrep)) {
        out[[i]] <- seq_len(p)
        if (printout) cat("In the loop: ", address(out), "\n")
      }
    if (printout) cat("After the loop: ", address(out), "\n")
    out
  }
  
f1_out <- f1(printout = TRUE)

#' Case 2

f2 <- function(nrep = 10, p = 20, printout = FALSE) {
    out <- list()
    if (printout) cat("Before the loop: ", address(out), "\n")
    for (i in seq_len(nrep)) {
        out[[i]] <- seq_len(p)
        if (printout) cat("In the loop: ", address(out), "\n")
      }
    if (printout) cat("After the loop: ", address(out), "\n")
    out
  }
  
f2_out <- f2(printout = TRUE)

identical(f1_out, f2_out)

#' Benchmark

library(microbenchmark)
library(ggplot2)

b_out_short <- microbenchmark(f1(nrep = 50, p = 50), 
                              f2(nrep = 50, p = 50), 
                              times = 100)
b_out_long <- microbenchmark(f1(nrep = 500, p = 50), 
                             f2(nrep = 500, p = 50), 
                             times = 100)

b_out_short
b_out_long
                             
autoplot(b_out_short)
autoplot(b_out_long)