#' Explore different ways to update an object

library(pryr)
library(microbenchmark)

#' Case 1

f1 <- function(seed = NULL, nrep = 10, p = 10, printout = FALSE) {
    if (!is.null(seed)) set.seed(seed)
    out <- runif(p) - .5
    if (printout) cat("Before loop: ", address(out), "\n")
    for (i in seq_len(nrep)) {
        out <- out * runif(p) - .5
        if (printout) cat("In loop: ", address(out), "\n")
      }
    if (printout) cat("After loop: ", address(out), "\n")
    out
  }

f1(printout = TRUE)

#' Case 2

f2 <- function(seed = NULL, nrep = 10, p = 10, printout = FALSE) {
    if (!is.null(seed)) set.seed(seed)
    out0 <- runif(p) - .5
    out1 <- out0
    if (printout) cat("Before loop: ", address(out1), "\n")
    for (i in seq_len(nrep)) {
        out1 <- out0 * runif(p) - .5
        out0 <- out1
        if (printout) cat("In loop: ", address(out1), "\n")
      }
    if (printout) cat("After loop: ", address(out1), "\n")
    out1
  }

f2(printout = TRUE)

#' Case 3

f3 <- function(seed = NULL, nrep = 10, p = 10, printout = FALSE) {
    if (!is.null(seed)) set.seed(seed)
    out0 <- runif(p) - .5
    out1 <- out0
    if (printout) cat("Before loop: ", address(out1), "\n")
    for (i in seq_len(nrep)) {
        out1[] <- out0 * runif(p) - .5
        out0[] <- out1
        if (printout) cat("In loop: ", address(out1), "\n")
      }
    if (printout) cat("After loop: ", address(out1), "\n")
    out1
  }

f3(printout = TRUE)

#' Benchmark

b_out <- microbenchmark(f1(seed = 413531, nrep = 100, p = 100), 
                        f2(seed = 413531, nrep = 100, p = 100),
                        f3(seed = 413531, nrep = 100, p = 100),
                        f1(seed = 413531, nrep = 100, p = 500), 
                        f2(seed = 413531, nrep = 100, p = 500),
                        f3(seed = 413531, nrep = 100, p = 500),
                        times = 100)
                        
b_out

library(ggplot2)
autoplot(b_out)

