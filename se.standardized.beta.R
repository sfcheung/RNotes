# Examine the (incorrect) standard error of the standardized coefficient

library(MASS)
rho <- .50
n <- 500
nrep <- 1000
sim.rho <- replicate(nrep, (mvrnorm(n,c(0,0),matrix(c(1,rho,rho,1),2,2))), simplify=FALSE)
lm.tmp <- function(data) {
    lm.std      <- summary(lm(scale(data[,1]) ~ scale(data[,2])))
    lm.unstd    <- summary(lm((data[,1]) ~ (data[,2])))
    b.unstd     <- lm.unstd$coefficients[2,1]
    b.unstd.se  <- lm.unstd$coefficients[2,2]
    b.std       <- lm.std$coefficients[2,1]
    b.std.se    <- lm.std$coefficients[2,2]
    return(c(b.unstd=b.unstd,b.unstd.se=b.unstd.se,b.std=b.std,b.std.se=b.std.se))
  }
sim.lm <- sapply(sim.rho, lm.tmp, simplify=TRUE)
tmp1 <- apply(sim.lm, 1, sd)
tmp2 <- apply(sim.lm, 1, mean)
results <- c(b.unstd.sd=tmp1[1],b.unstd.se=tmp2[2],b.std.sd=tmp1[3],b.std.se=tmp2[4])
results
