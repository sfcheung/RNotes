# Examine the (incorrect) standard error of the standardized coefficient

library(MASS)
rho <- .50
n <- 200
nrep <- 1000
sim.rho <- replicate(nrep, (mvrnorm(n,c(0,0),matrix(c(1,rho,rho,1),2,2))), simplify=FALSE)
lm.tmp <- function(data) {
    lm.std       <- summary(lm(scale(data[,1]) ~ scale(data[,2])))
    lm.unstd     <- summary(lm((data[,1]) ~ (data[,2])))
    b.unstd      <- lm.unstd$coefficients[2,1]
    b.unstd.var  <- lm.unstd$coefficients[2,2]^2
    b.std        <- lm.std$coefficients[2,1]
    b.std.var    <- lm.std$coefficients[2,2]^2
    r.var        <- ((1-b.std^2)^2)/(n-1)
    return(c(b.unstd=b.unstd,b.unstd.var=b.unstd.var,b.std=b.std,b.std.var=b.std.var,r.var=r.var))
  }
sim.lm <- sapply(sim.rho, lm.tmp, simplify=TRUE)
tmp1 <- apply(sim.lm, 1, sd)
tmp2 <- apply(sim.lm, 1, mean)
results <- c(tmp1[1],sqrt(tmp2[2]),tmp1[3],sqrt(tmp2[4]),sqrt(tmp2[5]))
names(results) <- c("b.ustd.sd","b.ustd.se","b.std.sd","b.std.se","r.se")
cat("Check whether the SD of estimates and the mean of SE agree\n")
round(results,4)
cat("NOTE: Actually, the sqrt of the mean of sampling variance is reported.\n")
