# Plot a binned scatterplot for Likert scale responses.

x <- round(runif(100,1,5))
y <- round(runif(100,1,5))
xy.data <- data.frame(x,y)

library(ggplot2)
xy.binned <- ggplot(xy.data,aes(x,y))
v <- 1:6 - .5
xy.binned + stat_bin2d(breaks=list(x=v,y=v))
