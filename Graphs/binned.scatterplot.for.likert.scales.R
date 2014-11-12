# Plot a binned scatterplot for discrete ordinal variables 
#   (e.g., Likert scale responses)

# Generate the random data for illustration
set.seed(431432)
x <- round(runif(100,1,5))
y <- round(runif(100,1,5))
xy.data <- data.frame(x,y)

library(ggplot2)

# Assuming the values are integers, set the scale such that each cell is at 
# the intersection of the values of x and y
xv <- sort(unique(x))
yv <- sort(unique(y))
xv <- c(xv,max(xv)+1) - .5
yv <- c(yv,max(yv)+1) - .5
# The graph
xy.binned <- ggplot(xy.data,aes(x,y))
xy.binned + stat_bin2d(breaks=list(x=xv,y=yv))
