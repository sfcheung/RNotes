# Plot a bubble plot for discrete ordinal variables 
#   (e.g., Likert scale responses).
# Usual plots for categorical variables (e.g., mosaic plot) are not suitable
#   for likert scales in which the data are usually treated as 
#   discrete numbers, rather than just different categories.

# Generate random data for illustration
set.seed(14897)
x <- round(runif(100,1,5))
y <- round(runif(100,-1,1)) + x
xy.data <- data.frame(x,y)

library(ggplot2)

# Compute the frequencies for all combinations of x and y
x.values <- sort(unique(xy.data$x))
y.values <- sort(unique(xy.data$y))
xbyy <- expand.grid(x.values,y.values)
xbyy.freq <- as.numeric(xtabs(~x+y, data=xy.data))

# Generate the bubble plot based on the frequencies
xy.bubble <- ggplot(data.frame(x=xbyy[,1],y=xbyy[,2], freq=xbyy.freq),aes(x,y))
xy.bubble + geom_point(aes(size=freq)) + scale_size_area(max_size=20)
# max_size is used to set the maximum size of the bubbles.
