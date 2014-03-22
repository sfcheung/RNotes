# Generate a bubble plot for two Likert scales.

x <- round(runif(100,1,3))
y <- round(runif(100,-1,1)) + x
xy.data <- data.frame(x,y)

library(ggplot2)
x.values <- sort(unique(xy.data$x))
y.values <- sort(unique(xy.data$y))
xbyy <- expand.grid(x.values,y.values)
xbyy.freq <- as.numeric(xtabs(~x+y, data=xy.data))
xy.bubble <- ggplot(data.frame(x=xbyy[,1],y=xbyy[,2], freq=xbyy.freq),aes(x,y))
xy.bubble + geom_point(aes(size=freq)) + scale_area(range=c(0,20))
