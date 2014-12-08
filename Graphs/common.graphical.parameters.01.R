# Commonly used graphical parameters 
# Part 1

# Sample data
set.seed(5413790)
caseid <- 1:100
x      <- rnorm(100)
y      <- .60*x + rnorm(100,0,sqrt(1-.60^2))
sample_data <- data.frame(caseid, x, y)

# Histogram
hist(x, 
      main=paste(
           "Title: This is an \"ugly\" graph for illustrating",
           "\nthe effect of each parameter"), 
        cex.main=1, col.main="red", font.main=2,
      sub="This is the subtitle", 
        cex.sub=1, col.sub="green", font.sub=3,
      xlab="x-axis label", 
      ylab="y-axis label", 
        cex.lab=1, col.lab="blue", font.lab=4,
      cex.axis=1, col.axis="orange", font.axis=3, lwd=4, 
      xlim=c(-4,4), ylim=c(0,30),
      fg="blue",
      lty="dotted", col="green", border="red",
      labels=TRUE)

# Scatterplot
plot(x, y,
      main=paste(
           "Title: This is an \"ugly\" graph for illustrating",
           "\nthe effect of each parameter"), 
        cex.main=1, col.main="red", font.main=2,
      sub="This is the subtitle", 
        cex.sub=1, col.sub="green", font.sub=3,
      xlab="x-axis label", 
      ylab="y-axis label", 
        cex.lab=1, col.lab="blue", font.lab=4,
      cex.axis=1, col.axis="orange", font.axis=3, 
      xlim=c(-4,4), ylim=c(-4,4),
      pch=22, lwd=1.5, col="red", bg="green", cex=2,
      fg="blue")