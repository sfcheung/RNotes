# To illustrate how to compute scale scores in R

# Generate the data
set.seed(89071434)
n <- 100
k <- 20
x <- runif(n*k,1,5)
RawData <- data.frame(round(matrix(x,n,k)))
colnames(RawData) <- paste("q", 1:k, sep="")
rownames(RawData) <- paste("Case", sprintf("%04d",1:n), sep="")
RawData$age <- round(runif(n,18,40))
RawData$gender <- factor(round(runif(n,0,1)), levels=c(0,1), 
                  labels=c("Male", "Female"))
tmp1 <- sample(1:n,round(n*.25))
tmp2 <- sample(1:k,round(n*.25), replace=TRUE)
for (i in 1:n) {
  for (j in 1:(k+2)) {
      if (runif(1) < .01) RawData[i,j] <- NA
    }
  }

library(psych)
# By positions
personality.keys <- list(
  Extraverion = c(-1,2,5,7,10,-11,13,15,-16,19),
  Openness    = c(3,4,6,-8,9,12,14,-17,18,20),
  Test        = c(1,2,3,4,5),
  Test2       = c(-1,-2,-3,-4,-5)
  )
personality.labels <- paste("q", 1:k, sep="")
keys <- make.keys(20,keys.list=personality.keys, 
                  item.labels=personality.labels)
# By column names
personality.keys2 <- list(
  Extraverion = c("-q1","q2","q5","q7","q10","-q11","q13","q15","-q16","q19"),
  Openness    = c("q3","q4","q6","-q8","q9","q12","q14","-q17","q18","q20"),
  Test        = c("q1","q2","q3","q4","q5"),
  Test2       = c("-q1","-q2","-q3","-q4","-q5")
  )
keys2 <- make.keys(personality.labels,keys.list=personality.keys)

# Scoring
personality.scoreItems <- scoreItems(keys2, RawData[,1:20], digits=4)
RawDataScored <- merge(RawData, personality.scoreItems$scores, by="row.names")
write.csv(RawDataScored,"RawDataScored.csv")