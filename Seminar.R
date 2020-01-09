library(stats)
setwd(".")
data <- read.csv("cleaned_unified_sample.csv")
y1 = data[,22]
y2 = data[,23]
y3 = data[,24]
X1 = data[,25:92]
X2 = data[,93:182]
cv = data[,4:21]
lm <- lm(as.matrix(y1) ~ as.matrix(X1))
blogit <- glm(as.matrix(y1) ~ cbind(as.matrix(X1),as.matrix(cv[,1:2])), data= data, family = "binomial")
blogit$coefficients
