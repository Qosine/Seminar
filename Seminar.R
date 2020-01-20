install.packages()
library(stats)
library(glmnet)
library(noia)
library(plot)
library(grplasso)
setwd("C:/Users/marcs/OneDrive/Bureaublad/Master/Seminar")
getwd()
set.seed(1)
data <- read.csv(file.choose())
CPS <- rbind(c(0.203195,0.10298,0.100214),
             c(0.185959,0.092719,0.09324),
             c(0.186438,0.091954,0.094484),
             c(0.424408,0.195766,0.228643))
#data 
data$male <- ifelse(data$sd_gender == "male", 1,0)
#define target

y1 = data[,22]
y2 = data[,23]
y3 = data[,24]
X1 = data[,25:92]
X2 = data[,93:182]
cv = data[,4:21]
cv2 = cbind(data[,4],data[,183])
y = as.matrix(y1)
X = cbind(as.matrix(X1),as.matrix(cv2[,1:2]))
data$target <- ifelse(data$sd_gender == "female" &  data$sd_age<35, 1,0)
CPS_target <- CPS[1,3]
gender <- "male"
lower <- 25
upper <- 34

source(file = "./logit.R",local=TRUE) 
#logit(y,X)
#weights(data,CPS_target,gender,upper, lower)
#Wlogit(y, X, Weights)
#Lasso_logit(y, X)


#univariate testing
head <- colnames(X1)
j=1
uni_1_1 <- vector()
uni_2_1 <- vector()
uni_3_1 <- vector()
for(i in X1){
  b1logit <- glm(kpi_familiarity ~ i, data= data, family = "binomial")
  if(summary(b1logit)[["coefficients"]][, "Pr(>|z|)"][2]<0.05){
    uni_1_1 <- rbind(uni_1_1,head[j])
  }
  b2logit <- glm(kpi_awareness ~ i, data= data, family = "binomial")
  if(summary(b2logit)[["coefficients"]][, "Pr(>|z|)"][2]<0.05){
    uni_2_1 <- rbind(uni_2_1,head[j])
  }
  b3logit <- glm(kpi_consideration ~ i, data= data, family = "binomial")
  if(summary(b3logit)[["coefficients"]][, "Pr(>|z|)"][2]<0.05){
    uni_3_1 <- rbind(uni_3_1,head[j])
  }
  j=j+1
}
uni_1_1
uni_2_1
uni_3_1




#factor analysis - werkt niet
factanal(X1, 5, rotation = "varimax", lower = 0.03) 







