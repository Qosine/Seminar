install.packages("survey")
library(stats)
library(glmnet)
library(noia)
library(plot)
library(grplasso)
library(dplyr)
library(stringr)
library(matlib)
library(survey)
library(lubridate)
setwd("C:/Users/marcs/OneDrive/Bureaublad/Master/Seminar")
getwd()
set.seed(1)
data <- read.csv("./cleaned_unified_sample.csv")

#CPS data columns are total/male/female and rows: 25-24/35-44/45-55/55+
CPS <- rbind(c(0.203195,0.10298,0.100214),
             c(0.185959,0.092719,0.09324),
             c(0.186438,0.091954,0.094484),
             c(0.424408,0.195766,0.228643))


#define target audience
d_CPS_target <- CPS[1,3]
str_gender <- "male"
i_lower <- 25
i_upper <- 34


#get function files
source(file = "./AwarenessGraph.R",local=TRUE) 
source(file = "./logit.R",local=TRUE)
#logit(y,X)
#weights(data,CPS_target,gender,upper, lower)
#Wlogit(y, X, Weights)
#Lasso_logit(y, X)


#define kpi's, mediaconsumptions, contactvar and control variables
v_kpi_familiarity = data[,22]
v_kpi_awareness = data[,23]
v_kpi_consideration = data[,24]
df_mediacons = data[,25:92]
df_contacts = data[,93:182]
df_controlvar = data[,4:21]


#X variables/regressors
v_audiosum = rowSums(df_contacts[,1:5])
v_digitalsum = rowSums(df_contacts[,6:12])
v_programsum = df_contacts[,13]
v_tvsum = rowSums(df_contacts[,14:81])
v_vodsum = rowSums(df_contacts[,82:89])
v_yousum = df_contacts[,90]
v_weeks <- week(data$id_date)
v_logweeks <- log(v_weeks)

#control variables
v_male <- (ifelse(data$sd_gender == "male", 1,0))
v_havechildren <- (ifelse(data$sd_havechildren == "yes", 1,0))
v_age3544 <- (ifelse(data$sd_age <=44 & data$sd_age >=35, 1,0))
v_age4554 <- (ifelse(data$sd_age <=54 & data$sd_age >=45, 1,0))
v_age55plus <- (ifelse(data$sd_age >=55, 1,0))
v_employed <- (ifelse(data$sd_employment == "employed", 1,0))
v_income030 <- (ifelse(data$sd_householdincome == "[0,30)", 1,0))
v_income3050 <- (ifelse(data$sd_householdincome == "[30,50)", 1,0))
v_income5075 <- (ifelse(data$sd_householdincome == "[50,75)", 1,0))
v_income75100 <- (ifelse(data$sd_householdincome == "[75,100)", 1,0))
v_income100150 <- (ifelse(data$sd_householdincome == "[100,150)", 1,0))
v_educ2 <- (ifelse(data$sd_education == "secondary", 1,0))
v_educ3 <- (ifelse(data$sd_education == "tertiary", 1,0))
v_etn_cauc <- (ifelse(data$sd_ethnicity_caucasian == "yes", 1,0))
v_etn_afric <- (ifelse(data$sd_ethnicity_africanamerican == "yes", 1,0))
v_etn_hisp <- (ifelse(data$sd_ethnicity_hispanic == "yes", 1,0))
v_etn_asian <- (ifelse(data$sd_ethnicity_asian == "yes", 1,0))
v_etn_native <- (ifelse(data$sd_ethnicity_nativeamerican == "yes", 1,0))
v_etn_other <- (ifelse(data$sd_ethnicity_other == "yes", 1,0))
v_married <- (ifelse(data$sd_maritalstatus == "married", 1,0))
v_single <- (ifelse(data$sd_maritalstatus == "single", 1,0))
v_seperated <- (ifelse(data$sd_maritalstatus == "seperated", 1,0))
v_strat <- ifelse(data$sd_age <=34 & data$sd_age >=25 & data$sd_gender == "male", 1,0)

#Define your X and y, vstrat is necessary for surveylogit(is filtered out in function)
df_X <- cbind(v_audiosum,v_digitalsum,v_programsum,v_tvsum,v_vodsum,v_yousum, v_logweeks, v_male, 
               v_age3544, v_age4554, v_age55plus, v_havechildren, v_etn_cauc,
               v_income030, v_income3050,v_income5075, v_income75100,v_income100150, v_educ2, v_educ3,v_strat)
v_y = v_kpi_awareness




#functions from logit file
logit_res <- logit(v_y,df_X)
Weights_res <- weights(data,d_CPS_target,str_gender,i_upper, i_lower)
wlogit_res <- Wlogit(v_y, df_X, dum_Weights)
lasso_res <- Lasso_logit(v_y, df_X)
survey_strat_logit(df_X, v_y, Weights_res)

