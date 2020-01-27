install.packages("dplyr")
library(dplyr)

setwd("C:/Users/marti/Documents/Uni/Ectrie/Seminar")
getwd()
set.seed(1)
data <- read.csv("./cleaned_unified_sample.csv", sep = ";")

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


# Calculate mean and sd of the nonzero values. Besides, the proportion of zeroes is calculated.
v_cat_means <- round(cbind(mean(v_audiosum[v_audiosum != 0]), mean(v_digitalsum[v_digitalsum != 0]),
                         mean(v_programsum[v_programsum != 0]), mean(v_tvsum[v_tvsum != 0]), 
                         mean(v_vodsum[v_vodsum != 0]), mean(v_yousum[v_yousum != 0])), 3)

v_cat_sd <-  round(cbind(sd(v_audiosum[v_audiosum != 0]), sd(v_digitalsum[v_digitalsum != 0]),
                       sd(v_programsum[v_programsum != 0]), sd(v_tvsum[v_tvsum != 0]), 
                       sd(v_vodsum[v_vodsum != 0]), sd(v_yousum[v_yousum != 0])), 3)

v_car_prop_zero <- round(cbind(length(v_audiosum[v_audiosum != 0])/nrow(data),
                             length(v_digitalsum[v_digitalsum != 0])/nrow(data),
                             length(v_programsum[v_programsum != 0])/nrow(data),
                             length(v_tvsum[v_tvsum != 0])/nrow(data),
                             length(v_vodsum[v_vodsum != 0])/nrow(data),
                             length(v_yousum[v_yousum != 0])/nrow(data)),3)

#Create age groups
data$age_group <- NA
for (i in 1:nrow(data)){
  if (data$sd_age[i] >= 25 && data$sd_age[i] <= 34) {
    data$age_group[i] <- "25-34"
  } else if (data$sd_age[i] >= 35 && data$sd_age[i] <= 44) {
    data$age_group[i] <- "35-44"
  } else if (data$sd_age[i] >= 45 && data$sd_age[i] <= 54) {
    data$age_group[i] <- "45-54"
  } else {data$age_group[i] <- "55+"}
}

# First, pivot tables that inclue the number of 1's and 0's across genders and age groups.
# Then, the number of 0's is divided by the number of people in total in the age group 
# (both male and female)
t_pivot_familiarity <-  table(data$age_group, data$sd_gender, data$kpi_familiarity)
v_prop_zero_fami <- as.vector(round(t_pivot_familiarity[c(1:8)]/(t_pivot_familiarity[c(1:8)] + t_pivot_familiarity[c(9:16)]), 3))

t_pivot_consideration <-  table(data$age_group, data$sd_gender, data$kpi_consideration)
v_prop_zero_consi <- as.vector(round(t_pivot_consideration[c(1:8)]/(t_pivot_consideration[c(1:8)] + t_pivot_consideration[c(9:16)]), 3))

t_pivot_awareness <-  table(data$age_group, data$sd_gender, data$kpi_awareness)
v_prop_zero_aware <- as.vector(round(t_pivot_awareness[c(1:8)]/(t_pivot_awareness[c(1:8)] + t_pivot_awareness[c(9:16)]), 3))

# Bind vector and write to a CSV file. 
m_total <- cbind(v_prop_zero_aware, v_prop_zero_consi, v_prop_zero_fami)
write.csv2(m_total, file = "pivot_gender.csv")
