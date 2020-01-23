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
cat_means <- round(cbind(mean(v_audiosum[v_audiosum != 0]), mean(v_digitalsum[v_digitalsum != 0]),
                         mean(v_programsum[v_programsum != 0]), mean(v_tvsum[v_tvsum != 0]), 
                         mean(v_vodsum[v_vodsum != 0]), mean(v_yousum[v_yousum != 0])), 3)

cat_sd <-  round(cbind(sd(v_audiosum[v_audiosum != 0]), sd(v_digitalsum[v_digitalsum != 0]),
                       sd(v_programsum[v_programsum != 0]), sd(v_tvsum[v_tvsum != 0]), 
                       sd(v_vodsum[v_vodsum != 0]), sd(v_yousum[v_yousum != 0])), 3)

car_prop_zero <- round(cbind(length(v_audiosum[v_audiosum != 0])/nrow(data),
                             length(v_digitalsum[v_digitalsum != 0])/nrow(data),
                             length(v_programsum[v_programsum != 0])/nrow(data),
                             length(v_tvsum[v_tvsum != 0])/nrow(data),
                             length(v_vodsum[v_vodsum != 0])/nrow(data),
                             length(v_yousum[v_yousum != 0])/nrow(data)),3)