# Fix seed and load libraries
set.seed(123456)
install.packages("survey")
install.packages("readr")

library(mvtnorm); library(dplyr); library(survey); library(ggplot2); library(robustbase); library(xtable); library(Rfast); library(reshape2); library(ggpubr)
# Pathing - fix this on your machine first (set to local Git directory)
path = "C:/Users/marti/Documents/GitHub/Seminar"
# path = "~/Documents/Econometrie/Masters/Seminar Nielsen"
#path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"
setwd(path)
source("./200302_simulation_support_functions.R")


#######################################################################
################## DO NOT CHANGE THE FOLLOWING ########################
#######################################################################

# NOTE: Target group/KPI choices are irrelevant anyway
#       We manually choose parameters of interest and generate own KPIs

CPS <- rbind.data.frame(c(0.203195,0.10298,0.100214),
                        c(0.185959,0.092719,0.09324),
                        c(0.186438,0.091954,0.094484),
                        c(0.424408,0.195766,0.228643))
colnames(CPS) = c("Total", "Male", "Female")
rownames(CPS) = c("25-34", "35-44", "45-54", "55-99")
target_gender = "Male"
target_age = "25-34"
kpi = "Consideration"




#######################################################################
####### LOAD POINTLOGIC SOURCE DATA AND DO SOME PRE-PROCESSING ########
#######################################################################

# Separate target age group for future reference
target_min_age = as.numeric(substring(target_age, 1, 2))
target_max_age = as.numeric(substring(target_age, nchar(target_age)-1, nchar(target_age)))

# Load Pointlogic source data
source_data = read.csv("./cleaned_unified_sample.csv")

# Separate true data into predictors and responses, and split target and non-target data
#true_fullsample_variables = separate_predictors_responses(source_data)
subsamples = split_sample(source_data, tolower(target_gender), target_min_age, target_max_age)
true_target_variables = separate_predictors_responses(subsamples$target)
true_nontarget_variables = separate_predictors_responses(subsamples$nontarget)





#######################################################################
############# CONSTRUCT DATASETS USED DURING SIMULATION ###############
#######################################################################

data_wo_Dem = data_w_Dem = list()
data_w_Dem = add_constant(separate_predictors_responses(source_data)$predictors)
data_wo_Dem = data_w_Dem[,1:7]
data_digiprog_summed = cbind(data_wo_Dem[,1:2],
                             (data_wo_Dem[,3] + data_wo_Dem[,4]),
                             data_wo_Dem[,5:7])

data_sqrt = data_wo_Dem^.5

#######################################################################
####### CHOOSE TRUE PARAMETERS FOR TARGET AND NON-TARGET GROUPS #######
#######################################################################

# NOTE: If you want to use target and non-target parameters as ...
#       originally derived from the Nielsen data, set boolean ...
#       use_Nielsen_parameters below to TRUE

ncol_to_add = dim(data_w_Dem)[2] - dim(data_wo_Dem)[2]

target_params_wo_Dem = c(-2.0, 0.6, 1.2, -0.5, 0.7, 0.8, 0.4)
target_params_insig = c(-2.0, 0.6, 1.2, -0.5, 0.7, 0.8, 0.05)
demographics_params = c(.31, .96, .71, -.66, .99, -.32, 1.44, 1.04,
                        1.86, 1.34, -1.36, -0.93, -1.22, -1.92, 0.75, -1.27,
                        -1.41, 0.37, -1.98, -0.56, 1.48)
target_params_w_Dem = append(target_params_wo_Dem, demographics_params)
target_params_insig_w_Dem = append(target_params_insig, demographics_params)

nontarget_params_wo_Dem = c(-1.5, -3, 3.2, -1.0, 0.5, 1.0, 0.6)
nontarget_params_w_Dem = append(nontarget_params_wo_Dem, demographics_params)

# Compute population-level parameters as linear combination of target and non-target params
true_population_params_sig = (CPS[target_age, target_gender]*target_params_w_Dem
                              + (1-CPS[target_age, target_gender])*nontarget_params_w_Dem)
true_population_params_insig = (CPS[target_age, target_gender]*target_params_insig_w_Dem
                                + (1-CPS[target_age, target_gender])*nontarget_params_w_Dem)


mean(generate_response(data_w_Dem, target_params_w_Dem, nrow(data_w_Dem)));

mean(generate_response(data_w_Dem, nontarget_params_w_Dem, nrow(data_w_Dem)))

mean(generate_response(data_w_Dem, target_params_insig_w_Dem, nrow(data_w_Dem)))

#if (TRUE) {

##### Significant parameters
# for (N in c(2000,3000,4000,5000)) {
#   print(paste("N:", N))
#   for (Q in 5*(8:18)) {
#     print(paste("Q:", Q))
#     assign(paste("significant_N", N, "_Q", Q, sep=""),
#            fit_total_audience_models(data_w_Dem, length(target_params_wo_Dem),
#                                      target_params_w_Dem, nontarget_params_w_Dem, true_population_params_sig,
#                                      sample_size_total = N, sample_size_target = (N*Q/100),
#                                      n_bootstraps = 1000))
#   }
# }
#save.image("D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar/200312_overnight_simulation_significant.RData")

##### Insignificant parameters
#   for (N in c(5000)) {
#     print(paste("N:", N))
#     for (Q in 5*(8:18)) {
#       print(paste("Q:", Q))
#       assign(paste("insignificant_N", N, "_Q", Q, sep=""),
#              fit_total_audience_models(data_w_Dem, length(target_params_wo_Dem),
#                                        target_params_insig_w_Dem, nontarget_params_w_Dem, true_population_params_insig,
#                                        sample_size_total = N, sample_size_target = (N*Q/100),
#                                        n_bootstraps = 1000))
#     }
#   }
#   save.image("D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar/200312_overnight_simulation_insignificant.RData")
# }

# Toggle map
setwd("C:/Users/marti/Documents/GitHub/Seminar/P40")
#load("Reps2000_2000_3000.RData")
load("200314_N2000_5000_P40.RData")

#tables:
# for P10
# c <- c("N2000_Q40_P10","N2000_Q45_P10","N2000_Q50_P10","N2000_Q55_P10","N2000_Q60_P10","N2000_Q65_P10","N2000_Q70_P10","N2000_Q75_P10","N2000_Q80_P10","N2000_Q85_P10","N2000_Q90_P10",
#        "N3000_Q40_P10","N3000_Q45_P10","N3000_Q50_P10","N3000_Q55_P10","N3000_Q60_P10","N3000_Q65_P10","N3000_Q70_P10","N3000_Q75_P10","N3000_Q80_P10","N3000_Q85_P10","N3000_Q90_P10",
#        "N5000_Q40_P10","N5000_Q45_P10","N5000_Q50_P10","N5000_Q55_P10","N5000_Q60_P10","N5000_Q65_P10","N5000_Q70_P10","N5000_Q75_P10","N5000_Q80_P10","N5000_Q85_P10","N5000_Q90_P10")
# for P20
# c <- c("N2000_Q40_P20","N2000_Q45_P20","N2000_Q50_P20","N2000_Q55_P20","N2000_Q60_P20","N2000_Q65_P20","N2000_Q70_P20","N2000_Q75_P20","N2000_Q80_P20","N2000_Q85_P20","N2000_Q90_P20",
#        "N3000_Q40_P20","N3000_Q45_P20","N3000_Q50_P20","N3000_Q55_P20","N3000_Q60_P20","N3000_Q65_P20","N3000_Q70_P20","N3000_Q75_P20","N3000_Q80_P20","N3000_Q85_P20","N3000_Q90_P20",
#        "N5000_Q40_P20","N5000_Q45_P20","N5000_Q50_P20","N5000_Q55_P20","N5000_Q60_P20","N5000_Q65_P20","N5000_Q70_P20","N5000_Q75_P20","N5000_Q80_P20","N5000_Q85_P20","N5000_Q90_P20")
# for P40
c <- c("N2000_Q40_P40","N2000_Q45_P40","N2000_Q50_P40","N2000_Q55_P40","N2000_Q60_P40","N2000_Q65_P40","N2000_Q70_P40","N2000_Q75_P40","N2000_Q80_P40","N2000_Q85_P40","N2000_Q90_P40",
       "N3000_Q40_P40","N3000_Q45_P40","N3000_Q50_P40","N3000_Q55_P40","N3000_Q60_P40","N3000_Q65_P40","N3000_Q70_P40","N3000_Q75_P40","N3000_Q80_P40","N3000_Q85_P40","N3000_Q90_P40",
       "N5000_Q40_P40","N5000_Q45_P40","N5000_Q50_P40","N5000_Q55_P40","N5000_Q60_P40","N5000_Q65_P40","N5000_Q70_P40","N5000_Q75_P40","N5000_Q80_P40","N5000_Q85_P40","N5000_Q90_P40")

# For P20 and P40
true_population_params <- true_population_params_sig

#for p10
# true_population_params <- (CPS[ "25-34","Male"]*target_params_w_Dem
#                           + (1-CPS["25-34","Male"])*nontarget_params_w_Dem)


cols <- c("/mu_E PB", "/mu SB",		"max_E PB", "Max_E PB",	"/mu_D pB",	"/mu_D SB",
          "/mu_E PB", "/mu SB",		"max_E PB", "Max_E PB",	"/mu_D pB",	"/mu_D SB")
svyglm_res <- matrix(0,nrow = length(c), ncol=28)
rownames(svyglm_res) <- c
total_w_interaction_res <- matrix(0,nrow = length(c), ncol=28)
rownames(total_w_interaction_res) <- c
target_w_interaction_res <- matrix(0,nrow = length(c), ncol=28)
rownames(target_w_interaction_res) <- c
svy_glm_dif <- matrix(0,nrow = length(c), ncol=28)
rownames(svy_glm_dif) <- c
LRT_res <- vector()
prediction_result <- matrix(0, nrow = length(c), ncol = 4)

est_int_total <- list()
est_int_svy <- list()
est_int_target <- list()
j =1

for(i in c){
  estimates <- get(i)
  estimates$glm.interaction_total_audience[abs(estimates$glm.interaction_total_audience)>100]=NA
  estimates$svyglm.total_audience[abs(estimates$svyglm.total_audience)>100]=NA
  estimates$glm.interaction_target_audience[abs(estimates$glm.interaction_target_audience)>100]=NA
  svyglm_res[i,] <- abs((colMeans(estimates$svyglm.total_audience, na.rm = TRUE)-true_population_params)/true_population_params)
  total_w_interaction_res[i,] <- abs((colMeans(estimates$glm.interaction_total_audience, na.rm = TRUE)-true_population_params)/true_population_params)
  target_w_interaction_res[i,] <- abs((colMeans(estimates$glm.interaction_target_audience, na.rm = TRUE)-target_params_w_Dem)/target_params_w_Dem)
  
  est_int_total[[j]] <- (estimates$glm.interaction_total_audience)
  est_int_svy[[j]] <- (estimates$svyglm.total_audience)
  est_int_target[[j]] <- (estimates$glm.interaction_target_audience)
  LRT_res[j] <- sum(estimates$LRT.interaction_model)
  prediction_result[j,] <- round(100*(colMeans(cbind(estimates$hitrate.svyglm,estimates$hitrate.interaction,estimates$bayesrate,estimates$alwayszero))),2)
  j=j+1
}

#prediction 
colnames(prediction_result) <- c("Weighted model", "Interaction model", "Bayesrate", "Always zero")
rownames(prediction_result) <-  substr(c,nchar(c)-8,nchar(c))
print(xtable(prediction_result, type = "latex"), file = "Prediction_table_P40.tex")


Q <- c(8:18)*0.05

glm_standardized_bias <- vector()
glm_standardized_bias_ind_max <-vector()
glm_standardized_bias_ind_mean <-vector()
glm_standardized_bias_ind_mean_D <- vector()

intera_standardized_bias <- vector()
intera_standardized_bias_ind_max <-vector()
intera_standardized_bias_ind_mean <-vector()
intera_standardized_bias_ind_mean_D <- vector()

intera_target_standardized_bias <- vector()
intera_target_standardized_bias_ind_max <-vector()
intera_target_standardized_bias_ind_mean <-vector()
intera_target_standardized_bias_ind_mean_D <- vector()

for( j in 1:length(c)){
  
  glm_standardized_bias[j] <-  100*sqrt(t(colMeans(est_int_svy[[j]], na.rm=T) - true_population_params) %*% solve(var(est_int_svy[[j]], na.rm=TRUE)) %*% (colMeans(est_int_svy[[j]],na.rm=T)-true_population_params))
  glm_standardized_bias_ind_mean[j] <-   mean(abs(100*(colMeans(est_int_svy[[j]][,1:7],na.rm=T)-true_population_params[1:7])/diag(var(est_int_svy[[j]][,1:7],na.rm=T))))
  glm_standardized_bias_ind_max[j] <-     max(abs(100*(colMeans(est_int_svy[[j]][,1:7],na.rm=T)-true_population_params[1:7])/diag(var(est_int_svy[[j]][,1:7],na.rm=T))))
  glm_standardized_bias_ind_mean_D[j] <- mean(abs(100*(colMeans(est_int_svy[[j]][,15:28],na.rm=T)-true_population_params[15:28])/diag(var(est_int_svy[[j]][,15:28],na.rm=T))))
  
  intera_standardized_bias[j] <-  100*sqrt(t(colMeans(est_int_total[[j]], na.rm=T) - true_population_params) %*% solve(var(est_int_total[[j]], na.rm=TRUE)) %*% (colMeans(est_int_total[[j]],na.rm=T)-true_population_params))
  intera_standardized_bias_ind_mean[j] <-   mean(abs(100*(colMeans(est_int_total[[j]][,1:7],na.rm=T)-true_population_params[1:7])/diag(var(est_int_total[[j]][,1:7],na.rm=T))))
  intera_standardized_bias_ind_max[j] <-     max(abs(100*(colMeans(est_int_total[[j]][,1:7],na.rm=T)-true_population_params[1:7])/diag(var(est_int_total[[j]][,1:7],na.rm=T))))
  intera_standardized_bias_ind_mean_D[j] <-mean(abs(100*(colMeans(est_int_total[[j]][,15:28],na.rm=T)-true_population_params[15:28])/diag(var(est_int_total[[j]][,15:28],na.rm=T))))
  
  intera_target_standardized_bias[j] <-  100*sqrt(t(colMeans(est_int_target[[j]], na.rm=T) - target_params_w_Dem) %*% solve(var(est_int_target[[j]], na.rm=TRUE)) %*% (colMeans(est_int_target[[j]],na.rm=T)-target_params_w_Dem))
  intera_target_standardized_bias_ind_mean[j] <-   mean(abs(100*(colMeans(est_int_target[[j]][,1:7],na.rm=T)-target_params_w_Dem[1:7])/diag(var(est_int_target[[j]][,1:7],na.rm=T))))
  intera_target_standardized_bias_ind_max[j] <-     max(abs(100*(colMeans(est_int_target[[j]][,1:7],na.rm=T)-target_params_w_Dem[1:7])/diag(var(est_int_target[[j]][,1:7],na.rm=T))))
  intera_target_standardized_bias_ind_mean_D[j] <-mean(abs(100*(colMeans(est_int_target[[j]][,15:28],na.rm=T)-target_params_w_Dem[15:28])/diag(var(est_int_target[[j]][,15:28],na.rm=T))))
  
}
# Standardised Biases
SB_result_mat <- matrix(0, nrow=11, ncol=9)
rownames(SB_result_mat) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
SB_result_mat[,1] <- glm_standardized_bias[1:11]
SB_result_mat[,2] <- glm_standardized_bias[12:22]
SB_result_mat[,3] <- glm_standardized_bias[23:33]

SB_result_mat[,4] <- intera_target_standardized_bias[1:11]
SB_result_mat[,5] <- intera_target_standardized_bias[12:22]
SB_result_mat[,6] <- intera_target_standardized_bias[23:33]

SB_result_mat[,7] <- intera_standardized_bias[1:11]
SB_result_mat[,8] <- intera_standardized_bias[12:22]
SB_result_mat[,9] <- intera_standardized_bias[23:33]


# N = 2000
N2000_result_mat <- matrix(0, nrow=11, ncol=12)
rownames(N2000_result_mat) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N2000_result_mat) = cols
# SVYGLM results
N2000_result_mat[,1] <- round(rowMeans(100*svyglm_res[1:11,1:7]),2)
N2000_result_mat[,2] <- round(glm_standardized_bias_ind_mean[1:11], 2)
N2000_result_mat[,3] <- round(rowMaxs(100*svyglm_res[1:11,1:7], value = TRUE),2)
N2000_result_mat[,4] <- colnames(data_w_Dem)[rowMaxs(100*svyglm_res[1:11,1:7], value = FALSE)]
N2000_result_mat[,5] <- round(rowMeans(100*svyglm_res[1:11,15:28]),2)
N2000_result_mat[,6] <- round(glm_standardized_bias_ind_mean_D[1:11],2)
# Interaction total results
N2000_result_mat[,7] <- round(rowMeans(100*total_w_interaction_res[1:11,1:7]),2)
N2000_result_mat[,8] <- round(intera_standardized_bias_ind_mean[1:11], 2)
N2000_result_mat[,9] <- round(rowMaxs(100*total_w_interaction_res[1:11,1:7], value = TRUE),2)
N2000_result_mat[,10] <- colnames(data_w_Dem)[rowMaxs(100*total_w_interaction_res[1:11,1:7], value = FALSE)]
N2000_result_mat[,11] <- round(rowMeans(100*total_w_interaction_res[1:11,15:28]),2)
N2000_result_mat[,12] <- round(intera_standardized_bias_ind_mean_D[1:11],2)



# N = 3000
N3000_result_mat <- matrix(0, nrow=11, ncol=12)
rownames(N3000_result_mat) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N3000_result_mat) = cols
# SVYGLM results
N3000_result_mat[,1] <- round(rowMeans(100*svyglm_res[12:22,1:7]),2)
N3000_result_mat[,2] <- round(glm_standardized_bias_ind_mean[12:22], 2)
N3000_result_mat[,3] <- round(rowMaxs(100*svyglm_res[12:22,1:7], value = TRUE),2)
N3000_result_mat[,4] <- colnames(data_w_Dem)[rowMaxs(100*svyglm_res[12:22,1:7], value = FALSE)]
N3000_result_mat[,5] <- round(rowMeans(100*svyglm_res[12:22,15:28]),2)
N3000_result_mat[,6] <- round(glm_standardized_bias_ind_mean_D[12:22],2)
# Interaction total results
N3000_result_mat[,7] <- round(rowMeans(100*total_w_interaction_res[12:22,1:7]),2)
N3000_result_mat[,8] <- round(intera_standardized_bias_ind_mean[12:22], 2)
N3000_result_mat[,9] <- round(rowMaxs(100*total_w_interaction_res[12:22,1:7], value = TRUE),2)
N3000_result_mat[,10] <- colnames(data_w_Dem)[rowMaxs(100*total_w_interaction_res[12:22,1:7], value = FALSE)]
N3000_result_mat[,11] <- round(rowMeans(100*total_w_interaction_res[12:22,15:28]),2)
N3000_result_mat[,12] <- round(intera_standardized_bias_ind_mean_D[12:22],2)



# N = 5000
N5000_result_mat <- matrix(0, nrow=11, ncol=12)
rownames(N5000_result_mat) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N5000_result_mat) = cols
# SVYGLM results
N5000_result_mat[,1] <- round(rowMeans(100*svyglm_res[23:33,1:7]),2)
N5000_result_mat[,2] <- round(glm_standardized_bias_ind_mean[23:33], 2)
N5000_result_mat[,3] <- round(rowMaxs(100*svyglm_res[23:33,1:7], value = TRUE),2)
N5000_result_mat[,4] <- colnames(data_w_Dem)[rowMaxs(100*svyglm_res[23:33,1:7], value = FALSE)]
N5000_result_mat[,5] <- round(rowMeans(100*svyglm_res[23:33,15:28]),2)
N5000_result_mat[,6] <- round(glm_standardized_bias_ind_mean_D[23:33],2)
# Interaction total results
N5000_result_mat[,7] <- round(rowMeans(100*total_w_interaction_res[23:33,1:7]),2)
N5000_result_mat[,8] <- round(intera_standardized_bias_ind_mean[23:33], 2)
N5000_result_mat[,9] <- round(rowMaxs(100*total_w_interaction_res[23:33,1:7], value = TRUE),2)
N5000_result_mat[,10] <- colnames(data_w_Dem)[rowMaxs(100*total_w_interaction_res[23:33,1:7], value = FALSE)]
N5000_result_mat[,11] <- round(rowMeans(100*total_w_interaction_res[23:33,15:28]),2)
N5000_result_mat[,12] <- round(intera_standardized_bias_ind_mean_D[23:33],2)



# Interaction Target glm

# N = 2500
N2000_result_mat_target <- matrix(0, nrow=11, ncol=6)
rownames(N2000_result_mat_target) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N2000_result_mat_target) = cols[1:6]
N2000_result_mat_target[,1] <- round(rowMeans(100*target_w_interaction_res[1:11,1:7]),2)
N2000_result_mat_target[,2] <- round(intera_target_standardized_bias_ind_mean[1:11], 2)
N2000_result_mat_target[,3] <- round(rowMaxs(100*target_w_interaction_res[1:11,1:7], value = TRUE),2)
N2000_result_mat_target[,4] <- colnames(data_w_Dem)[rowMaxs(100*target_w_interaction_res[1:11,1:7], value = FALSE)]
N2000_result_mat_target[,5] <- round(rowMeans(100*target_w_interaction_res[1:11,15:28]),2)
N2000_result_mat_target[,6] <- round(intera_target_standardized_bias_ind_mean_D[1:11],2)

# N = 3000
N3000_result_mat_target <- matrix(0, nrow=11, ncol=6)
rownames(N3000_result_mat_target) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N3000_result_mat_target) = cols[1:6]
N3000_result_mat_target[,1] <- round(rowMeans(100*target_w_interaction_res[12:22,1:7]),2)
N3000_result_mat_target[,2] <- round(intera_target_standardized_bias_ind_mean[12:22], 2)
N3000_result_mat_target[,3] <- round(rowMaxs(100*target_w_interaction_res[12:22,1:7], value = TRUE),2)
N3000_result_mat_target[,4] <- colnames(data_w_Dem)[rowMaxs(100*target_w_interaction_res[12:22,1:7], value = FALSE)]
N3000_result_mat_target[,5] <- round(rowMeans(100*target_w_interaction_res[12:22,15:28]),2)
N3000_result_mat_target[,6] <- round(intera_target_standardized_bias_ind_mean_D[12:22],2)


# N = 5000
N5000_result_mat_target <- matrix(0, nrow=11, ncol=6)
rownames(N5000_result_mat_target) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N5000_result_mat_target) = cols[1:6]
N5000_result_mat_target[,1] <- round(rowMeans(100*target_w_interaction_res[23:33,1:7]),2)
N5000_result_mat_target[,2] <- round(intera_target_standardized_bias_ind_mean[23:33], 2)
N5000_result_mat_target[,3] <- round(rowMaxs(100*target_w_interaction_res[23:33,1:7], value = TRUE),2)
N5000_result_mat_target[,4] <- colnames(data_w_Dem)[rowMaxs(100*target_w_interaction_res[23:33,1:7], value = FALSE)]
N5000_result_mat_target[,5] <- round(rowMeans(100*target_w_interaction_res[23:33,15:28]),2)
N5000_result_mat_target[,6] <- round(intera_target_standardized_bias_ind_mean_D[23:33],2)


# Standardised Biases
print(xtable(SB_result_mat, type = "latex"), file = "SB_results_P40.tex")

# SVYGLM & Interaction Total
print(xtable(N2000_result_mat, type = "latex"), file = "N2000_results_P40.tex")
print(xtable(N3000_result_mat, type = "latex"), file = "N3000_results_P40.tex")
print(xtable(N5000_result_mat, type = "latex"), file = "N5000_results_P40.tex")

# Interaction Target
print(xtable(N2000_result_mat_target, type = "latex"), file = "N2000_target_results_P40.tex")
print(xtable(N3000_result_mat_target, type = "latex"), file = "N3000_target_results_P40.tex")
print(xtable(N5000_result_mat_target, type = "latex"), file = "N5000_target_results_P40.tex")




#Grahps for interaction and svyglm percentage bias
N_obs <- c("2000","3000","5000")
save_name_IA <- c("MaxMean_IA_2000_P40.png","MaxMean_IA_3000_P40.png","MaxMean_IA_5000_P40.png")
save_name_svy <- c("MaxMean_svy_2000_P40.png","MaxMean_svy_3000_P40.png","MaxMean_svy_5000_P40.png")
svy_grahps <- list()
IA_graphs <- list()
for(i in 1:3){
  IA_res <- cbind(Q,cbind(rowMeans(100*round(total_w_interaction_res[(1+(i-1)*11):(i*11),1:7],2)),
                          rowMaxs(100*round(total_w_interaction_res[(1+(i-1)*11):(i*11),1:7],3), value=TRUE),
                          rowMeans(100*round(target_w_interaction_res[(1+(i-1)*11):(i*11),1:7],2)),
                          rowMaxs(100*round(target_w_interaction_res[(1+(i-1)*11):(i*11),1:7],3), value=TRUE)))
  colnames(IA_res) <- c("Q", "Total_mean", "Total_max","Target_mean", "Target_max")
  
  svy_res <- cbind(Q,cbind(rowMeans(100*round(svyglm_res[(1+(i-1)*11):(i*11),1:7],2)),
                           rowMaxs(100*round(svyglm_res[(1+(i-1)*11):(i*11),1:7],3), value=TRUE),
                           rowMeans(100*round(target_w_interaction_res[(1+(i-1)*11):(i*11),1:7],2)),
                           rowMaxs(100*round(target_w_interaction_res[(1+(i-1)*11):(i*11),1:7],3), value=TRUE)))
  colnames(svy_res) <- c("Q", "Svyglm_Mean", "Svyglm_Max","Target_Mean", "Target_Max")
  
  temp_IA <- melt(IA_res[1:11,-1])
  temp_IA$Q <- rep(Q,4)
  colnames(temp_IA) <- c("Var1","Legend", "Bias","Q")
  
  temp_svy <- melt(svy_res[1:11,-1])
  temp_svy$Q <- rep(Q,2)
  colnames(temp_svy) <- c("Var1","Legend", "Bias","Q")
  
  
  
  
  # ggplot for interaction
  IA_graphs[[i]] <- ggplot(temp_IA, aes(x=Q, y= Bias, colour = Legend))+
    geom_point()+
    geom_line() +
    theme_light() +
    ylim(0,50) +
    coord_fixed(ratio = 1) +
    theme(panel.grid.major.y = element_line( size=.1, color="grey" ),
          axis.line = element_line(colour = "black"),
          panel.grid = element_blank(), legend.position = "none",
          axis.line.x.top = element_blank(),panel.border = element_blank(), axis.line.y.right = element_blank(),
          aspect.ratio=1)
  ggsave(save_name_IA[i])
  
  # ggplot for svyglm
  svy_grahps[[i]] <- ggplot(temp_svy, aes(x=Q, y= Bias, colour = Legend))+
    geom_point()+
    geom_line() +
    theme_light() +
    ylim(0,50) +
    coord_fixed(ratio = 1) +
    theme(panel.grid.major.y = element_line( size=.1, color="grey" ) ,
          axis.line = element_line(colour = "black"),
          panel.grid = element_blank(),legend.position = "none",
          axis.line.x.top = element_blank(),panel.border = element_blank(), axis.line.y.right = element_blank(),
          aspect.ratio=1)
  ggsave(save_name_svy[i])
}

# Mean and max bias graphs
svy_grahps[[1]]
ggsave("svy_graph_2000_P40.png", width = 5, height = 5)
svy_grahps[[2]]
ggsave("svy_graph_3000_P40.png", width = 5, height = 5)
svy_grahps[[3]]
ggsave("svy_graph_5000_P40.png",width = 5, height = 5)
IA_graphs[[1]]
ggsave("IA_graph_2000_P40.png",width = 5, height = 5)
IA_graphs[[2]]
ggsave("IA_graph_3000_P40.png",width = 5, height = 5)
IA_graphs[[3]]
ggsave("IA_graph_5000_P40.png",width = 5, height = 5)




#standardized bias graph
N_obs <- c("2000","3000","5000")
save_name_IA <- c("MaxMean_IA_2000_P40.png","MaxMean_IA_3000_P40.png","MaxMean_IA_5000_P40.png")
save_name_svy <- c("MaxMean_svy_2000_P40.png","MaxMean_svy_3000_P40.png","MaxMean_svy_5000_P40.png")
stdbias_graphs <- list()

for(i in 1:4){
  stdbias_res <- cbind(Q,cbind(glm_standardized_bias[(1+(i-1)*11):(i*11)],
                               (intera_standardized_bias[(1+(i-1)*11):(i*11)]),
                               (intera_target_standardized_bias[(1+(i-1)*11):(i*11)])))
  colnames(stdbias_res) <- c("Q", "Survey", "Total interaction","Target interaction")
  
  temp_IA <- melt(stdbias_res[1:11,-1])
  temp_IA$Q <- rep(Q,3)
  colnames(temp_IA) <- c("Var1","Legend", "Bias","Q")
  
  # ggplot for standardized bias
  stdbias_graphs[[i]] <- ggplot(temp_IA, aes(x=Q, y= Bias, colour = Legend))+
    geom_point()+
    geom_line() +
    theme_light() +
    ylim(0,150) +
    theme(panel.grid.major.y = element_line( size=.1, color="grey" ) ,
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          panel.grid = element_blank(),
          axis.line.x.top = element_blank(),panel.border = element_blank(), axis.line.y.right = element_blank())
  
  
}

# ggarrange(stdbias_graphs[[1]], stdbias_graphs[[2]],stdbias_graphs[[3]], stdbias_graphs[[3]], common.legend = TRUE, legend="bottom")
# ggsave("stdbias_graph_P40.png")

stdbias_graphs[[1]]
ggsave("stdbias_graphs_2000_P40.png",width = 5, height = 5)
stdbias_graphs[[2]]
ggsave("stdbias_graphs_3000_P40.png",width = 5, height = 5)
stdbias_graphs[[3]]
ggsave("stdbias_graphs_5000_P40.png",width = 5, height = 5)


#LRT result
LRT_res_fin <- 100*round(cbind(LRT_res[1:11],LRT_res[12:22],LRT_res[23:33])/1000,3)
rownames(LRT_res_fin) <-  c(8:18)*0.05
colnames(LRT_res_fin) <- c("2000","3000","5000")
print(xtable(LRT_res_fin, type = "latex"), file = "LRT_results_P40.tex")

#Wald result

#Hitrates
#Hitrates
