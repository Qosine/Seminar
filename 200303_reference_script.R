# Fix seed and load libraries
set.seed(200127)
library(mvtnorm); library(simcausal); library(dplyr); library(survey); library(ggplot2); library(robustbase)
library(invgamma)

# Pathing - fix this on your machine first (set to local Git directory)
path = "~/Documents/Econometrie/Masters/Seminar Nielsen"
path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"
setwd(path)
source("./200302_simulation_support_functions.R")

# CPS weights
CPS <- rbind.data.frame(c(0.203195,0.10298,0.100214),
                        c(0.185959,0.092719,0.09324),
                        c(0.186438,0.091954,0.094484),
                        c(0.424408,0.195766,0.228643))
colnames(CPS) = c("Total", "Male", "Female")
rownames(CPS) = c("25-34", "35-44", "45-54", "55-99")



#######################################################################
################## CHOOSE TARGET GROUP AND KPI BELOW ##################

target_gender = "Male" # options are "Male", "Female"
target_age = "25-34" # options are "25-34", "35-44", "45-54", "55-99"
kpi = "Consideration" # options are "Familiarity", "Consideration", "Awareness"

################## CHOOSE TARGET GROUP AND KPI ABOVE ##################
#######################################################################



target_min_age = as.numeric(substring(target_age, 1, 2))
target_max_age = as.numeric(substring(target_age, nchar(target_age)-1, nchar(target_age)))

# Load Pointlogic source data
source_data = read.csv("./cleaned_unified_sample.csv")

# Separate true data into predictors and responses, and split target and non-target data
true_fullsample_variables = separate_predictors_responses(source_data)
subsamples = split_sample(source_data, tolower(target_gender), target_min_age, target_max_age)
true_target_variables = separate_predictors_responses(subsamples$target)

true_target_params = c(-1.5, -3, 3.2, -1.0, 0.5, 1.0, .75)
true_nontarget_params = c(-2.0, 0.6, 0.4, 0.2, 0.1, 0.5, -1)
true_nontarget_params_exclAudio = true_nontarget_params[-2]

# true_target_params = c(-1.5, 3.2, -1.0, 0.5, 1.0, .75)
# true_nontarget_params = c(-2.0, 0.4, 0.2, 0.1, 0.5, 1)

real_nontargets = list()
real_nontargets$predictors = add_constant(separate_predictors_responses(subsamples$nontarget)$predictors)
real_nontargets$Audio = real_nontargets$predictors[,2]
real_nontargets$kpi = generate_response(real_nontargets$predictors,
                                        true_nontarget_params,
                                        nrow(real_nontargets$predictors))

X_invgamma = real_nontargets$predictors[,1]
for (i in 2:ncol(real_nontargets$predictors)) {
  X_invgamma = cbind(X_invgamma, rinvgamma(nrow(real_nontargets$predictors), shape=2, scale=1))
  to_set_to_zero = sample(1:nrow(real_nontargets$predictors),
                          (sum(real_nontargets$predictors[,i]==0)),
                          replace=FALSE)
  X_invgamma[to_set_to_zero,i] = 0
}
y_invgamma = generate_response(X_invgamma, true_nontarget_params, nrow(X_invgamma))

# real_nontargets$kpi_exclAudio = generate_response(real_nontargets$predictors_exclAudio,
#                                                   true_nontarget_params_exclAudio,
#                                                   nrow(real_nontargets$predictors_exclAudio))

run_simulation <- function(N,Q,reps) {
  p = length(true_target_params)
  mvn_results = data.frame(matrix(0, reps, p))
  mvn_wAudio_results = data.frame(matrix(0, reps, p))
  real_data_results = data.frame(matrix(0, reps, p))
  invgamma_results = data.frame(matrix(0, reps, p))
  
  
  for (i in 1:reps) {
    X_mvn = add_constant(rmvnorm(N*(1-Q),
                                 mean = colMeans(real_nontargets$predictors[,2:p]),
                                 sigma = cor(real_nontargets$predictors[,2:p])))
    y_mvn = generate_response(X_mvn, true_nontarget_params, nrow(X_mvn))
    glm.mvn <- glm(y_mvn ~ 0 + X_mvn, family = binomial(link="logit"))
    mvn_results[i,] <- glm.mvn$coefficients
    
    idx_rs_nontargets = sample( 1:nrow(subsamples$nontarget), (N*(1-Q)) )
    rs_nontargets = list()
    rs_nontargets$predictors = real_nontargets$predictors[idx_rs_nontargets,]
    rs_nontargets$kpi = real_nontargets$kpi[idx_rs_nontargets]
    glm.realData <- glm(rs_nontargets$kpi ~ 0 + rs_nontargets$predictors, family = binomial(link="logit"))
    real_data_results[i,] <- glm.realData$coefficients
    
    rs_invgamma = list()
    rs_invgamma$predictors = X_invgamma[idx_rs_nontargets,]
    rs_invgamma$kpi = y_invgamma[idx_rs_nontargets]
    glm.invgamma <- glm(rs_invgamma$kpi ~ 0 + rs_invgamma$predictors, family = binomial(link="logit"))
    invgamma_results[i,] = glm.invgamma$coefficients
    
    # X_mvn_wAudio = cbind(X_mvn[,1], real_nontargets$Audio[idx_rs_nontargets], X_mvn[,3:ncol(X_mvn)])
    # y_mvn_wAudio = generate_response(X_mvn_wAudio, true_nontarget_params, nrow(X_mvn))
    # glm.mvn_wAudio.nontarget <- glm(y_mvn_wAudio ~ 0 + X_mvn_wAudio, family = binomial(link="logit"))
    # mvn_wAudio_results[i,] <- glm.mvn_wAudio.nontarget$coefficients
    
    # rs_nontargets$predictors_exclAudio = real_nontargets$predictors_exclAudio[idx_rs_nontargets,]
    # rs_nontargets$kpi_exclAudio = real_nontargets$kpi_exclAudio[idx_rs_nontargets]
    
  }
  return(list(mvn_results = mvn_results,
              invgamma_results = invgamma_results,
              real_data_results = real_data_results))
}

Q10 = run_simulation(N=5000, Q=.1, reps=500)
Q30 = run_simulation(N=5000, Q=.3, reps=1000)
Q50 = run_simulation(N=5000, Q=.5, reps=1000)
Q70 = run_simulation(N=5000, Q=.7, reps=1000)
Q90 = run_simulation(N=5000, Q=.9, reps=1000)
