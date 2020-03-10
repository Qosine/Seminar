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

true_target_params = c(-1.5, -1.5, 3.2, -1.0, 0.5, 1.0, .75)
true_nontarget_params = c(-2.0, 0.6, 0.4, 0.2, 0.1, 0.5, -1)
#true_nontarget_params_exclAudio = true_nontarget_params[-2]

# Load Pointlogic source data
source_data = read.csv("./cleaned_unified_sample.csv")
# Separate true data into predictors and responses, and split target and non-target data
true_fullsample_variables = separate_predictors_responses(source_data)
subsamples = split_sample(source_data, tolower(target_gender), target_min_age, target_max_age)

load("./simulated_target_predictors.RData")
simulated_targets = simulated_nontargets = list()
simulated_targets$predictors = add_constant(simulated_target_predictors)
simulated_targets$kpi = generate_response(simulated_targets$predictors,
                                          true_target_params,
                                          nrow(simulated_targets$predictors))

load("./simulated_nontarget_predictors.RData")
simulated_nontargets$predictors = add_constant(simulated_nontarget_population)
simulated_nontargets$kpi = generate_response(simulated_nontargets$predictors,
                                             true_nontarget_params,
                                             nrow(simulated_nontargets$predictors))





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
  
  print(paste("Q:", Q, "N:", N*(1-Q)))
  
  p = length(true_target_params)
  
  uncorr_mvn_results = data.frame(matrix(0, reps, p))
  corr_mvn_results = data.frame(matrix(0, reps, p))
  mvn_wAudio_results = data.frame(matrix(0, reps, p))
  invgamma_results = data.frame(matrix(0, reps, p))
  
  real_target_results = data.frame(matrix(0, reps, p))
  real_nontarget_results = data.frame(matrix(0, reps, p))
  
  sim_target_results = data.frame(matrix(0, reps, p))
  sim_nontarget_results =data.frame(matrix(0, reps, p))
  
  mu_0 = colMeans(real_nontargets$predictors[,2:p])
  sigma_0 = cor(real_nontargets$predictors[,2:p])
  
  for (i in 1:reps) {
    
    if (Q==.9) {
      corr_X_mvn = add_constant(rmvnorm(750, mean = mu_0, sigma = sigma_0))
      uncorr_X_mvn = add_constant(rmvnorm(750, mean = rep(0, (p-1)), sigma = diag(p-1)))
    } else {
      corr_X_mvn = add_constant(rmvnorm(N*(1-Q), mean = mu_0, sigma = sigma_0))
      uncorr_X_mvn = add_constant(rmvnorm(N*(1-Q), mean = rep(0, (p-1)), sigma = diag(p-1)))
    }
    
    corr_y_mvn = generate_response(corr_X_mvn, true_nontarget_params, nrow(corr_X_mvn))
    glm.corr_mvn <- glm(corr_y_mvn ~ 0 + corr_X_mvn, family = binomial(link="logit"))
    corr_mvn_results[i,] <- glm.corr_mvn$coefficients
    
    uncorr_y_mvn = generate_response(uncorr_X_mvn, true_nontarget_params, nrow(uncorr_X_mvn))
    glm.uncorr_mvn <- glm(uncorr_y_mvn ~ 0 + uncorr_X_mvn, family = binomial(link="logit"))
    uncorr_mvn_results[i,] <- glm.uncorr_mvn$coefficients
    
    idx_rs_sim_targets = sample( 1:nrow(simulated_targets$predictors), (N*Q) )
    rs_sim_targets = list()
    rs_sim_targets$predictors = simulated_targets$predictors[idx_rs_sim_targets,]
    rs_sim_targets$kpi = simulated_targets$kpi[idx_rs_sim_targets]
    glm.realTargets <- glm(rs_sim_targets$kpi ~ 0 + rs_sim_targets$predictors, family = binomial(link="logit"))
    real_target_results[i,] = glm.realTargets$coefficients
    
    
    idx_rs_real_nontargets = sample( 1:nrow(subsamples$nontarget), (N*(1-Q)) )
    rs_real_nontargets = list()
    rs_real_nontargets$predictors = real_nontargets$predictors[idx_rs_real_nontargets,]
    rs_real_nontargets$kpi = real_nontargets$kpi[idx_rs_real_nontargets]
    glm.realNontargets <- glm(rs_real_nontargets$kpi ~ 0 + rs_real_nontargets$predictors, family = binomial(link="logit"))
    real_nontarget_results[i,] <- glm.realNontargets$coefficients
    rs_sim_nontargets = list()
    rs_sim_nontargets$predictors = simulated_nontargets$predictors[idx_rs_real_nontargets,]
    rs_sim_nontargets$kpi = simulated_nontargets$kpi[idx_rs_real_nontargets]
    glm.simNontargets <- glm(rs_sim_nontargets$kpi ~ 0 + rs_sim_nontargets$predictors, family = binomial(link="logit"))
    sim_nontarget_results[i,] <- glm.simNontargets$coefficients
    
    rs_invgamma = list()
    rs_invgamma$predictors = X_invgamma[idx_rs_real_nontargets,]
    rs_invgamma$kpi = y_invgamma[idx_rs_real_nontargets]
    glm.invgamma <- glm(rs_invgamma$kpi ~ 0 + rs_invgamma$predictors, family = binomial(link="logit"))
    invgamma_results[i,] = glm.invgamma$coefficients
    
    # X_mvn_wAudio = cbind(X_mvn[,1], real_nontargets$Audio[idx_rs_real_nontargets], X_mvn[,3:ncol(X_mvn)])
    # y_mvn_wAudio = generate_response(X_mvn_wAudio, true_nontarget_params, nrow(X_mvn))
    # glm.mvn_wAudio.nontarget <- glm(y_mvn_wAudio ~ 0 + X_mvn_wAudio, family = binomial(link="logit"))
    # mvn_wAudio_results[i,] <- glm.mvn_wAudio.nontarget$coefficients
    
    # rs_real_nontargets$predictors_exclAudio = real_nontargets$predictors_exclAudio[idx_rs_real_nontargets,]
    # rs_real_nontargets$kpi_exclAudio = real_nontargets$kpi_exclAudio[idx_rs_real_nontargets]
    
    if (i%%100==0) {print(paste("Iteration:", i))}
    
    
  }
  return(list(uncorr_mvn_results = uncorr_mvn_results,
              corr_mvn_results = corr_mvn_results,
              invgamma_results = invgamma_results,
              real_target_results = real_target_results,
              real_nontarget_results = real_nontarget_results,
              sim_nontarget_results = sim_nontarget_results))
}

# Q10 = run_simulation(N=7500, Q=.1, reps=1000)
# Q50 = run_simulation(N=7500, Q=.5, reps=1000)
# Q90 = run_simulation(N=7500, Q=.9, reps=1000)

pctBias <- function(beta_true, beta_hat) {abs((colMeans(beta_hat) - beta_true)/beta_true)}
euclidean_length <-  function(x) sqrt(sum(x^2))


target_proportions = c(10, 50, 90)  #5*(2:18)[c(TRUE,FALSE)]
for (prop in target_proportions) {
  assign(paste("Q", prop, sep=""), c())
  assign(paste("Q", prop, sep=""), run_simulation(N = 7500, Q = prop/100, reps = 1000))
}

df_rows = paste("Q", target_proportions, sep = "")
df_cols = c("Intercept", "Audio", "Digital", "Program", "TV", "VOD", "Youtube")
uncorr_mvn_results_df = data.frame(matrix(0, length(df_rows), length(df_cols)))
rownames(uncorr_mvn_results_df) = df_rows; colnames(uncorr_mvn_results_df) = df_cols
invgamma_results_df = resampling_results_df = pointlogic_data_results_df = corr_mvn_results_df = uncorr_mvn_results_df

for (prop in target_proportions) {
  varname = paste("Q", prop, sep="")
  estimates = get(varname)
  uncorr_mvn_bias = pctBias(true_nontarget_params, as.matrix(estimates$uncorr_mvn_results))
  corr_mvn_bias = pctBias(true_nontarget_params, as.matrix(estimates$corr_mvn_results))
  invgamma_bias = pctBias(true_nontarget_params, as.matrix(estimates$invgamma_results))
  pointlogic_data_bias = pctBias(true_nontarget_params, as.matrix(estimates$real_nontarget_results))
  resampling_bias = pctBias(true_nontarget_params, as.matrix(estimates$sim_nontarget_results))
  
  uncorr_mvn_results_df[varname,] = uncorr_mvn_bias
  corr_mvn_results_df[varname,] = corr_mvn_bias
  invgamma_results_df[varname,] = invgamma_bias
  resampling_results_df[varname,] = resampling_bias
  pointlogic_data_results_df[varname,] = pointlogic_data_bias
}


round(100*uncorr_mvn_results_df, 2)
round(100*corr_mvn_results_df,2)
round(100*invgamma_results_df,2)
round(100*resampling_results_df,2)
round(100*pointlogic_data_results_df,2)


round(100*mvn_bias,1)
round(100*invgamma_bias,1)
round(100*pointlogic_data_bias,1)
round(100*resampling_bias,1)

# Summary bias measure
euclidean_length(mvn_bias)
euclidean_length(invgamma_bias)
euclidean_length(real_nontargets_bias)
euclidean_length(sim_nontargets_bias)

# Summary variance measure
for (results in Q10) {
  bias = euclidean_length(100*((colMeans(results)-true_nontarget_params)/true_nontarget_params))
  variance = det(var(sqrt(2500)*results))
  print(cat("Bias: ", bias,
            "\nVariance: ", variance))
}
