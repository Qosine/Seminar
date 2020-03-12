# Fix seed and load libraries
set.seed(123456)
library(mvtnorm); library(dplyr); library(survey); library(ggplot2); library(robustbase)

# Pathing - fix this on your machine first (set to local Git directory)
# path = "~/Documents/Econometrie/Masters/Seminar Nielsen"
path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"
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

target_params_wo_Dem = c(-2.0, 0.6, 1.2, -0.5, 0.7, 0.8,0.4)
demographics_params = c(.31, .96, .71, -.66, .99, -.32, 1.44, 1.04,
                        1.86, 1.34, -1.36, -0.93, -1.22, -1.92, 0.75, -1.27,
                        -1.41, 0.37, -1.98, -0.56, 1.48)
target_params_w_Dem = append(target_params_wo_Dem, demographics_params)

nontarget_params_wo_Dem = c(-1.5, -3, 3.2, -1.0, 0.5, 1.0, 0.6)
nontarget_params_w_Dem = append(nontarget_params_wo_Dem, demographics_params)

# Compute population-level parameters as linear combination of target and non-target params
true_population_params = (CPS[target_age, target_gender]*target_params_w_Dem
                          + (1-CPS[target_age, target_gender])*nontarget_params_w_Dem)


mean(generate_response(data_wo_Dem, target_params_wo_Dem, nrow(data_wo_Dem)))
mean(generate_response(data_w_Dem, target_params_w_Dem, nrow(data_w_Dem)))
mean(generate_response(data_digiprog_summed, target_params_wo_Dem[1:6], nrow(data_digiprog_summed)))
data_sqrt = data_wo_Dem^.5
mean(generate_response(data_sqrt, target_params_wo_Dem, nrow(data_sqrt)))

#######################################################################
#######################################################################



fit_target_audience_models <- function(X_wo_demographics, X_w_demographics, X_digiprog_summed,
                              beta_target_wo_demographics, beta_target_w_demographics,
                              beta_target_digiprog_summed, beta_nontarget_w_demographics,
                              sample_size, n_bootstraps) {
  
  start_time = Sys.time()
  
  assertthat::assert_that(nrow(X_wo_demographics) == nrow(X_w_demographics))
  N = nrow(X_wo_demographics)
  
  glm.target.no_demographics = matrix(0, n_bootstraps, length(beta_target_wo_demographics))
  glm.target.sqrt = matrix(0, n_bootstraps, length(beta_target_wo_demographics))
  glm.target.sum_Digiprog = matrix(0, n_bootstraps, length(beta_target_digiprog_summed))
  glm.target.demographics = matrix(0, n_bootstraps, length(beta_target_w_demographics))
  
  LRT.target.no_demographics = rep(0, n_bootstraps)
  LRT.target.sqrt = rep(0, n_bootstraps)
  LRT.target.sum_Digiprog = rep(0, n_bootstraps)
  LRT.target.demographics = rep(0, n_bootstraps)
  
  for (i in 1:n_bootstraps) {
    
    idx_target = sample(1:nrow(X_wo_demographics), sample_size, replace=T)
    idx_nontarget = sample(setdiff(1:nrow(X_wo_demographics), idx_target), sample_size, replace=T)
    
    # Compile target data
    target_wo_Dem = target_w_Dem = target_sqrt = target_sum_Digiprog = list()
    target_wo_Dem$predictors = X_wo_demographics[idx_target,]
    target_w_Dem$predictors = X_w_demographics[idx_target,]
    target_sqrt$predictors = (X_wo_demographics^(1/2))[idx_target,]
    target_sum_Digiprog$predictors = X_digiprog_summed[idx_target,]
    
    target_wo_Dem$kpi = generate_response(target_wo_Dem$predictors,
                                          beta_target_wo_demographics,
                                          length(idx_target))
    target_w_Dem$kpi = generate_response(target_w_Dem$predictors,
                                         beta_target_w_demographics,
                                         length(idx_target))
    target_sqrt$kpi = generate_response(target_sqrt$predictors,
                                        beta_target_wo_demographics,
                                        length(idx_target))
    target_sum_Digiprog$kpi = generate_response(target_sum_Digiprog$predictors,
                                                beta_target_digiprog_summed,
                                                length(idx_target))

    # KPI generation failsafe - avoid rare events setting
    min_mean_kpi = min(mean(target_wo_Dem$kpi),
                       mean(target_w_Dem$kpi),
                       mean(target_sqrt$kpi),
                       mean(target_sum_Digiprog$kpi))
    max_mean_kpi = max(mean(target_wo_Dem$kpi),
                       mean(target_w_Dem$kpi),
                       mean(target_sqrt$kpi),
                       mean(target_sum_Digiprog$kpi))
    if (min_mean_kpi < 0.1 | max_mean_kpi > 0.9) {
      stop(cat("Parameter choice creates a rare event scenario:\n",
               "Min kpi is ", 100*round(min_mean_kpi,3), "% ",
               "Max kpi is ", 100*round(max_mean_kpi,3), "%\n",
               "Breaking program, please try a less extreme parameter setting", sep=""))
    }
    
    # Model without demographics and without any adjustments to data
    glm.target.no_demographics.H1 = glm(target_wo_Dem$kpi ~ 0 + target_wo_Dem$predictors,
                                        family = binomial(link = "logit"))
    glm.target.no_demographics.H0 <- glm(target_wo_Dem$kpi ~
                                           0 + offset(target_wo_Dem$predictors%*%beta_target_wo_demographics),
                                         family = binomial(link = "logit"))
    glm.target.no_demographics.LRT = 2*abs(logLik(glm.target.no_demographics.H1) - logLik(glm.target.no_demographics.H0))
    glm.target.no_demographics.df = length(beta_target_wo_demographics)
    
    
    # Model with demographics
    glm.target.demographics.H1 = glm(target_w_Dem$kpi ~ 0 + target_w_Dem$predictors,
                                     family = binomial(link = "logit"))
    glm.target.demographics.H0 <- glm(target_w_Dem$kpi ~
                                        0 + offset(target_w_Dem$predictors%*%beta_target_w_demographics),
                                      family = binomial(link = "logit"))
    glm.target.demographics.LRT = 2*abs(logLik(glm.target.demographics.H1) - logLik(glm.target.demographics.H0))
    glm.target.demographics.df = length(beta_target_w_demographics)
    
    # Model with square root of predictors
    glm.target.sqrt.H1 = glm(target_sqrt$kpi ~ 0 + target_sqrt$predictors,
                             family = binomial(link = "logit"))
    glm.target.sqrt.H0 <- glm(target_sqrt$kpi ~
                                0 + offset(target_sqrt$predictors%*%beta_target_wo_demographics),
                              family = binomial(link = "logit"))
    glm.target.sqrt.LRT = 2*abs(logLik(glm.target.sqrt.H1) - logLik(glm.target.sqrt.H0))
    glm.target.sqrt.df = length(beta_target_wo_demographics)
    
    # Model with digital and program summed
    glm.target.sum_Digiprog.H1 = glm(target_sum_Digiprog$kpi ~ 0 + target_sum_Digiprog$predictors,
                                     family = binomial(link = "logit"))
    glm.target.sum_Digiprog.H0 <- glm(target_sum_Digiprog$kpi ~
                                        0 + offset(target_sum_Digiprog$predictors%*%beta_target_digiprog_summed),
                                      family = binomial(link = "logit"))
    glm.target.sum_Digiprog.LRT = 2*abs(logLik(glm.target.sum_Digiprog.H1) - logLik(glm.target.sum_Digiprog.H0))
    glm.target.sum_Digiprog.df = length(beta_target_digiprog_summed)
    
    
    # Store results
    glm.target.no_demographics[i,] = glm.target.no_demographics.H1$coefficients
    glm.target.demographics[i,] = glm.target.demographics.H1$coefficients
    glm.target.sqrt[i,] = glm.target.sqrt.H1$coefficients
    glm.target.sum_Digiprog[i,] = glm.target.sum_Digiprog.H1$coefficients
    LRT.target.no_demographics[i] = ( glm.target.no_demographics.LRT > qchisq(0.95, glm.target.no_demographics.df) )
    LRT.target.demographics[i] = ( glm.target.demographics.LRT > qchisq(0.95, glm.target.demographics.df) )
    LRT.target.sqrt[i] = ( glm.target.sqrt.LRT > qchisq(0.95, glm.target.sqrt.df) )
    LRT.target.sum_Digiprog[i] = ( glm.target.sum_Digiprog.LRT > qchisq(0.95, glm.target.sum_Digiprog.df) )
    
    # Track progress
    if (i%%250 == 0) {
      current_time = Sys.time()
      elapsed = current_time - start_time
      print(paste("Currently at iteration:", i, "| Time elapsed:", round(elapsed, 2), attr(elapsed, "units")))
    }
    
  }
  
  out = list()
  out$glm.target.no_demographics = glm.target.no_demographics
  out$glm.target.demographics = glm.target.demographics
  out$glm.target.sqrt = glm.target.sqrt
  out$glm.target.sum_Digiprog = glm.target.sum_Digiprog
  out$LRT.target.no_demographics = LRT.target.no_demographics
  out$LRT.target.demographics = LRT.target.demographics
  out$LRT.target.sqrt = LRT.target.sqrt
  out$LRT.target.sum_Digiprog = LRT.target.sum_Digiprog
  return(out)
}

# obs_sizes = 100*(5:25)#[c(TRUE,FALSE)]
# for (obs in obs_sizes) {
#   print(paste("Observations:", obs))
#   assign(paste("obs", obs, sep=""), c())
#   assign(paste("obs", obs, sep=""), run_bootstrap(data_wo_Dem, data_w_Dem, data_digiprog_summed,
#                                                    target_params_wo_Dem, target_params_w_Dem,
#                                                    target_params_wo_Dem[1:6], nontarget_params_wo_Dem,
#                                                    sample_size = obs, n_bootstraps = 1000))
# }
# 
# df_rows = paste("obs", obs_sizes, sep = "")
# df_cols = c("Intercept", "Audio", "Digital", "Program", "TV", "VOD", "Youtube")
# glm_target_wo_Dem_df = data.frame(matrix(0, length(df_rows), length(target_params_wo_Dem)))
# glm_target_w_Dem_df = data.frame(matrix(0, length(df_rows), length(target_params_w_Dem)))
# rownames(glm_target_wo_Dem_df) = df_rows; colnames(glm_target_wo_Dem_df)[1:7] = df_cols
# rownames(glm_target_w_Dem_df) = df_rows; colnames(glm_target_w_Dem_df)[1:7] = df_cols
# glm_target_sqrt_df = glm_target_wo_Dem_df
# glm_target_sum_Digiprog_df = glm_target_wo_Dem_df[,1:6]
# 
# for (obs in obs_sizes) {
#   varname = paste("obs", obs, sep="")
#   estimates = get(varname)
#   wo_Dem_bias = pctBias(target_params_wo_Dem, as.matrix(estimates$glm.target.no_demographics))
#   w_Dem_bias = pctBias(target_params_w_Dem, as.matrix(estimates$glm.target.demographics))
#   sqrt_bias = pctBias(target_params_wo_Dem, as.matrix(estimates$glm.target.sqrt))
#   sum_Digiprog_bias = pctBias(target_params_wo_Dem[1:6], as.matrix(estimates$glm.target.sum_Digiprog))
#   glm_target_wo_Dem_df[varname,] = wo_Dem_bias
#   glm_target_w_Dem_df[varname,] = w_Dem_bias
#   glm_target_sqrt_df[varname,] = sqrt_bias
#   glm_target_sum_Digiprog_df[varname,] = sum_Digiprog_bias
# }

fit_total_audience_models <- function(X_w_demographics,
                                      beta_target, beta_nontarget,
                                      sample_size_total, sample_size_target,
                                      n_bootstraps,
                                      target_group_age = target_age,
                                      target_group_gender = target_gender) {
  
  start_time = Sys.time()
  
  N = nrow(X_w_demographics)
  
  svyglm_total = matrix(0, n_bootstraps, length(beta_target))
  glm_interaction = matrix(0, n_bootstraps, ( length(beta_target)+length(beta_nontarget) ))
  LRT_interaction = rep(0, n_bootstraps)
  
  for (i in 1:n_bootstraps) {
    
    idx_target = sample(1:nrow(X_w_demographics), sample_size_total, replace=T)
    idx_nontarget = sample(setdiff(1:nrow(X_w_demographics), idx_target), sample_size_total, replace=T)
    
    # Compile random sample
    target_data = nontarget_data = full_sample = list()
    target_data$predictors = X_w_demographics[idx_target,]
    target_data$kpi = generate_response(target_data$predictors,
                                        beta_target,
                                        nrow(target_data$predictors))
    nontarget_data$predictors = X_w_demographics[idx_nontarget,]
    nontarget_data$kpi = generate_response(nontarget_data$predictors,
                                           beta_nontarget,
                                           nrow(nontarget_data$predictors))
    
    full_sample$predictors = rbind(target_data$predictors, nontarget_data$predictors)
    full_sample$kpi = append(target_data$kpi, nontarget_data$kpi)
    
    # Compute weights
    weights = compute_weights(( nrow(target_data$predictors)/nrow(full_sample$predictors) ),
                              CPS[target_group_age, target_group_gender],
                              nrow(target_data$predictors),
                              nrow(nontarget_data$predictors))
    
    # Create data for interaction model (dependent is same as full_sample)
    interaction = rbind(target_data$predictors,
                        matrix(0, nrow(nontarget_data$predictors), ncol(nontarget_data$predictors)))
    interaction_predictors = cbind(full_sample$predictors, interaction)
    
    # KPI failsafe - avoid rare events setting
    min_mean_kpi = min(mean(target_data$kpi),
                       mean(nontarget_data$kpi))
    max_mean_kpi = max(mean(target_data$kpi),
                       mean(nontarget_data$kpi))
    if (min_mean_kpi < 0.1 | max_mean_kpi > 0.9) {
      stop(cat("Parameter choice creates a rare event scenario:\n",
               "Min kpi is ", 100*round(min_mean_kpi,3), "% ",
               "Max kpi is ", 100*round(max_mean_kpi,3), "%\n",
               "Breaking program, please try a less extreme parameter setting", sep=""))
    }
    
    # Fit Horvitz-Thompson
    svy_inputs = create_svyglm_inputs(full_sample$predictors, full_sample$kpi)
    design_func <- svydesign(id = ~1,
                             data = svy_inputs$data,
                             weight = weights)
    svyglm.total_audience <- svyglm(formula = svy_inputs$func,
                                    design =  design_func,
                                    family = "quasibinomial")
    
    # Fit interaction model
    glm.interaction_model.H1 <-  glm(full_sample$kpi ~ 0 + interaction_predictors,
                                  family = binomial(link="logit"))
    beta_interaction = append(beta_nontarget, (beta_target - beta_nontarget) )
    glm.interaction_model.H0 <- glm(full_sample$kpi ~
                                    0 + offset(interaction_predictors%*%beta_interaction),
                                  family = binomial(link = "logit"))
    glm.interaction_model.LRT = 2*abs(logLik(glm.interaction_model.H1) - logLik(glm.interaction_model.H0))
    glm.interaction_model.df = length(beta_interaction)
    
    # Store results
    svyglm_total[i,] = svyglm.total_audience$coefficients
    glm_interaction[i,] = glm.interaction_model.H1$coefficients
    LRT_interaction[i] = ( glm.interaction_model.LRT > qchisq(0.95, glm.interaction_model.df) )
    
    # Track progress
    if (i%%250 == 0) {
      current_time = Sys.time()
      elapsed = current_time - start_time
      print(paste("Currently at iteration:", i, "| Time elapsed:", round(elapsed, 2), attr(elapsed, "units")))
    }
    
  }
  
  # Retrieve target and total audience parameters from interaction model output
  glm_interaction_nontarget = glm_interaction[,1:length(beta_nontarget)]
  glm_interaction_target = (glm_interaction_nontarget +
                              glm_interaction[,(length(beta_nontarget)+1):(length(beta_nontarget) +
                                                                             length(beta_target))])
  glm_interaction_total = (CPS[target_age, target_gender]*glm_interaction_target +
                                   (1-CPS[target_age, target_gender])*glm_interaction_nontarget)
  
  
  out = list()
  out$svyglm.total_audience = svyglm_total
  out$glm.interaction_target_audience = glm_interaction_target
  out$glm.interaction_total_audience = glm_interaction_total
  out$LRT.interaction_model = LRT_interaction
  return(out)
}

if (TRUE) {
  for (N in c(2500, 3000)) {
    print(paste("N:", N))
    for (Q in 5*(10:16)) {
      print(paste("Q:", Q))
      assign(paste("N", N, "_Q", Q, sep=""),
             fit_total_audience_models(data_w_Dem,
                                       target_params_w_Dem, nontarget_params_w_Dem,
                                       sample_size_total = N, sample_size_target = (N*Q),
                                       n_bootstraps = 1000))
    }
  }
}

c <- c("N2500_Q50","N2500_Q55","N2500_Q60","N2500_Q65","N2500_Q70","N2500_Q75","N2500_Q80",
       "N3000_Q50","N3000_Q55","N3000_Q60","N3000_Q65","N3000_Q70","N3000_Q75","N3000_Q80")
p = dim(N2500_Q50$svyglm.total_audience)[2]
svyglm_res <- matrix(0,nrow = length(c), ncol=p)
rownames(svyglm_res) <- c
total_w_interaction_res <- matrix(0,nrow = length(c), ncol=p)
rownames(total_w_interaction_res) <- c
target_w_interaction_res <- matrix(0,nrow = length(c), ncol=p)
rownames(target_w_interaction_res) <- c
LRT_results = c()

for(i in c){
  estimates <- get(i)
  svyglm_res[i,] <- pctBias(true_population_params, estimates$svyglm.total_audience)
  total_w_interaction_res[i,] <- pctBias(true_population_params, estimates$glm.interaction_total_audience)
  target_w_interaction_res[i,] <- pctBias(target_params_w_Dem, estimates$glm.interaction_target_audience)
  LRT_results = append(LRT_results, mean(estimates$LRT.interaction_model))
}

