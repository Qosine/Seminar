# Fix seed and load libraries
set.seed(123456)
library(mvtnorm); library(dplyr); library(survey); library(ggplot2); library(robustbase); library(xtable);
library(Rfast); library(reshape2); library(ggpubr)

# Pathing - fix this on your machine first (set to local Git directory)
#path = "C:/Users/marcs/OneDrive/Bureaublad/Master/Seminar/Seminar/GitHub/Seminar"
# path = "~/Documents/Econometrie/Masters/Seminar Nielsen/Seminar"
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
kpi = "Consideration"




#######################################################################
####### LOAD POINTLOGIC SOURCE DATA AND DO SOME PRE-PROCESSING ########
#######################################################################

# Separate target age group for future reference
# target_min_age = as.numeric(substring(target_age, 1, 2))
# target_max_age = as.numeric(substring(target_age, nchar(target_age)-1, nchar(target_age)))

# Load Pointlogic source data
source_data = read.csv("./cleaned_unified_sample.csv")

# Separate true data into predictors and responses, and split target and non-target data
#true_fullsample_variables = separate_predictors_responses(source_data)
# subsamples = split_sample(source_data, tolower(target_gender), target_min_age, target_max_age)
# true_target_variables = separate_predictors_responses(subsamples$target)
# true_nontarget_variables = separate_predictors_responses(subsamples$nontarget)





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

fit_total_audience_models <- function(X_w_demographics, n_interaction_variables,
                                      beta_target, beta_nontarget, beta_population,
                                      sample_size_total, sample_size_target,
                                      n_bootstraps,
                                      target_group_age,
                                      target_group_gender) {
  
  start_time = Sys.time()
  
  N = nrow(X_w_demographics)
  beta_interaction = append(beta_nontarget, (beta_target - beta_nontarget)[1:n_interaction_variables] )
  
  glm_target = matrix(0, n_bootstraps, length(beta_target))
  svyglm_total = matrix(0, n_bootstraps, length(beta_target))
  glm_interaction = matrix(0, n_bootstraps, ( length(beta_target)+n_interaction_variables ))
  LRT_interaction = rep(0, n_bootstraps)
  Wald_svyglm = rep(0, n_bootstraps)
  ttest_svyglm = rep(0, n_bootstraps)
  hitrate_svyglm = rep(0, n_bootstraps)
  hitrate_interaction = rep(0, n_bootstraps)
  bayesrate = rep(0, n_bootstraps)
  alwayszero = rep(0, n_bootstraps)
  
  for (i in 1:n_bootstraps) {
    
    idx_target = sample(1:nrow(X_w_demographics), sample_size_target)
    idx_nontarget = sample(setdiff(1:nrow(X_w_demographics), idx_target), (sample_size_total-sample_size_target))
    idx_train = append(idx_target, idx_nontarget)
    idx_test = sample(setdiff(1:nrow(X_w_demographics), idx_train), 2500)
    
    # Compile random sample
    target_data = nontarget_data = full_sample = test_data = list()
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
    interaction = rbind(target_data$predictors[,1:n_interaction_variables],
                        matrix(0, nrow(nontarget_data$predictors), n_interaction_variables))
    interaction_predictors = cbind(full_sample$predictors, interaction)
    
    # Create test set
    # test_data$predictors = X_w_demographics[idx_test,]
    # test_data$kpi = generate_response(test_data$predictors,
    #                                   beta_population,
    #                                   nrow(test_data$predictors))
    # Q = sample_size_target/sample_size_total
    # sample_size_target_test = floor(Q*nrow(test_data$predictors))
    # test_data$interaction = rbind(test_data$predictors[1:sample_size_target_test, 1:n_interaction_variables],
    #                               matrix(0, nrow(test_data$predictors)-sample_size_target_test, n_interaction_variables))
    # test_data$interaction_predictors = cbind(test_data$predictors, test_data$interaction)
    # test_data$interaction_kpi = generate_response(test_data$interaction_predictors,
    #                                               beta_interaction,
    #                                               length(idx_test))
    # 
    
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
    
    # Targer GLM
    glm.target.H1 <- glm(target_data$kpi ~ 0 + target_data$predictors,
                         family = binomial(link="logit"))
    
    # Fit Horvitz-Thompson
    # svy_inputs = create_svyglm_inputs(full_sample$predictors, full_sample$kpi)
    # design_func <- svydesign(id = ~1,
    #                          data = svy_inputs$data,
    #                          weight = weights)
    # svyglm.total_audience <- svyglm(formula = svy_inputs$func,
    #                                 design =  design_func,
    #                                 family = "quasibinomial")
    # svyglm.total_audience.Wald = regTermTest(svyglm.total_audience,
    #                                          test.terms = ~ constant + audiosum + digitalsum + programsum + tvsum + vodsum + yousum + male + havechildren + age3544 + age55plus + employed + income3050 + income5075 + income75100 + income100150 + income150200 + income2001000 + educ3 + etn_cauc + etn_afric + etn_hisp + etn_asian + etn_native + etn_other + married + single + separated,
    #                                          null = beta_population,
    #                                          df = Inf,
    #                                          method = "Wald")
    # meaningless_variable = rnorm(sample_size_total)
    # ttest_inputs = create_svyglm_inputs(cbind(full_sample$predictors, meaningless_variable), full_sample$kpi)
    # ttest_design = svydesign(id=~1,
    #                          data = ttest_inputs$data,
    #                          weight = weights)
    # svyglm.total_audience.ttest = summary(svyglm(formula = ttest_inputs$func,
    #                                              design =  ttest_design,
    #                                              family = "quasibinomial"))$coefficients[ncol(full_sample$predictors)+1,"Pr(>|t|)"]
    
    # Fit interaction model
    glm.interaction_model.H1 <-  glm(full_sample$kpi ~ 0 + interaction_predictors,
                                  family = binomial(link="logit"))
    # glm.interaction_model.H0 <- glm(full_sample$kpi ~
    #                                 0 + offset(interaction_predictors%*%beta_interaction),
    #                               family = binomial(link = "logit"))
    # glm.interaction_model.LRT = 2*abs(logLik(glm.interaction_model.H1) - logLik(glm.interaction_model.H0))
    # glm.interaction_model.df = length(beta_interaction)
    glm.interaction_model.nontarget = glm.interaction_model.H1$coefficients[1:length(beta_nontarget)]
    glm.interaction_model.target = glm.interaction_model.nontarget
    glm.interaction_model.target[1:n_interaction_variables] = (glm.interaction_model.target[1:n_interaction_variables] +
                                                            glm.interaction_model.H1$coefficients[(length(beta_nontarget)+1):(length(beta_nontarget) + n_interaction_variables)])
    glm_interaction_model.total = (CPS[target_age, target_gender]*glm.interaction_model.target +
                                     (1-CPS[target_age, target_gender])*glm.interaction_model.nontarget)
    
    
    
    
    # Prediction
    # svyglm.total_audience.predict = predict_response(test_data$predictors,
    #                                                  svyglm.total_audience$coefficients,
    #                                                  0.5)
    # glm.interaction_model.predict = predict_response(test_data$predictors,
    #                                                  glm_interaction_model.total,
    #                                                  0.5)
    # bayes_classifier = predict_response(test_data$predictors,
    #                                     beta_population,
    #                                     0.5)
    
    # Store results
    glm_target[i,] = glm.target.H1$coefficients
    # svyglm_total[i,] = svyglm.total_audience$coefficients
    glm_interaction[i,] = glm.interaction_model.H1$coefficients
    # LRT_interaction[i] = ( glm.interaction_model.LRT > qchisq(0.95, glm.interaction_model.df) )
    # Wald_svyglm[i] = ( svyglm.total_audience.Wald$p <= 0.05 )
    # ttest_svyglm[i] = ( svyglm.total_audience.ttest <= 0.05)
    # hitrate_svyglm[i] = mean(svyglm.total_audience.predict==test_data$kpi)
    # bayesrate[i] = mean(bayes_classifier==test_data$kpi)
    # alwayszero[i] = mean(0==test_data$kpi)
    # hitrate_interaction[i] = mean(glm.interaction_model.predict==test_data$kpi)
    
    # Track progress
    if (i%%250 == 0) {
      current_time = Sys.time()
      elapsed = current_time - start_time
      print(paste("Currently at iteration:", i, "| Time elapsed:", round(elapsed, 2), attr(elapsed, "units")))
    }
    
  }
  
  # Retrieve target and total audience parameters from interaction model output
  glm_interaction_nontarget = glm_interaction[,1:length(beta_nontarget)]
  glm_interaction_target = glm_interaction_nontarget
  glm_interaction_target[,1:n_interaction_variables] = (glm_interaction_target[,1:n_interaction_variables] +
                                                          glm_interaction[,(length(beta_nontarget)+1):(length(beta_nontarget) + n_interaction_variables)])
  glm_interaction_total = (CPS[target_age, target_gender]*glm_interaction_target +
                                   (1-CPS[target_age, target_gender])*glm_interaction_nontarget)
  
  
  out = list()
  out$glm.target_audience = glm_target
  # out$svyglm.total_audience = svyglm_total
  out$glm.interaction_target_audience = glm_interaction_target
  out$glm.interaction_total_audience = glm_interaction_total
  # out$LRT.interaction_model = LRT_interaction
  # out$Wald.svyglm = Wald_svyglm
  # out$ttest.svyglm = ttest_svyglm
  # out$hitrate.svyglm = hitrate_svyglm
  # out$bayesrate = bayesrate
  # out$alwayszero = alwayszero
  # out$hitrate.interaction = hitrate_interaction
  return(out)
}

# 10% in population
if (TRUE) {

  # Compute population-level parameters as linear combination of target and non-target params
  target_age = "25-34"
  target_gender = "Male"
  true_population_params_sig = (CPS[target_age, target_gender]*target_params_w_Dem
                                + (1-CPS[target_age, target_gender])*nontarget_params_w_Dem)

  for (N in c(2000, 3000, 5000)) {
    print(paste("N:", N))
    for (Q in 5*(8:18)) {
      print(paste("Q:", Q))
      assign(paste("N", N, "_Q", Q, "_P10", sep=""),
             fit_total_audience_models(data_w_Dem, length(target_params_wo_Dem),
                                       target_params_w_Dem, nontarget_params_w_Dem, true_population_params_sig,
                                       sample_size_total = N, sample_size_target = (N*Q/100),
                                       n_bootstraps = 2000,
                                       target_group_age = target_age,
                                       target_group_gender = target_gender))
    }
  }
  #save.image("D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar/200312_overnight_simulation_significant.RData")
}

# 20% in population
if (TRUE) {

  # Compute population-level parameters as linear combination of target and non-target params
  target_age = "25-34"
  target_gender = "Total"
  true_population_params_sig = (CPS[target_age, target_gender]*target_params_w_Dem
                                + (1-CPS[target_age, target_gender])*nontarget_params_w_Dem)

  for (N in c(2000, 3000, 5000)) {
    print(paste("N:", N))
    for (Q in 5*(8:18)) {
      print(paste("Q:", Q))
      assign(paste("N", N, "_Q", Q, "_P20", sep=""),
             fit_total_audience_models(data_w_Dem, length(target_params_wo_Dem),
                                       target_params_w_Dem, nontarget_params_w_Dem, true_population_params_sig,
                                       sample_size_total = N, sample_size_target = (N*Q/100),
                                       n_bootstraps = 2000,
                                       target_group_age = target_age,
                                       target_group_gender = target_gender))
    }
  }
  save.image("D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar/200314_N2000_5000P40.RData")
}

# 40% in population
if (TRUE) {
  
  # Compute population-level parameters as linear combination of target and non-target params
  target_age = "55-99"
  target_gender = "Total"
  true_population_params_sig = (CPS[target_age, target_gender]*target_params_w_Dem
                                + (1-CPS[target_age, target_gender])*nontarget_params_w_Dem)
  
  for (N in c(2000, 3000, 5000)) {
    print(paste("N:", N))
    for (Q in 5*(8:18)) {
      print(paste("Q:", Q))
      assign(paste("N", N, "_Q", Q, "_P10", sep=""),
             fit_total_audience_models(data_w_Dem, length(target_params_wo_Dem),
                                       target_params_w_Dem, nontarget_params_w_Dem, true_population_params_sig,
                                       sample_size_total = N, sample_size_target = (N*Q/100),
                                       n_bootstraps = 2000,
                                       target_group_age = target_age,
                                       target_group_gender = target_gender))
    }
  }
  #save.image("D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar/200315_N1500_test")
}



#tables:
#toggle sigficant or insignificant
c <- c("significant_N2500_Q40","significant_N2500_Q45","significant_N2500_Q50","significant_N2500_Q55","significant_N2500_Q60","significant_N2500_Q65","significant_N2500_Q70","significant_N2500_Q75","significant_N2500_Q80","significant_N2500_Q85","significant_N2500_Q90",
       "significant_N3000_Q40","significant_N3000_Q45","significant_N3000_Q50","significant_N3000_Q55","significant_N3000_Q60","significant_N3000_Q65","significant_N3000_Q70","significant_N3000_Q75","significant_N3000_Q80","significant_N3000_Q85","significant_N3000_Q90", 
       "significant_N4000_Q40","significant_N4000_Q45","significant_N4000_Q50","significant_N4000_Q55","significant_N4000_Q60","significant_N4000_Q65","significant_N4000_Q70","significant_N4000_Q75","significant_N4000_Q80","significant_N4000_Q85","significant_N4000_Q90",
       "significant_N5000_Q40","significant_N5000_Q45","significant_N5000_Q50","significant_N5000_Q55","significant_N5000_Q60","significant_N5000_Q65","significant_N5000_Q70","significant_N5000_Q75","significant_N5000_Q80","significant_N5000_Q85","significant_N5000_Q90")
#c <- c("insignificant_N2500_Q40","insignificant_N2500_Q45","insignificant_N2500_Q50","insignificant_N2500_Q55","insignificant_N2500_Q60","insignificant_N2500_Q65","insignificant_N2500_Q70","insignificant_N2500_Q75","insignificant_N2500_Q80","insignificant_N2500_Q85","insignificant_N2500_Q90",
#       "insignificant_N3000_Q40","insignificant_N3000_Q45","insignificant_N3000_Q50","insignificant_N3000_Q55","insignificant_N3000_Q60","insignificant_N3000_Q65","insignificant_N3000_Q70","insignificant_N3000_Q75","insignificant_N3000_Q80","insignificant_N3000_Q85","insignificant_N3000_Q90", 
#       "insignificant_N4000_Q40","insignificant_N4000_Q45","insignificant_N4000_Q50","insignificant_N4000_Q55","insignificant_N4000_Q60","insignificant_N4000_Q65","insignificant_N4000_Q70","insignificant_N4000_Q75","insignificant_N4000_Q80","insignificant_N4000_Q85","insignificant_N4000_Q90",
#       "insignificant_N5000_Q40","insignificant_N5000_Q45","insignificant_N5000_Q50","insignificant_N5000_Q55","insignificant_N5000_Q60","insignificant_N5000_Q65","insignificant_N5000_Q70","insignificant_N5000_Q75","insignificant_N5000_Q80","insignificant_N5000_Q85","insignificant_N5000_Q90")
true_population_params <- true_population_params_sig
#true_population_params <- true_population_params_insig
setwd("./Plots_significant")
#setwd("./Plots_insignificant")

cols <- c("/mu_E PB", "/mu SB",		"max_E PB", "Max_E PB",	"/mu_D pB",	"/mu_D SB")
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
  prediction_result[j,] <- colMeans(cbind(estimates$hitrate.svyglm,estimates$hitrate.interaction,estimates$bayesrate,estimates$alwayszero))
  j=j+1
}

#prediction 
colnames(prediction_result) <- c("Weighted model", "Interaction model", "Bayes Classifier", "Always zero")
rownames(prediction_result) <-  substr(c,nchar(c)-8,nchar(c))
print(xtable(round(prediction_result*100,2), type = "latex"), file = "Prediction_table.tex")


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

for( j in 1:44){
  
  glm_standardized_bias[j] <-  100*sqrt(t(colMeans(est_int_svy[[j]], na.rm=T) - true_population_params) %*% solve(var(est_int_svy[[j]], na.rm=TRUE)) %*% (colMeans(est_int_svy[[j]],na.rm=T)-true_population_params))
  glm_standardized_bias_ind_mean[j] <-   mean(abs(100*(colMeans(est_int_svy[[j]][,1:14],na.rm=T)-true_population_params[1:14])/diag(var(est_int_svy[[j]][,1:14],na.rm=T))))
  glm_standardized_bias_ind_max[j] <-     max(abs(100*(colMeans(est_int_svy[[j]][,1:14],na.rm=T)-true_population_params[1:14])/diag(var(est_int_svy[[j]][,1:14],na.rm=T))))
  glm_standardized_bias_ind_mean_D[j] <- mean(abs(100*(colMeans(est_int_svy[[j]][,15:28],na.rm=T)-true_population_params[15:28])/diag(var(est_int_svy[[j]][,15:28],na.rm=T))))
  
  intera_standardized_bias[j] <-  100*sqrt(t(colMeans(est_int_total[[j]], na.rm=T) - true_population_params) %*% solve(var(est_int_total[[j]], na.rm=TRUE)) %*% (colMeans(est_int_total[[j]],na.rm=T)-true_population_params))
  intera_standardized_bias_ind_mean[j] <-   mean(abs(100*(colMeans(est_int_total[[j]][,1:14],na.rm=T)-true_population_params[1:14])/diag(var(est_int_total[[j]][,1:14],na.rm=T))))
  intera_standardized_bias_ind_max[j] <-     max(abs(100*(colMeans(est_int_total[[j]][,1:14],na.rm=T)-true_population_params[1:14])/diag(var(est_int_total[[j]][,1:14],na.rm=T))))
  intera_standardized_bias_ind_mean_D[j] <-mean(abs(100*(colMeans(est_int_total[[j]][,15:28],na.rm=T)-true_population_params[15:28])/diag(var(est_int_total[[j]][,15:28],na.rm=T))))
  
  intera_target_standardized_bias[j] <-  100*sqrt(t(colMeans(est_int_target[[j]], na.rm=T) - target_params_w_Dem) %*% solve(var(est_int_target[[j]], na.rm=TRUE)) %*% (colMeans(est_int_target[[j]],na.rm=T)-target_params_w_Dem))
  intera_target_standardized_bias_ind_mean[j] <-   mean(abs(100*(colMeans(est_int_target[[j]][,1:14],na.rm=T)-target_params_w_Dem[1:14])/diag(var(est_int_target[[j]][,1:14],na.rm=T))))
  intera_target_standardized_bias_ind_max[j] <-     max(abs(100*(colMeans(est_int_target[[j]][,1:14],na.rm=T)-target_params_w_Dem[1:14])/diag(var(est_int_target[[j]][,1:14],na.rm=T))))
  intera_target_standardized_bias_ind_mean_D[j] <-mean(abs(100*(colMeans(est_int_target[[j]][,15:28],na.rm=T)-target_params_w_Dem[15:28])/diag(var(est_int_target[[j]][,15:28],na.rm=T))))
  
}
# Standardised Biases
SB_result_mat <- matrix(0, nrow=11, ncol=12)
rownames(SB_result_mat) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
SB_result_mat[,1] <- glm_standardized_bias[1:11]
SB_result_mat[,2] <- glm_standardized_bias[12:22]
SB_result_mat[,3] <- glm_standardized_bias[23:33]
SB_result_mat[,4] <- glm_standardized_bias[34:44]

SB_result_mat[,5] <- intera_target_standardized_bias[1:11]
SB_result_mat[,6] <- intera_target_standardized_bias[12:22]
SB_result_mat[,7] <- intera_target_standardized_bias[23:33]
SB_result_mat[,8] <- intera_target_standardized_bias[34:44]

SB_result_mat[,9] <- intera_standardized_bias[1:11]
SB_result_mat[,10] <- intera_standardized_bias[12:22]
SB_result_mat[,11] <- intera_standardized_bias[23:33]
SB_result_mat[,12] <- intera_standardized_bias[34:44]


# SVYGLM results

# N = 2500
N2500_result_mat <- matrix(0, nrow=11, ncol=6)
rownames(N2500_result_mat) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N2500_result_mat) = cols
N2500_result_mat[,1] <- round(rowMeans(100*svyglm_res[1:11,1:14]),2)
N2500_result_mat[,2] <- round(glm_standardized_bias_ind_mean[1:11], 2)
N2500_result_mat[,3] <- round(rowMaxs(100*svyglm_res[1:11,1:14], value = TRUE),2)
N2500_result_mat[,4] <- colnames(data_w_Dem)[rowMaxs(100*svyglm_res[1:11,1:14], value = FALSE)]
N2500_result_mat[,5] <- round(rowMeans(100*svyglm_res[1:11,15:28]),2)
N2500_result_mat[,6] <- round(glm_standardized_bias_ind_mean_D[1:11],2)
# N = 3000
N3000_result_mat <- matrix(0, nrow=11, ncol=6)
rownames(N3000_result_mat) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N3000_result_mat) = cols
N3000_result_mat[,1] <- round(rowMeans(100*svyglm_res[12:22,1:14]),2)
N3000_result_mat[,2] <- round(glm_standardized_bias_ind_mean[12:22], 2)
N3000_result_mat[,3] <- round(rowMaxs(100*svyglm_res[12:22,1:14], value = TRUE),2)
N3000_result_mat[,4] <- colnames(data_w_Dem)[rowMaxs(100*svyglm_res[12:22,1:14], value = FALSE)]
N3000_result_mat[,5] <- round(rowMeans(100*svyglm_res[12:22,15:28]),2)
N3000_result_mat[,6] <- round(glm_standardized_bias_ind_mean_D[12:22],2)
# N = 4000
N4000_result_mat <- matrix(0, nrow=11, ncol=6)
rownames(N4000_result_mat) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N4000_result_mat) = cols
N4000_result_mat[,1] <- round(rowMeans(100*svyglm_res[23:33,1:14]),2)
N4000_result_mat[,2] <- round(glm_standardized_bias_ind_mean[23:33], 2)
N4000_result_mat[,3] <- round(rowMaxs(100*svyglm_res[23:33,1:14], value = TRUE),2)
N4000_result_mat[,4] <- colnames(data_w_Dem)[rowMaxs(100*svyglm_res[23:33,1:14], value = FALSE)]
N4000_result_mat[,5] <- round(rowMeans(100*svyglm_res[23:33,15:28]),2)
N4000_result_mat[,6] <- round(glm_standardized_bias_ind_mean_D[23:33],2)
# N = 5000
N5000_result_mat <- matrix(0, nrow=11, ncol=6)
rownames(N5000_result_mat) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N5000_result_mat) = cols
N5000_result_mat[,1] <- round(rowMeans(100*svyglm_res[34:44,1:14]),2)
N5000_result_mat[,2] <- round(glm_standardized_bias_ind_mean[34:44], 2)
N5000_result_mat[,3] <- round(rowMaxs(100*svyglm_res[34:44,1:14], value = TRUE),2)
N5000_result_mat[,4] <- colnames(data_w_Dem)[rowMaxs(100*svyglm_res[34:44,1:14], value = FALSE)]
N5000_result_mat[,5] <- round(rowMeans(100*svyglm_res[34:44,15:28]),2)
N5000_result_mat[,6] <- round(glm_standardized_bias_ind_mean_D[34:44],2)



# Interaction total results

# N = 2500
N2500_result_mat_total <- matrix(0, nrow=11, ncol=6)
rownames(N2500_result_mat_total) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N2500_result_mat_total) = cols
N2500_result_mat_total[,1] <- round(rowMeans(100*total_w_interaction_res[1:11,1:14]),2)
N2500_result_mat_total[,2] <- round(intera_standardized_bias_ind_mean[1:11], 2)
N2500_result_mat_total[,3] <- round(rowMaxs(100*total_w_interaction_res[1:11,1:14], value = TRUE),2)
N2500_result_mat_total[,4] <- colnames(data_w_Dem)[rowMaxs(100*total_w_interaction_res[1:11,1:14], value = FALSE)]
N2500_result_mat_total[,5] <- round(rowMeans(100*total_w_interaction_res[1:11,15:28]),2)
N2500_result_mat_total[,6] <- round(intera_standardized_bias_ind_mean_D[1:11],2)
# N = 3000
N3000_result_mat_total <- matrix(0, nrow=11, ncol=6)
rownames(N3000_result_mat_total) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N3000_result_mat_total) = cols
N3000_result_mat_total[,1] <- round(rowMeans(100*total_w_interaction_res[12:22,1:14]),2)
N3000_result_mat_total[,2] <- round(intera_standardized_bias_ind_mean[12:22], 2)
N3000_result_mat_total[,3] <- round(rowMaxs(100*total_w_interaction_res[12:22,1:14], value = TRUE),2)
N3000_result_mat_total[,4] <- colnames(data_w_Dem)[rowMaxs(100*total_w_interaction_res[12:22,1:14], value = FALSE)]
N3000_result_mat_total[,5] <- round(rowMeans(100*total_w_interaction_res[12:22,15:28]),2)
N3000_result_mat_total[,6] <- round(intera_standardized_bias_ind_mean_D[12:22],2)
# N = 4000
N4000_result_mat_total <- matrix(0, nrow=11, ncol=6)
rownames(N4000_result_mat_total) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N4000_result_mat_total) = cols
N4000_result_mat_total[,1] <- round(rowMeans(100*total_w_interaction_res[23:33,1:14]),2)
N4000_result_mat_total[,2] <- round(intera_standardized_bias_ind_mean[23:33], 2)
N4000_result_mat_total[,3] <- round(rowMaxs(100*total_w_interaction_res[23:33,1:14], value = TRUE),2)
N4000_result_mat_total[,4] <- colnames(data_w_Dem)[rowMaxs(100*total_w_interaction_res[23:33,1:14], value = FALSE)]
N4000_result_mat_total[,5] <- round(rowMeans(100*total_w_interaction_res[23:33,15:28]),2)
N4000_result_mat_total[,6] <- round(intera_standardized_bias_ind_mean_D[23:33],2)
# N = 5000
N5000_result_mat_total <- matrix(0, nrow=11, ncol=6)
rownames(N5000_result_mat_total) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N5000_result_mat_total) = cols
N5000_result_mat_total[,1] <- round(rowMeans(100*total_w_interaction_res[34:44,1:14]),2)
N5000_result_mat_total[,2] <- round(intera_standardized_bias_ind_mean[34:44], 2)
N5000_result_mat_total[,3] <- round(rowMaxs(100*total_w_interaction_res[34:44,1:14], value = TRUE),2)
N5000_result_mat_total[,4] <- colnames(data_w_Dem)[rowMaxs(100*total_w_interaction_res[34:44,1:14], value = FALSE)]
N5000_result_mat_total[,5] <- round(rowMeans(100*total_w_interaction_res[34:44,15:28]),2)
N5000_result_mat_total[,6] <- round(intera_standardized_bias_ind_mean_D[34:44],2)



# Interaction Target glm

# N = 2500
N2500_result_mat_target <- matrix(0, nrow=11, ncol=6)
rownames(N2500_result_mat_target) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N2500_result_mat_target) = cols
N2500_result_mat_target[,1] <- round(rowMeans(100*target_w_interaction_res[1:11,1:14]),2)
N2500_result_mat_target[,2] <- round(intera_target_standardized_bias_ind_mean[1:11], 2)
N2500_result_mat_target[,3] <- round(rowMaxs(100*target_w_interaction_res[1:11,1:14], value = TRUE),2)
N2500_result_mat_target[,4] <- colnames(data_w_Dem)[rowMaxs(100*target_w_interaction_res[1:11,1:14], value = FALSE)]
N2500_result_mat_target[,5] <- round(rowMeans(100*target_w_interaction_res[1:11,15:28]),2)
N2500_result_mat_target[,6] <- round(intera_target_standardized_bias_ind_mean_D[1:11],2)

# N = 3000
N3000_result_mat_target <- matrix(0, nrow=11, ncol=6)
rownames(N3000_result_mat_target) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N3000_result_mat_target) = cols
N3000_result_mat_target[,1] <- round(rowMeans(100*target_w_interaction_res[12:22,1:14]),2)
N3000_result_mat_target[,2] <- round(intera_target_standardized_bias_ind_mean[12:22], 2)
N3000_result_mat_target[,3] <- round(rowMaxs(100*target_w_interaction_res[12:22,1:14], value = TRUE),2)
N3000_result_mat_target[,4] <- colnames(data_w_Dem)[rowMaxs(100*target_w_interaction_res[12:22,1:14], value = FALSE)]
N3000_result_mat_target[,5] <- round(rowMeans(100*target_w_interaction_res[12:22,15:28]),2)
N3000_result_mat_target[,6] <- round(intera_target_standardized_bias_ind_mean_D[12:22],2)

# N = 4000
N4000_result_mat_target <- matrix(0, nrow=11, ncol=6)
rownames(N4000_result_mat_target) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N4000_result_mat_target) = cols
N4000_result_mat_target[,1] <- round(rowMeans(100*target_w_interaction_res[23:33,1:14]),2)
N4000_result_mat_target[,2] <- round(intera_target_standardized_bias_ind_mean[23:33], 2)
N4000_result_mat_target[,3] <- round(rowMaxs(100*target_w_interaction_res[23:33,1:14], value = TRUE),2)
N4000_result_mat_target[,4] <- colnames(data_w_Dem)[rowMaxs(100*target_w_interaction_res[23:33,1:14], value = FALSE)]
N4000_result_mat_target[,5] <- round(rowMeans(100*target_w_interaction_res[23:33,15:28]),2)
N4000_result_mat_target[,6] <- round(intera_target_standardized_bias_ind_mean_D[23:33],2)

# N = 5000
N5000_result_mat_target <- matrix(0, nrow=11, ncol=6)
rownames(N5000_result_mat_target) = c("0.4","0.45","0.50","0.55","0.60","0.65","0.70","0,75","0.80","0.85","0.9")
colnames(N5000_result_mat_target) = cols
N5000_result_mat_target[,1] <- round(rowMeans(100*target_w_interaction_res[34:44,1:14]),2)
N5000_result_mat_target[,2] <- round(intera_target_standardized_bias_ind_mean[34:44], 2)
N5000_result_mat_target[,3] <- round(rowMaxs(100*target_w_interaction_res[34:44,1:14], value = TRUE),2)
N5000_result_mat_target[,4] <- colnames(data_w_Dem)[rowMaxs(100*target_w_interaction_res[34:44,1:14], value = FALSE)]
N5000_result_mat_target[,5] <- round(rowMeans(100*target_w_interaction_res[34:44,15:28]),2)
N5000_result_mat_target[,6] <- round(intera_target_standardized_bias_ind_mean_D[34:44],2)


# Standardised Biases
print(xtable(SB_result_mat, type = "latex"), file = "SB_results.tex")

# SVYGLM & Interaction Total
print(xtable(N2500_result_mat, type = "latex"), file = "N2500_results.tex")
print(xtable(N3000_result_mat, type = "latex"), file = "N3000_results.tex")
print(xtable(N4000_result_mat, type = "latex"), file = "N4000_results.tex")
print(xtable(N5000_result_mat, type = "latex"), file = "N5000_results.tex")

# Interaction Target
print(xtable(N2500_result_mat_target, type = "latex"), file = "N2500_target_results.tex")
print(xtable(N3000_result_mat_target, type = "latex"), file = "N3000_target_results.tex")
print(xtable(N4000_result_mat_target, type = "latex"), file = "N4000_target_results.tex")
print(xtable(N5000_result_mat_target, type = "latex"), file = "N5000_target_results.tex")

# Interaction Total
print(xtable(N2500_result_mat_total, type = "latex"), file = "N2500_total_results.tex")
print(xtable(N3000_result_mat_total, type = "latex"), file = "N3000_total_results.tex")
print(xtable(N4000_result_mat_total, type = "latex"), file = "N4000_total_results.tex")
print(xtable(N5000_result_mat_total, type = "latex"), file = "N5000_total_results.tex")





#Grahps for interaction and svyglm percentage bias
N_obs <- c("2500","3000","4000","5000")
save_name_IA <- c("MaxMean_IA_2500.png","MaxMean_IA_3000.png","MaxMean_IA_4000.png","MaxMean_IA_5000.png")
save_name_svy <- c("MaxMean_svy_2500.png","MaxMean_svy_3000.png","MaxMean_svy_4000.png","MaxMean_svy_5000.png")
svy_grahps <- list()
IA_graphs <- list()
for(i in 1:4){
  IA_res <- cbind(Q,cbind(rowMeans(100*round(total_w_interaction_res[(1+(i-1)*11):(i*11),1:14],2)),
                          rowMaxs(100*round(total_w_interaction_res[(1+(i-1)*11):(i*11),1:14],3), value=TRUE),
                          rowMeans(100*round(target_w_interaction_res[(1+(i-1)*11):(i*11),1:14],2)),
                          rowMaxs(100*round(target_w_interaction_res[(1+(i-1)*11):(i*11),1:14],3), value=TRUE)))
  colnames(IA_res) <- c("Q", "IA Total Mean", "IA Total Max","IA Target Mean", "IA Target Max")
  
  svy_res <- cbind(Q,cbind(rowMeans(100*round(svyglm_res[(1+(i-1)*11):(i*11),1:14],2)),
                           rowMaxs(100*round(svyglm_res[(1+(i-1)*11):(i*11),1:14],3), value=TRUE),
                           rowMeans(100*round(target_w_interaction_res[(1+(i-1)*11):(i*11),1:14],2)),
                           rowMaxs(100*round(target_w_interaction_res[(1+(i-1)*11):(i*11),1:14],3), value=TRUE)))
  colnames(svy_res) <- c("Q", "SVY Total Mean", "SVY Total Max","SVY Target Mean", "SVY Target Max")
  
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
    ggtitle(paste("Mean, Max PB for IA Model, N =", N_obs[i], sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_light() +
    ylim(0,30) +
    theme(panel.grid.major.y = element_line( size=.1, color="grey" ) ,
          axis.line = element_line(colour = "black"),plot.title = element_text(size=10, face = "bold"),
          legend.position="right", legend.text = element_text(size=10),
          panel.grid = element_blank(), legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
          axis.line.x.top = element_blank(),panel.border = element_blank(), axis.line.y.right = element_blank())
  ggsave(save_name_IA[i])
  
  # ggplot for svyglm
  svy_grahps[[i]] <- ggplot(temp_svy, aes(x=Q, y= Bias, colour = Legend))+
    geom_point()+
    geom_line() +
    ggtitle(paste("Mean, Max PB for SVY Model, N =", N_obs[i], sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_light() +
    ylim(0,30) +
    theme(panel.grid.major.y = element_line( size=.1, color="grey" ) ,
          axis.line = element_line(colour = "black"),plot.title = element_text(size=10, face = "bold"),
          legend.position="right", legend.text = element_text(size=10),
          panel.grid = element_blank(), legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
          axis.line.x.top = element_blank(),panel.border = element_blank(), axis.line.y.right = element_blank())
  ggsave(save_name_svy[i])
}
ggarrange(svy_grahps[[1]],svy_grahps[[4]], common.legend = TRUE, legend="bottom")
ggsave("svy_graphs_2500_5000.png")
ggarrange(svy_grahps[[2]],svy_grahps[[3]], common.legend = TRUE, legend="bottom")
ggsave("svy_graphs_3000_4000.png")
ggarrange(IA_graphs[[2]],IA_graphs[[3]], common.legend = TRUE, legend="bottom")
ggsave("IA_graphs_3000_4000.png")
ggarrange(IA_graphs[[1]],IA_graphs[[4]], common.legend = TRUE, legend="bottom")
ggsave("IA_graphs_2500_5000.png")



#standardized bias graph
N_obs <- c("2500","3000","4000","5000")
save_name_IA <- c("MaxMean_IA_2500.png","MaxMean_IA_3000.png","MaxMean_IA_4000.png","MaxMean_IA_5000.png")
save_name_svy <- c("MaxMean_svy_2500.png","MaxMean_svy_3000.png","MaxMean_svy_4000.png","MaxMean_svy_5000.png")
stdbias_graphs <- list()

for(i in 1:4){
  stdbias_res <- cbind(Q,cbind(glm_standardized_bias[(1+(i-1)*11):(i*11)],
                               (intera_standardized_bias[(1+(i-1)*11):(i*11)]),
                               (intera_target_standardized_bias[(1+(i-1)*11):(i*11)])))
  colnames(stdbias_res) <- c("Q", "SVY", "IA Total","IA Target")
  
  temp_IA <- melt(stdbias_res[1:11,-1])
  temp_IA$Q <- rep(Q,3)
  colnames(temp_IA) <- c("Var1","Legend", "Bias","Q")
  
  # ggplot for standardized bias
  stdbias_graphs[[i]] <- ggplot(temp_IA, aes(x=Q, y= Bias, colour = Legend))+
    geom_point()+
    geom_line() +
    ggtitle(paste("SB for N =", N_obs[i] , sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_light() +
    ylim(0,120) +
    theme(panel.grid.major.y = element_line( size=.1, color="grey" ) ,
          axis.line = element_line(colour = "black"),plot.title = element_text(size=10, face = "bold"),
          legend.position="right", legend.text = element_text(size=10),
          panel.grid = element_blank(), legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
          axis.line.x.top = element_blank(),panel.border = element_blank(), axis.line.y.right = element_blank())
  
  
}
ggarrange(stdbias_graphs[[1]], stdbias_graphs[[2]],stdbias_graphs[[3]],stdbias_graphs[[4]], common.legend = TRUE, legend="bottom")
ggsave("stdbias_graph.png")

#LRT result
LRT_res_fin <- 100*round(cbind(LRT_res[1:11],LRT_res[12:22],LRT_res[23:33],LRT_res[34:44])/1000,3)
rownames(LRT_res_fin) <-  c(8:18)*0.05
colnames(LRT_res_fin) <- c("2500","3000", "4000","5000")
print(xtable(LRT_res_fin, type = "latex"), file = "LRT_results.tex")

#Wald result

#Hitrates


