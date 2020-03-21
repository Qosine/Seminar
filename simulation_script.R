#######################################################################
### Script that runs entire simulation
### Authors:  Seminar BA&QM - Nielsen Group 4
###           Hofstra/Kouwenhoven/Leenen/Stam
### Usage:    Runs straight from Nielsen source data as long as
###           R libraries listed on lines 15-16 are installed
### Runtime:  Approximately 6 hours
### CPU:      i7-8570H (6 physical cores + 6 hyperthreaded) @ 2.2 GHz
###           Parallelised on 6 cores with Microsoft R Open
#######################################################################


# Fix seed and load libraries
set.seed(123456)
library(mvtnorm); library(dplyr); library(survey); library(ggplot2); library(robustbase); library(xtable)
library(Rfast); library(reshape2); library(ggpubr)

# Pathing - fix this on your machine first (set to local Git directory)
path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"
setwd(path)
source("./simulation_support_functions.R")

#######################################################################
################## DO NOT CHANGE THE FOLLOWING ########################
#######################################################################

CPS <- rbind.data.frame(c(0.203195,0.10298,0.100214),
                        c(0.185959,0.092719,0.09324),
                        c(0.186438,0.091954,0.094484),
                        c(0.424408,0.195766,0.228643))
colnames(CPS) = c("Total", "Male", "Female")
rownames(CPS) = c("25-34", "35-44", "45-54", "55-99")

#######################################################################
####### LOAD POINTLOGIC SOURCE DATA AND DO SOME PRE-PROCESSING ########
#######################################################################

# Load Pointlogic source data
source_data = read.csv("./cleaned_unified_sample.csv")
data_wo_Dem = data_w_Dem = list()
data_w_Dem = add_constant(separate_predictors_responses(source_data)$predictors)
data_wo_Dem = data_w_Dem[,1:7]
ncol_to_add = dim(data_w_Dem)[2] - dim(data_wo_Dem)[2]

#######################################################################
####### CHOOSE TRUE PARAMETERS FOR TARGET AND NON-TARGET GROUPS #######
#######################################################################

target_params_wo_Dem = c(-2.0, 0.6, 1.2, -0.5, 0.7, 0.8, 0.4)
nontarget_params_wo_Dem = c(-1.5, -3, 3.2, -1.0, 0.5, 1.0, 0.6)

demographics_params = c(.31, .96, .71, -.66, .99, -.32, 1.44, 1.04,
                        1.86, 1.34, -1.36, -0.93, -1.22, -1.92, 0.75, -1.27,
                        -1.41, 0.37, -1.98, -0.56, 1.48)

target_params_w_Dem = append(target_params_wo_Dem, demographics_params)
nontarget_params_w_Dem = append(nontarget_params_wo_Dem, demographics_params)

#######################################################################
####################### MAIN SIMULATION FUNCTION ######################
#######################################################################

run_simulation <- function(X_w_demographics, n_interaction_variables,
                                      beta_target, beta_nontarget, beta_population,
                                      sample_size_total, sample_size_target,
                                      n_simulation_runs,
                                      target_group_age,
                                      target_group_gender) {
  
  # Start tracking runtime
  start_time = Sys.time()
  
  # Create parameters for interaction model (need interaction term parameters as well as non-target baseline)
  beta_interaction = append(beta_nontarget, (beta_target - beta_nontarget)[1:n_interaction_variables] )
  
  # Allocate storage space
  glm_target = matrix(0, n_simulation_runs, length(beta_target))
  svyglm_total = matrix(0, n_simulation_runs, length(beta_target))
  glm_interaction = matrix(0, n_simulation_runs, ( length(beta_target)+n_interaction_variables ))
  LRT_interaction = rep(0, n_simulation_runs)
  Wald_svyglm = rep(0, n_simulation_runs)
  ttest_svyglm = rep(0, n_simulation_runs)
  hitrate_svyglm = rep(0, n_simulation_runs)
  hitrate_interaction = rep(0, n_simulation_runs)
  bayesrate = rep(0, n_simulation_runs)
  alwayszero = rep(0, n_simulation_runs)
  
  for (i in 1:n_simulation_runs) {
    
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
    
    # Append target and non-target data
    full_sample$predictors = rbind(target_data$predictors, nontarget_data$predictors)
    full_sample$kpi = append(target_data$kpi, nontarget_data$kpi)
    
    # Compute weights for Horvitz-Thompson/weighted model
    weights = compute_weights(( nrow(target_data$predictors)/nrow(full_sample$predictors) ),
                              CPS[target_group_age, target_group_gender],
                              nrow(target_data$predictors),
                              nrow(nontarget_data$predictors))
    
    # Create data for interaction model (dependent is same as full_sample)
    interaction = rbind(target_data$predictors[,1:n_interaction_variables],
                        matrix(0, nrow(nontarget_data$predictors), n_interaction_variables))
    interaction_predictors = cbind(full_sample$predictors, interaction)
    
    # Create test set
    test_data$predictors = X_w_demographics[idx_test,]
    test_data$kpi = generate_response(test_data$predictors,
                                      beta_population,
                                      nrow(test_data$predictors))

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
    
    # Fit target data only GLM
    glm.target.H1 <- glm(target_data$kpi ~ 0 + target_data$predictors,
                         family = binomial(link="logit"))
    
    # Fit Horvitz-Thompson
    svy_inputs = create_svyglm_inputs(full_sample$predictors, full_sample$kpi)
    design_func <- svydesign(id = ~1,
                             data = svy_inputs$data,
                             weight = weights)
    svyglm.total_audience <- svyglm(formula = svy_inputs$func,
                                    design =  design_func,
                                    family = "quasibinomial")
    
    # Perform Wald test
    svyglm.total_audience.Wald = regTermTest(svyglm.total_audience,
                                             test.terms = ~ constant + audiosum + digitalsum + programsum + tvsum + vodsum + yousum + male + havechildren + age3544 + age55plus + employed + income3050 + income5075 + income75100 + income100150 + income150200 + income2001000 + educ3 + etn_cauc + etn_afric + etn_hisp + etn_asian + etn_native + etn_other + married + single + separated,
                                             null = beta_population,
                                             df = Inf,
                                             method = "Wald")
    
    # Alternative var check: Construct variable that does not contribute to Pr(Y=1) and use t-test
    meaningless_variable = rnorm(sample_size_total)
    ttest_inputs = create_svyglm_inputs(cbind(full_sample$predictors, meaningless_variable), full_sample$kpi)
    ttest_design = svydesign(id=~1,
                             data = ttest_inputs$data,
                             weight = weights)
    svyglm.total_audience.ttest = summary(svyglm(formula = ttest_inputs$func,
                                                 design =  ttest_design,
                                                 family = "quasibinomial"))$coefficients[ncol(full_sample$predictors)+1,"Pr(>|t|)"]
    
    # Fit interaction model
    glm.interaction_model.H1 <-  glm(full_sample$kpi ~ 0 + interaction_predictors,
                                  family = binomial(link="logit"))
    
    # Fit model under H0 for LRT and compute test statistic and df
    glm.interaction_model.H0 <- glm(full_sample$kpi ~
                                    0 + offset(interaction_predictors%*%beta_interaction),
                                  family = binomial(link = "logit"))
    glm.interaction_model.LRT = 2*abs(logLik(glm.interaction_model.H1) - logLik(glm.interaction_model.H0))
    glm.interaction_model.df = length(beta_interaction)
    
    # Derive relevant parameters from fitted interaction model
    glm.interaction_model.nontarget = glm.interaction_model.H1$coefficients[1:length(beta_nontarget)]
    glm.interaction_model.target = glm.interaction_model.nontarget
    glm.interaction_model.target[1:n_interaction_variables] = (glm.interaction_model.target[1:n_interaction_variables] +
                                                            glm.interaction_model.H1$coefficients[(length(beta_nontarget)+1):(length(beta_nontarget) + n_interaction_variables)])
    glm_interaction_model.total = transform_total_params(CPS[target_age, target_gender],
                                                         glm.interaction_model.target,
                                                         glm.interaction_model.nontarget)
    
    # Prediction
    svyglm.total_audience.predict = predict_response(test_data$predictors,
                                                     svyglm.total_audience$coefficients,
                                                     0.5)
    glm.interaction_model.predict = predict_response(test_data$predictors,
                                                     glm_interaction_model.total,
                                                     0.5)
    bayes_classifier = predict_response(test_data$predictors,
                                        beta_population,
                                        0.5)
    
    # Store results
    glm_target[i,] = glm.target.H1$coefficients
    svyglm_total[i,] = svyglm.total_audience$coefficients
    glm_interaction[i,] = glm.interaction_model.H1$coefficients
    LRT_interaction[i] = ( glm.interaction_model.LRT > qchisq(0.95, glm.interaction_model.df) )
    Wald_svyglm[i] = ( svyglm.total_audience.Wald$p <= 0.05 )
    ttest_svyglm[i] = ( svyglm.total_audience.ttest <= 0.05)
    hitrate_svyglm[i] = mean(svyglm.total_audience.predict==test_data$kpi)
    bayesrate[i] = mean(bayes_classifier==test_data$kpi)
    alwayszero[i] = mean(0==test_data$kpi)
    hitrate_interaction[i] = mean(glm.interaction_model.predict==test_data$kpi)
    
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
  glm_interaction_total = transform_total_params(CPS[target_age, target_gender],
                                                       glm_interaction_target,
                                                       glm_interaction_nontarget)
  
  # Create list of outputs
  out = list()
  out$glm.target_audience = glm_target
  out$svyglm.total_audience = svyglm_total
  out$glm.interaction_target_audience = glm_interaction_target
  out$glm.interaction_total_audience = glm_interaction_total
  out$LRT.interaction_model = LRT_interaction
  out$Wald.svyglm = Wald_svyglm
  out$ttest.svyglm = ttest_svyglm
  out$hitrate.svyglm = hitrate_svyglm
  out$bayesrate = bayesrate
  out$alwayszero = alwayszero
  out$hitrate.interaction = hitrate_interaction
  return(out)
}

#######################################################################
############### ALL CALLS MADE TO  SIMULATION FUNCTION ################
#######################################################################

# Target audience is 10% of population
# Written as conditional statement to also allow for automatic storage
if (TRUE) {

  # Choose value of P based on suitable CPS subgroup, derive true population parameters as explained in paper
  target_age = "25-34"
  target_gender = "Male"
  true_population_params_sig = transform_total_params(CPS[target_age, target_gender],
                                                      target_params_w_Dem,
                                                      nontarget_params_w_Dem)

  # Loop over sample sizes and all values of Q
  for (N in c(2000, 3000, 5000)) {
    print(paste("N:", N))
    for (Q in 5*(8:18)) {
      print(paste("Q:", Q))
      assign(paste("N", N, "_Q", Q, "_P10", sep=""),
             run_simulation(data_w_Dem, length(target_params_wo_Dem),
                                       target_params_w_Dem, nontarget_params_w_Dem, true_population_params_sig,
                                       sample_size_total = N, sample_size_target = (N*Q/100),
                                       n_simulation_runs = 2,
                                       target_group_age = target_age,
                                       target_group_gender = target_gender))
    }
  }
  # If desired, choose storage location below
  #save.image("D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar/200312_overnight_simulation_significant.RData")
}

# Target audience is 20% of population
# Change target audience based on CPS data and repeat entire procedure
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
             run_simulation(data_w_Dem, length(target_params_wo_Dem),
                                       target_params_w_Dem, nontarget_params_w_Dem, true_population_params_sig,
                                       sample_size_total = N, sample_size_target = (N*Q/100),
                                       n_simulation_runs = 2000,
                                       target_group_age = target_age,
                                       target_group_gender = target_gender))
    }
  }
  #save.image("D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar/200314_N2000_5000P40.RData")
}

# Target audience is 40% of population
# Change target audience based on CPS data and repeat entire procedure
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
      assign(paste("N", N, "_Q", Q, "_P40", sep=""),
             run_simulation(data_w_Dem, length(target_params_wo_Dem),
                                       target_params_w_Dem, nontarget_params_w_Dem, true_population_params_sig,
                                       sample_size_total = N, sample_size_target = (N*Q/100),
                                       n_simulation_runs = 2000,
                                       target_group_age = target_age,
                                       target_group_gender = target_gender))
    }
  }
  #save.image("D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar/200315_N1500_test")
}
