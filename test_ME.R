install.packages("margins")
library(margins)
install.packages("matrixStats")
library(matrixStats)

# Simulation support functions
XBeta <- function(SampleStatistic, beta_vector){
  SampleStatistic*beta_vector
}
P_Yi1 <- function(XBeta){
  sigmoid_function(XBeta)
}
P_Yi0 <- function(XBeta){
  1 - sigmoid_function(XBeta)
}
marginal_effects_glm <- function(P_Yi1,P_Yi0, beta_vector){
  P_Yi1*P_Yi0*beta_vector
}
marginal_effects_svyglm <- function(weights, y_i, P_Yi0, beta_vector){
  (1/weights)*(y_i*beta_vector*P_Yi0())
}

X_statistic <- function(group, statistic = "mean"){
  if (group == "target"){
    X <- simulated_population$predictors
  }else if (group == "non-target"){
    X <- nontarget_predictors
  }
  else if (group == "total"){
    X <- true_fullsample_variables$predictors
  }
  
  if (statistic == "mean")
  {X_statistic <- colMeans(X)}
  else if (statistic == "median")
  {X_statistic <- colMedians(X)}
  else if (statistic == "Q25"){
    X_statistic <- colQuantiles((X))[,2]}
  else if (statistic == "Q75"){
    X_statistic <- colQuantiles((X))[,4]  
  }
  return(X_statistic)
}

Estimated_Beta <- function(simulated_coefficients){
  colMeans(simulated_coefficients)
}


set.seed(200127)
# install.packages("mvtnrom")
# install.packages("simcausal")
# install.packages("dplyr")
# install.packages("survey")
# install.packages("ggplot2")
# install.packages("robustbase")
library(mvtnorm); library(dplyr); library(survey); library(ggplot2); library(robustbase)

# Pathing - fix this on your machine first (set to local Git directory)
path = "~/Documents/Econometrie/Masters/Seminar Nielsen"
path = "C:/Users/marti/Documents/GitHub/Seminar"
setwd(path)
getwd()
source("./200302_simulation_support_functions.R")
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
true_nontarget_variables = separate_predictors_responses(subsamples$nontarget)

# Fit logit coefficients of the true target data
logit.target.familiarity <- glm( true_target_variables$familiarity ~ true_target_variables$predictors,
                                 family=binomial(link="logit") )
logit.target.awareness <- glm( true_target_variables$awareness ~ true_target_variables$predictors,
                               family=binomial(link="logit") )
logit.target.consideration <- glm( true_target_variables$consideration ~ true_target_variables$predictors,
                                   family=binomial(link="logit") )

# Fit logit coefficients of the true non-target data
logit.nontarget.familiarity <- glm( true_nontarget_variables$familiarity ~ true_nontarget_variables$predictors,
                                    family=binomial(link="logit") )
logit.nontarget.awareness <- glm( true_nontarget_variables$awareness ~ true_nontarget_variables$predictors,
                                  family=binomial(link="logit") )
logit.nontarget.consideration <- glm( true_nontarget_variables$consideration ~ true_nontarget_variables$predictors,
                                      family=binomial(link="logit") )

# Fit svyglm of the true target data
Q = .80
weights = compute_weights(Q,
                          CPS[target_age, target_gender],
                          nrow(true_target_variables$predictors),
                          nrow(true_nontarget_variables$predictors))
svyglm.target.familiarity <- svyglm( true_target_variables$familiarity ~ true_target_variables$predictors,
                                 family=binomial(link="logit") )
svyglm.target.awareness <- svyglm( true_target_variables$awareness ~ true_target_variables$predictors,
                               family=binomial(link="logit") )
svyglm.target.consideration <- svyglm( true_target_variables$consideration ~ true_target_variables$predictors,
                                   family=binomial(link="logit") )


# Fit svyglm of the true non-target data
svyglm.target.familiarity <- svyglm( true_nontarget_variables$familiarity ~ true_nontarget_variables$predictors,
                                     family=binomial(link="logit") )
svyglm.target.awareness <- svyglm( true_nontarget_variables$awareness ~ true_nontarget_variables$predictors,
                                   family=binomial(link="logit") )
svyglm.target.consideration <- svyglm( true_nontarget_variables$consideration ~ true_nontarget_variables$predictors,
                                       family=binomial(link="logit") )


# Derive some necessary info from user inputs
necessary_info = initialise_target_group(target_gender, target_age, kpi)


#true_target_params = necessary_info$true_target_params
true_target_params = c(-3, -2, -.5, .5, 1, 2, 3)
true_nontarget_params = c(-5, -3, -1, 1, 3, 5, 7)

#true_population_params = necessary_info$true_population_params
simulated_target_predictors = necessary_info$target_data
simulated_population = list()
simulated_population$predictors = add_constant(simulated_target_predictors)
simulated_population$kpi = generate_response(simulated_population$predictors,
                                             true_target_params,
                                             nrow(simulated_population$predictors) )

nontarget_predictors = add_constant(separate_predictors_responses(subsamples$nontarget)$predictors)
nontarget_response = generate_response(nontarget_predictors, true_nontarget_params, nrow(nontarget_predictors))

# True marginal effects for different groups, based on choice of statistic,
# default is the mean of X
true_target_me <- marginal_effects_glm(P_Yi1(X_statistic(group = "target")*true_target_params),
                                   P_Yi0(X_statistic(group = "target")*true_target_params),
                                   beta_vector = true_target_params)

true_nontarget_me <- marginal_effects_glm(P_Yi1(X_statistic(group = "non-target")*true_nontarget_params),
                                      P_Yi0(X_statistic(group = "non-target")*true_nontarget_params),
                                      beta_vector = true_nontarget_params)

true_population_params = CPS[target_age, target_gender]*true_target_params + (1-CPS[target_age, target_gender])*true_nontarget_params

# Set simulation hyperparameters
N = 2500
Q = .80
reps = 100

###################### RUN SIMULATION FUNCTION
run_simulation <- function(N, Q, reps, target_group_gender = target_gender, target_group_age = target_age, kpi = kpi) {
  
  # For looped simulation, keep track of which value of Q we are currently on
  print(paste("Q:", Q))
  start_time = Sys.time()
  
  # Allocate memory for simulation results
  p = ncol(true_fullsample_variables$predictors) + 1 # add dimension for intercept
  target_glm_results = data.frame(matrix(0, reps, p))
  nontarget_glm_results <- data.frame(matrix(0, reps, p))
  total_svyglm_results = data.frame(matrix(0, reps, p))
  interaction_results = data.frame(matrix(0, reps, p*2))
  interaction_model_variance_check = rep(0, reps)
  
  # Name columns of results df
  coefficient_names = c("Intercept", "Audio", "Digital", "Program", "TV",
                        "VOD", "Youtube", "Target", "Target*Audio", "Target*Digital",
                        "Target*Program", "Target*TV", "Target*VOD", "Target*Youtube")
  colnames(target_glm_results) = coefficient_names[1:(length(coefficient_names)/2)]
  colnames(total_svyglm_results) = coefficient_names[1:(length(coefficient_names)/2)]
  colnames(interaction_results) = coefficient_names
  
  for (i in 1:reps) {
    
    # Create index of random samples for target and non-target data
    idx_rs_targets = sample( 1:nrow(simulated_population$predictors), N*Q )
    idx_rs_nontargets = sample( 1:nrow(subsamples$nontarget), (N*(1-Q)) )
    
    # Compile explanatory variables
    rs_targets = rs_nontargets = list()
    rs_targets$predictors = simulated_population$predictors[idx_rs_targets,]
    rs_targets$kpi = simulated_population$kpi[idx_rs_targets]
    rs_nontargets$predictors = nontarget_predictors[idx_rs_nontargets,]
    rs_nontargets$kpi = nontarget_response[idx_rs_nontargets]
    #nontarget_data = separate_predictors_responses(subsamples$nontarget[idx_rs_nontargets,])
    #rs_nontargets$predictors = add_constant(nontarget_data$predictors)
    
    # Compile dependent variable (KPI)
    
    
    # if (tolower(kpi) == "consideration") {
    #   rs_nontargets$kpi = nontarget_data$consideration
    # } else if (tolower(kpi) == "awareness") {
    #   rs_nontargets$kpi = nontarget_data$awareness
    # } else if (tolower(kpi) == "familiarity") {
    #   rs_nontargets$kpi = nontarget_data$familiarity
    # } else {
    #   print("Invalid KPI specified, breaking the loop")
    #   break
    # }
    
    # Combine the target and non-target data into a single matrix
    full_sample = list()
    full_sample$predictors = rbind(rs_targets$predictors,
                                   rs_nontargets$predictors)
    full_sample$kpi = append(rs_targets$kpi,
                             rs_nontargets$kpi)
    
    # Compute weights
    weights = compute_weights(Q,
                              CPS[target_group_age, target_group_gender],
                              nrow(rs_targets$predictors),
                              nrow(rs_nontargets$predictors))
    
    # Create data for interaction model (dependent is same as full_sample)
    full_sample_w_interact = list()
    interaction = rbind(rs_targets$predictors,
                        matrix(0,
                               nrow(rs_nontargets$predictors),
                               ncol(rs_nontargets$predictors)))
    colnames(interaction) = rename_interaction_columns(colnames(interaction))
    full_sample_w_interact$predictors = cbind(full_sample$predictors, interaction)
    
    # Fit regular logit for model without interactions
    glm.target_audience <- glm(rs_targets$kpi ~ 0 + rs_targets$predictors,
                               family = binomial(link="logit"))
    glm.nontarget_audience <- glm(rs_nontargets$kpi ~ 0 + rs_nontargets$predictors,
                                  family = binomial(link="logit"))
    
    # Fit weighted logit for model without interactions
    svy_inputs = create_svyglm_inputs(full_sample$predictors, full_sample$kpi)
    design_func <- svydesign(id = ~1,
                             data = svy_inputs$data,
                             weight = weights)
    svyglm.total_audience <- svyglm(formula = svy_inputs$func,
                                    design =  design_func,
                                    family = "quasibinomial")
    
    # Fit unweighted logit for model with interactions
    glm.interaction_model <-  glm(full_sample$kpi ~ 0 + full_sample_w_interact$predictors,
                                  family = binomial(link="logit"))
    
    # Fit additional glm for interaction that allows variance check:
    # We add a nonsense variable that does not explain anything in y, and store parameter and SE
    # Model should then reject the null hypothesis 5% of the time
    meaningless_variable = rnorm(nrow(full_sample_w_interact$predictors))
    variance_check = list(predictors = cbind(full_sample_w_interact$predictors, meaningless_variable),
                          kpi = full_sample$kpi)
    glm.variance_check <- glm(variance_check$kpi ~ 0 + variance_check$predictors,
                              family = binomial(link="logit"))
    variance_check_p_value = summary(glm.variance_check)$coefficients[ncol(variance_check$predictors),
                                                                      "Pr(>|z|)"]
    
    # Store results of current run
    target_glm_results[i,] = glm.target_audience$coefficients
    total_svyglm_results[i,] = svyglm.total_audience$coefficients
    interaction_results[i,] = glm.interaction_model$coefficients
    #anova_results[i] <- anova(logit.sim.no_interact.weighted,method = "Wald")$p
    interaction_model_variance_check[i] = variance_check_p_value
    nontarget_glm_results[i,] <- glm.nontarget_audience$coefficients
    # Keep track of which simulation run we are in
    if (i%%25 == 0) {
      current_time = Sys.time()
      elapsed = current_time - start_time
      print(paste("Currently at iteration:", i, "| Time elapsed:", round(elapsed, 2), attr(elapsed, "units")))
    }
    
  }
  li_results = list()
  li_results$glm_target_audience <- target_glm_results
  li_results$glm_nontarget_audience <- nontarget_glm_results
  li_results$svyglm_total_audience <- total_svyglm_results
  li_results$interaction_targ_results <- interaction_results[,1:p] + interaction_results[,(p+1):(2*p)]
  li_results$interaction_total_results <- (interaction_results[,1:p]*(1-CPS[target_group_age, target_group_gender])
                                           + CPS[target_group_age, target_group_gender]*(interaction_results[,1:p]
                                                                                         + interaction_results[,(p+1):(2*p)]))
  li_results$interaction_variance_check <- interaction_model_variance_check
  li_results$weights <- weights
  return(li_results)
}

#### Check variance of interaction model
# IA_var_check = run_simulation(N = 7500, Q=0.10, reps=1000,
#                               target_group_gender = target_gender, target_group_age = target_age,
#                               kpi = kpi)
# sum(IA_var_check$interaction_variance_check<=0.05)

target_proportions = 5*(2:18)[c(TRUE,FALSE)]
for (prop in target_proportions) {
  assign(paste("Q", prop, sep=""), c())
  assign(paste("Q", prop, sep=""), run_simulation(N = 2500, Q = prop/100, reps = 100,
                                                  target_group_gender = target_gender, target_group_age = target_age,
                                                  kpi = kpi))
}

setwd("C:/Users/marti/Documents/Uni/Ectrie/Seminar")
load("~/Uni/Ectrie/Seminar/200309_overnight_simulation.RData")

df_rows = paste("Q", target_proportions, sep = "")
df_cols = c("Intercept", "Audio", "Digital", "Program", "TV", "VOD", "Youtube")
glm_target_results_df = data.frame(matrix(0, length(df_rows), length(df_cols)))
rownames(glm_target_results_df) = df_rows; colnames(glm_target_results_df) = df_cols
svyglm_total_results_df = interaction_targ_results_df = interaction_total_results_df = interaction_nontarg_results_df = glm_target_me_bias_df = glm_nontarget_me_bias_df = glm_target_results_df


for (prop in target_proportions) {
  varname = paste("Q", prop, sep="")
  estimates = get(varname)

  est_glm_target_me = marginal_effects_glm(P_Yi1(X_statistic(group = "target")*Estimated_Beta(estimates$glm_target_audience)), 
                                 P_Yi0(X_statistic(group = "target")*Estimated_Beta(estimates$glm_target_audience)),
                                 Estimated_Beta(estimates$glm_target_audience))
  glm_target_me_bias <- abs((est_glm_target_me- true_target_me)/true_target_me)
  
  est_glm_nontarget_me = marginal_effects_glm(P_Yi1(X_statistic(group = "non-target")*Estimated_Beta(estimates$glm_target_audience)), 
                                       P_Yi0(X_statistic(group = "non-target")*Estimated_Beta(estimates$glm_target_audience)),
                                       Estimated_Beta(estimates$glm_target_audience))
  glm_nontarget_me_bias <- abs((est_glm_nontarget_me- true_nontarget_me)/true_nontarget_me)
  

  # glm_nontarget_me_bias <- pctBias(true_nontarget_me,
  #                                  as.matrix(marginal_effects_glm(P_Yi1(X_statistic(group = "non-target")*Estimated_Beta(estimates$glm_target_audience)), 
  #                                                             P_Yi0(X_statistic(group = "non-target")*Estimated_Beta(estimates$glm_target_audience)),
  #                                                             Estimated_Beta(estimates$glm_target_audience))))
  
  interaction_nontarget_params = ((estimates$interaction_total_results
                                   - CPS[target_age, target_gender]*estimates$interaction_targ_results)
                                  /(1-CPS[target_age, target_gender]))
  interaction_nontarg_bias = pctBias(true_nontarget_params, as.matrix(interaction_nontarget_params))
  
  glm_target_results_df[varname,] = glm_targ_bias
  svyglm_total_results_df[varname,] = svyglm_total_bias
  interaction_targ_results_df[varname,] = interaction_targ_bias
  interaction_total_results_df[varname,] = interaction_total_bias
  interaction_nontarg_results_df[varname,] = interaction_nontarg_bias
  glm_target_me_bias_df[varname,] <- glm_target_me_bias
  glm_nontarget_me_bias_df[varname,] <- glm_nontarget_me_bias
}



# See how the interaction model compares to weighting on total audience
# Note they are identical for target audience since they use same model
# (regular logit) on the same dataset (target data only)
round(100*interaction_total_results_df,1)[1:9,]
round(100*svyglm_total_results_df,1)[1:9,]
round(100*(interaction_total_results_df-svyglm_total_results_df),1)[1:8,]

round(100*glm_target_results_df,1)[1:9,]
round(100*interaction_targ_results_df,1)[1:9,]
round(100*interaction_nontarg_results_df,1)[1:9,]




