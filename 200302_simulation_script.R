# Fix seed and load libraries
set.seed(200127)
library(mvtnorm); library(simcausal); library(dplyr); library(survey); library(ggplot2); library(robustbase)

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

# Derive some necessary info from user inputs
necessary_info = initialise_target_group(target_gender, target_age, kpi)

# KEY STEP: Define target and nontarget parameters

true_target_params = c(-1.5, -3, 3.2, -1.0, 0.5, 1.0, .75)
#true_target_params = unname(logit.target.consideration$coefficients)
true_nontarget_params = c(-2.0, 0.6, 0.4, 0.2, 0.1, 0.5, 1)
#true_nontarget_params = unname(logit.nontarget.consideration$coefficients)
true_population_params = (CPS[target_age, target_gender]*true_target_params
                          + (1-CPS[target_age, target_gender])*true_nontarget_params)

# true_target_params = necessary_info$true_target_params
# true_target_params = unname(logit.target.consideration$coefficients)
#true_nontarget_params = unname(logit.nontarget.consideration$coefficients)
# true_population_params = necessary_info$true_population_params

simulated_target_predictors = necessary_info$target_data
simulated_targets = simulated_nontargets = real_nontargets = list()
simulated_targets$predictors = add_constant(simulated_target_predictors)
simulated_targets$kpi = generate_response(simulated_targets$predictors,
                                             true_target_params,
                                             nrow(simulated_targets$predictors) )

load("./simulated_nontarget_predictors.RData")
simulated_nontargets$predictors = add_constant(simulated_nontarget_population)
simulated_nontargets$kpi = generate_response(simulated_nontargets$predictors,
                                             true_nontarget_params,
                                             nrow(simulated_nontargets$predictors))

# real_nontargets$predictors = add_constant(separate_predictors_responses(subsamples$nontarget)$predictors)
# real_nontargets$kpi = generate_response(real_nontargets$predictors, true_nontarget_params, nrow(real_nontargets$predictors))

if ( max(mean(simulated_targets$kpi), mean(simulated_nontargets$kpi)) > 0.9 |
     min(mean(simulated_targets$kpi), mean(simulated_nontargets$kpi)) < 0.1 ) {
  stop(cat("Parameter choice creates a rare event scenario:\n",
           100*round(mean(simulated_targets$kpi),3), "% of targets and ",
           100*round(mean(simulated_nontargets$kpi),3), "% of nontargets have a KPI equal to 1\n",
           "Breaking program, please try a less extreme parameter set", sep="")) }

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
  nontarget_glm_results = data.frame(matrix(0, reps, p))
  total_svyglm_results = data.frame(matrix(0, reps, p))
  interaction_results = data.frame(matrix(0, reps, p*2))
  var_check_glm = rep(0, reps)
  var_check_svyglm = rep(0, reps)
  var_check_interaction = rep(0, reps)
  
  # Name columns of results df
  coefficient_names = c("Intercept", "Audio", "Digital", "Program", "TV",
                        "VOD", "Youtube", "Target", "Target*Audio", "Target*Digital",
                        "Target*Program", "Target*TV", "Target*VOD", "Target*Youtube")
  colnames(target_glm_results) = coefficient_names[1:(length(coefficient_names)/2)]
  colnames(total_svyglm_results) = coefficient_names[1:(length(coefficient_names)/2)]
  colnames(interaction_results) = coefficient_names
  
  for (i in 1:reps) {
    
    # Create index of random samples for target and non-target data
    idx_rs_targets = sample( 1:nrow(simulated_targets$predictors), N*Q )
    idx_rs_nontargets = sample( 1:nrow(subsamples$nontarget), (N*(1-Q)) )
    
    # Compile explanatory variables
    rs_targets = rs_nontargets = list()
    rs_targets$predictors = simulated_targets$predictors[idx_rs_targets,]
    rs_targets$kpi = simulated_targets$kpi[idx_rs_targets]
    
    rs_nontargets$predictors = simulated_nontargets$predictors[idx_rs_nontargets,]
    rs_nontargets$kpi = simulated_nontargets$kpi[idx_rs_nontargets]
    # rs_nontargets$predictors = real_nontargets$predictors[idx_rs_nontargets,]
    # rs_nontargets$kpi = real_nontargets$kpi[idx_rs_nontargets]
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
    
    # Create arbitrary variable for variance check
    meaningless_variable.target = rnorm(nrow(rs_targets$predictors)) # for target glm
    meaningless_variable.full_sample = rnorm(nrow(full_sample_w_interact$predictors)) # for svyglm, interaction model
    var_check_inputs = create_svyglm_inputs(cbind(full_sample$predictors, meaningless_variable.full_sample),
                                            full_sample$kpi)
    var_check_design_func <- svydesign(id = ~1,
                                  data = var_check_inputs$data,
                                  weight = weights)
    
    # If variances valid, models should reject the null hypothesis 5% of the time
    p_value.glm_var <- summary(glm(rs_targets$kpi ~ 0 + cbind(rs_targets$predictors,
                                                          meaningless_variable.target),
                               family = binomial(link="logit")))$coefficients[(ncol(rs_targets$predictors)+1),
                                                                              "Pr(>|z|)"]
    p_value.svyglm_var <- summary(svyglm(formula = var_check_inputs$func,
                                         design = var_check_design_func,
                                         family = "quasibinomial"))$coefficients[ncol(full_sample$predictors)+1,
                                                                                 "Pr(>|t|)"]
    p_value.ia_var <- summary(glm(full_sample$kpi ~ 0 + cbind(full_sample_w_interact$predictors,
                                                            meaningless_variable.full_sample),
                                        family = binomial(link="logit")))$coefficients[ncol(full_sample_w_interact$predictors)+1,
                                                                                       "Pr(>|z|)"]
    
    
    
    # Store results of current run
    target_glm_results[i,] = glm.target_audience$coefficients
    nontarget_glm_results[i,] = glm.nontarget_audience$coefficients
    total_svyglm_results[i,] = svyglm.total_audience$coefficients
    interaction_results[i,] = glm.interaction_model$coefficients
    var_check_glm[i] = p_value.glm_var
    var_check_svyglm[i] = p_value.svyglm_var
    var_check_interaction[i] = p_value.ia_var
    
    # Keep track of which simulation run we are in
    if (i%%100 == 0) {
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
  li_results$var_check_glm <- var_check_glm
  li_results$var_check_svyglm <- var_check_svyglm
  li_results$var_check_interaction <- var_check_interaction
  return(li_results)
}

#### Check variance of interaction model
# IA_var_check = run_simulation(N = 7500, Q=0.10, reps=1000,
#                               target_group_gender = target_gender, target_group_age = target_age,
#                               kpi = kpi)
# sum(IA_var_check$interaction_variance_check<=0.05)

# test_small_Q = run_simulation(N=2500, Q=0.001, reps = 500,
#                       target_group_gender = target_gender, target_group_age = target_age,
#                       kpi = kpi)

target_proportions = c(10,90)  #5*(2:18)[c(TRUE,FALSE)]
for (prop in target_proportions) {
  assign(paste("Q", prop, sep=""), c())
  assign(paste("Q", prop, sep=""), run_simulation(N = 7500, Q = prop/100, reps = 1000,
                                                  target_group_gender = target_gender, target_group_age = target_age,
                                                  kpi = kpi))
}

df_rows = paste("Q", target_proportions, sep = "")
df_cols = c("Intercept", "Audio", "Digital", "Program", "TV", "VOD", "Youtube")
glm_target_results_df = data.frame(matrix(0, length(df_rows), length(df_cols)))
rownames(glm_target_results_df) = df_rows; colnames(glm_target_results_df) = df_cols
svyglm_total_results_df = interaction_targ_results_df = glm_nontarget_results_df = interaction_total_results_df = interaction_nontarg_results_df = glm_target_results_df


for (prop in target_proportions) {
  varname = paste("Q", prop, sep="")
  estimates = get(varname)
  glm_targ_bias = pctBias(true_target_params, as.matrix(estimates$glm_target_audience))
  glm_nontarg_bias = pctBias(true_nontarget_params, as.matrix(estimates$glm_nontarget_audience))
  svyglm_total_bias = pctBias(true_population_params, as.matrix(estimates$svyglm_total_audience))
  interaction_targ_bias = pctBias(true_target_params, as.matrix(estimates$interaction_targ_results))
  interaction_total_bias = pctBias(true_population_params, as.matrix(estimates$interaction_total_results))
  
  interaction_nontarget_params = ((estimates$interaction_total_results
                                  - CPS[target_age, target_gender]*estimates$interaction_targ_results)
                                  /(1-CPS[target_age, target_gender]))
  interaction_nontarg_bias = pctBias(true_nontarget_params, as.matrix(interaction_nontarget_params))
  
  glm_target_results_df[varname,] = glm_targ_bias
  glm_nontarget_results_df[varname,] = glm_nontarg_bias
  svyglm_total_results_df[varname,] = svyglm_total_bias
  interaction_targ_results_df[varname,] = interaction_targ_bias
  interaction_total_results_df[varname,] = interaction_total_bias
  interaction_nontarg_results_df[varname,] = interaction_nontarg_bias
}

# See how the interaction model compares to weighting on total audience
# Note they are identical for target audience since they use same model
# (regular logit) on the same dataset (target data only)
round(100*interaction_total_results_df,1)[1:9,]
round(100*svyglm_total_results_df,1)[1:4,]
round(100*(interaction_total_results_df-svyglm_total_results_df),1)[1:8,]

round(100*glm_target_results_df,1)[1:9,]
round(100*glm_nontarget_results_df,1)[1:9,]
round(100*interaction_targ_results_df,1)[1:9,]
round(100*interaction_nontarg_results_df,1)[1:9,]
