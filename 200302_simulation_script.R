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
logit.nontarget.familiarity <- glm( true_nontarget_variables$familiarity
                                    ~ true_nontarget_variables$predictors,
                                    family=binomial(link="logit") )
logit.nontarget.awareness <- glm( true_nontarget_variables$awareness
                                  ~ true_nontarget_variables$predictors,
                                  family=binomial(link="logit") )
logit.nontarget.consideration <- glm( true_nontarget_variables$consideration
                                      ~ true_nontarget_variables$predictors,
                                      family=binomial(link="logit") )

# Derive some necessary info from user inputs
necessary_info = initialise_target_group(target_gender, target_age)
true_population_params = necessary_info$true_population_params
simulated_target_predictors = necessary_info$target_data
simulated_population = list()
simulated_population$predictors = add_constant(simulated_target_predictors)
simulated_population$familiarity = generate_response(simulated_population$predictors,
                                                     logit.target.familiarity$coefficients,
                                                     nrow(simulated_population$predictors) )
simulated_population$awareness = generate_response(simulated_population$predictors,
                                                   logit.target.awareness$coefficients,
                                                   nrow(simulated_population$predictors) )
simulated_population$consideration = generate_response(simulated_population$predictors,
                                                       logit.target.consideration$coefficients,
                                                       nrow(simulated_population$predictors) )

# Set simulation hyperparameters
N = 2500
Q = .80
reps = 100

###################### RUN SIMULATION FUNCTION
run_simulation <- function(N, Q, reps, target_gender, target_age, dependent_variable, kpi) {
  
  # For looped simulation, keep track of which value of Q we are currently on
  print(paste("Q:", Q))
  start_time = Sys.time()
  
  # Allocate memory for simulation results
  target_glm_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))))
  total_svyglm_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))))
  interaction_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))*2))
  
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
    nontarget_data = separate_predictors_responses(subsamples$nontarget[idx_rs_nontargets,])
    rs_nontargets$predictors = add_constant(nontarget_data$predictors)
    
    # Compile dependent variable (KPI)
    if (tolower(kpi) == "consideration") {
      rs_targets$kpi = simulated_population$consideration[idx_rs_targets]
      rs_nontargets$kpi = nontarget_data$consideration
    } else if (tolower(kpi) == "awareness") {
      rs_targets$kpi = simulated_population$awareness[idx_rs_targets]
      rs_nontargets$kpi = nontarget_data$awareness
    } else if (tolower(kpi) == "familiarity") {
      rs_targets$kpi = simulated_population$familiarity[idx_rs_targets]
      rs_nontargets$kpi = nontarget_data$familiarity
    } else {
      print("Invalid KPI specified, breaking the loop")
      break
    }
    
    # Combine the target and non-target data into a single matrix
    full_sample = list()
    full_sample$predictors = rbind(rs_targets$predictors,
                                   rs_nontargets$predictors)
    full_sample$kpi = append(rs_targets$kpi,
                             rs_nontargets$kpi)
    
    # Compute weights
    weights = compute_weights(Q,
                              CPS[target_age, target_gender],
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
    
    # Store results of current run
    target_glm_results[i,] = glm.target_audience$coefficients
    total_svyglm_results[i,] = svyglm.total_audience$coefficients
    interaction_results[i,] = glm.interaction_model$coefficients
    #anova_results[i] <- anova(logit.sim.no_interact.weighted,method = "Wald")$p
    
    # Keep track of which simulation run we are in
    if (i%%100 == 0) {
      current_time = Sys.time()
      elapsed = current_time - start_time
      print(paste("Currently at iteration:", i, "| Time elapsed:", round(elapsed, 2), attr(elapsed, "units")))
    }
    
  }
  li_results = list()
  li_results$glm_target_audience <- target_glm_results
  li_results$svyglm_total_audience <- total_svyglm_results
  li_results$glm_interaction_model <- interaction_results[,1:7] + interaction_results[,8:14]
  #li_results$anova_results <- anova_results
  return(li_results)
}