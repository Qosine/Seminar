set.seed(200127)
library(mvtnorm)
library(simcausal)
library(dplyr)
library(survey)

## SETUP ##
split_sample <- function(data,
                         target_gender_m=TRUE, # target audience M/F? M = TRUE
                         min_age=25, # min_age and max_age are INCLUSIVE
                         max_age=34) {
  
  if (target_gender_m==TRUE) {target_gender="male"} else {target_gender="female"}
  
  target_sample = data[ ( data["sd_gender"]==target_gender
                          & data["sd_age"]>=min_age
                          & data["sd_age"]<=max_age ), ]
  nontarget_sample = setdiff(data, target_sample)
  
  out = list(); out$target = target_sample; out$nontarget = nontarget_sample
  
  return (out)
}

## EXPLANATORY VARIABLES / PREDICTORS ##
separate_predictors_responses <- function(subsample) {
  
  # Extract columns of interest
  df_contacts = subsample[,93:182]
  v_familiarity = subsample[,22]
  v_awareness = subsample[,23]
  v_consideration = subsample[,24]
  
  # Sum across categories
  v_audiosum = rowSums(df_contacts[,1:5])
  v_digitalsum = rowSums(df_contacts[,6:12])
  v_programsum = df_contacts[,13]
  v_tvsum = rowSums(df_contacts[,14:81])
  v_vodsum = rowSums(df_contacts[,82:89])
  v_yousum = df_contacts[,90]
  
  # Return single argument with predictor and response values
  out = list()
  out$predictors = cbind(v_audiosum, v_digitalsum, v_programsum, v_tvsum,
                         v_vodsum, v_yousum)
  out$familiarity = v_familiarity
  out$awareness = v_awareness
  out$consideration = v_consideration

  return(out)
}

add_constant <- function(sample) {
  constant = rep( 1, max(nrow(sample), ncol(sample)) )
  return( cbind(constant, sample) )
}

rename_interaction_columns <- function(column_names) {
  return( paste("ia", column_names, sep="_"))
}

## DEPENDENT VARIABLES / OUTCOMES ##
sigmoid_function <- function(x) 1/(1+exp(-x))

generate_response <- function(predictors, parameters, sample_size) {
  Pr_success <- sigmoid_function( predictors%*%parameters ) # compute Pr(Y=1 | X=x)
  response <- rbern( n=sample_size, prob=Pr_success ) # draw from corresponding Bernoulli distribution
  return (response)
}

## WEIGHTS ##
compute_weights <- function(target_sample_proportion,
                            target_CPS_proportion,
                            no_target_obs,
                            no_nontarget_obs){
  
  target_weight = target_sample_proportion/target_CPS_proportion
  nontarget_weight = (1-target_sample_proportion)/(1-target_CPS_proportion)
  weights = append(rep(target_weight, no_target_obs),
                   rep(nontarget_weight, no_nontarget_obs) )
  return(weights)
}

## SURVEY RELATION ##
create_svyglm_inputs <- function(predictors,
                                 response,
                                 intercept_included=TRUE) {
  data = data.frame(cbind(response, predictors))
  predictor_names = paste(colnames(predictors), collapse = " + ")
  if (intercept_included==TRUE) {
    func = paste(deparse(substitute(response)), "~ 0 +", predictor_names)
  }
  else {func = paste(deparse(substitute(response)), "~", predictor_names)}
  
  
  out = list(); out$data = data; out$func = func
  return(out)
}

## EVALUATION ##

RMSE <- function(y, yhat) {(mean((y - yhat)^2))^.5}
Bias <- function(beta_true, beta_hat) {mean(beta_hat) - beta_true}


## FITTING TRUE MODELS ##

# Pathing
path = "~/Documents/Econometrie/Masters/Seminar Nielsen"
path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"
setwd(path)

# Load Pointlogic source data
source_data = read.csv("./cleaned_unified_sample.csv")

# Separate true data into predictors and responses
true_fullsample_variables = separate_predictors_responses(source_data)

# Split into true target and true non-target data
subsamples = split_sample(source_data)

# Separate predictors and responses for real data
true_target_variables = separate_predictors_responses(subsamples$target)
true_nontarget_variables = separate_predictors_responses(subsamples$nontarget)

# Fit logit coefficients of the true target data
logit.target.familiarity <- glm( true_target_variables$familiarity
                                 ~ true_target_variables$predictors,
                                 family=binomial(link="logit") )
logit.target.awareness <- glm( true_target_variables$awareness
                               ~ true_target_variables$predictors,
                               family=binomial(link="logit") )
logit.target.consideration <- glm( true_target_variables$consideration
                                   ~ true_target_variables$predictors,
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

# CPS weights
CPS <- rbind.data.frame(c(0.203195,0.10298,0.100214),
                        c(0.185959,0.092719,0.09324),
                        c(0.186438,0.091954,0.094484),
                        c(0.424408,0.195766,0.228643))
colnames(CPS) = c("Total", "Male", "Female")
rownames(CPS) = c("25-34", "35-44", "45-55", "55+")


# Load simulated target population, add column for constant and generate responses
load("./simulated_target_predictors.RData")
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
N = 7500
Q = .60
reps = 400

###################### RUN SIMULATION FUNCTION
run_simulation <- function(N, Q, reps, target_gender = "Male", target_age = "25-34"){
  
  # Allocate memory for simulation results
  unweighted_logit_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))*2))
  weighted_logit_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))*2))
  
  # Name columns of results df
  coefficient_names = c("Intercept", "Audio", "Digital", "Program", "TV",
                        "VOD", "Youtube", "Target", "Target*Audio", "Target*Digital",
                        "Target*Program", "Target*TV", "Target*VOD", "Target*Youtube")
  colnames(unweighted_logit_results) = coefficient_names
  colnames(weighted_logit_results) = coefficient_names
  
  
  for (i in 1:reps) {
    
    # Create index of random samples for target and non-target data
    idx_rs_targets = sample( 1:nrow(simulated_population$predictors), N*Q )
    idx_rs_nontargets = sample( 1:nrow(subsamples$nontarget), (N*(1-Q)) )
    
    # Compile random sample of targets
    rs_targets = list()
    rs_targets$predictors = simulated_population$predictors[idx_rs_targets,]
    rs_targets$familiarity = simulated_population$familiarity[idx_rs_targets]
    rs_targets$awareness = simulated_population$awareness[idx_rs_targets]
    rs_targets$consideration = simulated_population$consideration[idx_rs_targets]
    
    # Compile random sample of nontargets (need to manually add constant still)
    rs_nontargets = separate_predictors_responses(subsamples$nontarget[idx_rs_nontargets,])
    rs_nontargets$predictors = add_constant(rs_nontargets$predictors)
    
    # Combine the target and non-target data into a single matrix
    full_sample = list()
    full_sample$predictors = rbind(rs_targets$predictors,
                                   rs_nontargets$predictors)
    full_sample$familiarity = append(rs_targets$familiarity,
                                     rs_nontargets$familiarity)
    full_sample$awareness = append(rs_targets$awareness,
                                   rs_nontargets$awareness)
    full_sample$consideration = append(rs_targets$consideration,
                                       rs_nontargets$consideration)
    
    # Compute weights
    #target_CPS_weight = CPS[target_gender, target_age]
    weights = compute_weights(Q,
                              CPS["25-34", "Male"],
                              nrow(rs_targets$predictors),
                              nrow(rs_nontargets$predictors))
    
    # To include target-group specific parameters we need to augment the design matrix
    # We do this by adding interaction variables. For the target sample this means
    # keeping the original data, for the non-target sample we must replace by zero
    interaction = rbind(rs_targets$predictors,
                        matrix(0,
                               nrow(rs_nontargets$predictors),
                               ncol(rs_nontargets$predictors)))
    colnames(interaction) = rename_interaction_columns(colnames(interaction))
    full_sample$predictors = cbind(full_sample$predictors, interaction)
    
    # Fit regular logit
    logit.simulation.unweighted <- glm(full_sample$familiarity
                                        ~ 0 + full_sample$predictors,
                                        family = binomial(link="logit"))
    
    # Fit weighted logit
    svy_inputs = create_svyglm_inputs(full_sample$predictors, full_sample$familiarity)
    design_func <- svydesign(id = ~1,
                             data = svy_inputs$data,
                             weight = weights)
    logit.simulation.weighted <- svyglm(formula = svy_inputs$func,
                                        design =  design_func,
                                        family = "quasibinomial")
    
    # Store results of current run
    unweighted_logit_results[i,] = logit.simulation.unweighted$coefficients
    weighted_results[i,] = logit.simulation.weighted$coefficients
    
    # Keep track of which simulation run we are in
    if (i %% 100 == 0) { print(paste("Currently at iteration:", i)) }
    
  }
  li_results = list()
  li_results$unweighted <- unweighted_logit_results
  li_results$weighted <- weighted_logit_results
  return(li_results)
}

output_results <- function(li_results){
  familiarity_results <-li_results$fam
  awareness_results <- li_results$awa
  consideration_results <-li_results$con
  #
  familiarity_results_target = familiarity_results[,1:7] + familiarity_results[,8:14]
  awareness_results_target = awareness_results[,1:7] + awareness_results[,8:14]
  consideration_results_target = consideration_results[,1:7] + consideration_results[,8:14]
  
  (abs(logit.target.familiarity$coefficients - colMeans(familiarity_results_target) ))/logit.target.familiarity$coefficients
  abs(logit.target.awareness$coefficients - colMeans(awareness_results_target) )
  abs(logit.target.consideration$coefficients - colMeans(consideration_results_target) )
  
  abs(logit.nontarget.familiarity$coefficients - colMeans(familiarity_results[,1:7]))/logit.nontarget.familiarity$coefficients
  
  #
  colMeans(familiarity_results_target)
  colMeans(awareness_results_target)
  colMeans(consideration_results_target)
  
}

#run sim & output results:
li_results <- run_simulation()
output_results(li_results)
rejection_percentage_true_pars()


## Function that will output the percentage of true parameter rejections of the simulation
rejection_percentage_true_pars <- function(results=li_results, true_parameter_estimate = ){
  target_parameters = colmeans(li_results$)
    rej_count=0
  for(i in nrow(target)){
    p_val_ttest = t.test(target ,alternative = "two.sided",mu=true_parameter_estimate)$p.value
    if (p_val_ttest<.05) {
      rej_count = rej_count+1
    }  
}

}
# # Simulated target group
# sim_target_variables = list()
# #   1) Separate predictors and responses for simulated (target) group
# sim_target_variables$predictors = add_constant(simulated_population)
# #   2) Fit outcomes
# sim_target_variables$familiarity = generate_response(sim_target_variables$predictors,
#                                                      logit.target.familiarity$coefficients,
#                                                      nrow(simulated_population))
# sim_target_variables$awareness = generate_response(sim_target_variables$predictors,
#                                                    logit.target.awareness$coefficients,
#                                                    nrow(simulated_population))
# sim_target_variables$consideration = generate_response(sim_target_variables$predictors,
#                                                        logit.target.consideration$coefficients,
#                                                        nrow(simulated_population))
# 
# # Create full sample of simulated targets and real data nontargets
# sim_fullsample_variables = list()
# sim_fullsample_variables$predictors = rbind(sim_target_variables$predictors,
#                                             true_nontarget_variables$predictors)
# sim_fullsample_variables$familiarity = append(sim_target_variables$familiarity,
#                                               true_nontarget_variables$familiarity)
# sim_fullsample_variables$awareness = append(sim_target_variables$awareness,
#                                             true_nontarget_variables$awareness)
# sim_fullsample_variables$consideration = append(sim_target_variables$consideration,
#                                                 true_nontarget_variables$consideration)
#   
# 
# 
# 
# # Create nontarget subsample of appropriate size
# idx = sample(1:nrow(subsamples$nontarget),
#              (nrow(source_data)-nrow(simulated_population)) )
# true_nontarget_variables = separate_predictors_responses(subsamples$nontarget[idx,])
# true_nontarget_variables$predictors = add_constant(true_nontarget_variables$predictors)
# 
# # Appending design matrix with interaction between predictors and target dummy
# interaction = rbind(unname(sim_target_variables$predictors),
#                     matrix(0,
#                            nrow(true_nontarget_variables$predictors),
#                            ncol(true_nontarget_variables$predictors)))
# 
# sim_fullsample_variables$predictors = cbind(sim_fullsample_variables$predictors,
#                                             interaction)
# 
