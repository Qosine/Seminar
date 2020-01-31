set.seed(200127)
library(mvtnorm)
library(simcausal)
library(dplyr)

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

## DEPENDENT VARIABLES / OUTCOMES ##

sigmoid_function <- function(x) 1/(1+exp(-x))

generate_response <- function(predictors, parameters, sample_size) {
  Pr_success <- sigmoid_function( predictors%*%parameters ) # compute Pr(Y=1 | X=x)
  response <- rbern( n=sample_size, prob=Pr_success ) # draw from corresponding Bernoulli distribution
  return (response)
}

## MAIN SIMULATION ##

# Pathing
path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"
setwd(path)

# Load Pointlogic source data
source_data = read.csv("./cleaned_unified_sample.csv")

# Load simulated target population
load("./simulated_target_population.RData")

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

########## UP TO THIS NAME LINE, ALL PREDICTOR VARIABLE NAMES ARE PRESERVED
###################### BELOW THIS LINE WILL GO IN SIMULATION FUNCTION

N = 7500
Q = .60
reps = 100
familiarity_results = matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))*2)
awareness_results = matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))*2)
consideration_results = matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))*2)

for (i in 1:reps) {
  idx_rs_targets = sample( 1:nrow(simulated_population), N*Q )
  idx_rs_nontargets = sample( 1:nrow(subsamples$nontarget), (N*(1-Q)) )
  
  rs_targets = list()
  rs_targets$predictors = simulated_population[idx_rs_targets,]
  rs_targets$predictors = add_constant(rs_targets$predictors)
  rs_targets$familiarity = generate_response(rs_targets$predictors,
                                   logit.target.familiarity$coefficients,
                                   N*Q)
  rs_targets$awareness = generate_response(rs_targets$predictors,
                                             logit.target.awareness$coefficients,
                                             N*Q)
  rs_targets$consideration = generate_response(rs_targets$predictors,
                                             logit.target.consideration$coefficients,
                                             N*Q)
  
  
  rs_nontargets = separate_predictors_responses(subsamples$nontarget[idx_rs_nontargets,])
  rs_nontargets$predictors = add_constant(rs_nontargets$predictors)
  
  full_sample = list()
  full_sample$predictors = rbind(rs_targets$predictors,
                                 rs_nontargets$predictors)
  interaction = rbind(rs_targets$predictors,
                      matrix(0, nrow(rs_nontargets$predictors), ncol(rs_nontargets$predictors)))
  full_sample$predictors = cbind(full_sample$predictors, interaction)
  full_sample$familiarity = append(rs_targets$familiarity,
                                   rs_nontargets$familiarity)
  full_sample$awareness = append(rs_targets$awareness,
                                 rs_nontargets$awareness)
  full_sample$consideration = append(rs_targets$consideration,
                                     rs_nontargets$consideration)
  
  logit.simulation.familiarity <- glm(full_sample$familiarity
                                      ~ 0 + full_sample$predictors,
                                      family = binomial(link="logit"))
  logit.simulation.awareness <- glm(full_sample$awareness
                                      ~ 0 + full_sample$predictors,
                                      family = binomial(link="logit"))
  logit.simulation.consideration <- glm(full_sample$consideration
                                      ~ 0 + full_sample$predictors,
                                      family = binomial(link="logit"))
  
  coefficient_names = c("Intercept", "Audio", "Digital", "Program", "TV",
                        "VOD", "Youtube", "Target", "Target*Audio", "Target*Digital",
                        "Target*Program", "Target*TV", "Target*VOD", "Target*Youtube")
  
  names(logit.simulation.familiarity$coefficients) = coefficient_names
  names(logit.simulation.awareness$coefficients) = coefficient_names
  names(logit.simulation.consideration$coefficients) = coefficient_names
  
  familiarity_results[i,] = logit.simulation.familiarity$coefficients
  awareness_results[i,] = logit.simulation.awareness$coefficients
  consideration_results[i,] = logit.simulation.consideration$coefficients
  
  if (i %% 100 == 0){
    print(paste("Currently at iteration:", i))
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
