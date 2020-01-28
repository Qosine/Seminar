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
create_predictors_responses <- function(subsample) {
  
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


## ESTIMATE PARAMETERS FOR DGP ##

estimate_params <- function(true_predictors, true_outcomes) {
  logit <- glm( true_outcomes ~ true_predictors, family=binomial(link="logit") )
  true_params <- logit_real_data$coefficients[,1]
  return (true_params)
}


## DEPENDENT VARIABLES / OUTCOMES ##

sigmoid_function <- function(x) 1/(1+exp(-x))

generate_outcomes <- function(predictors, parameters, sample_size) {
  Pr_success <- sigmoid_function( predictors%*%parameters ) # compute Pr(Y=1 | X=x)
  outcomes <- rbern( n=sample_size, prob=Pr_success ) # draw from corresponding Bernoulli distribution
  return (outcomes)
}


## TEST SCRIPT
path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"
setwd(path)
source_data = read.csv("./cleaned_unified_sample.csv")
load("./simulated_target_population.RData")
#simulated_population = simulated_population[1:1000,]
constant = rep( 1, max(nrow(simulated_population), ncol(simulated_population)) )


sim_target_variables = list()
sim_target_variables$predictors = cbind(constant, simulated_population)
subsamples = split_sample(source_data)

target_sample = subsamples$target
true_target_variables = create_predictors_responses(target_sample)
logit.target.familiarity <- glm( true_target_variables$familiarity
                                 ~ true_target_variables$predictors,
                                 family=binomial(link="logit") )
logit.target.awareness <- glm( true_target_variables$awareness
                               ~ true_target_variables$predictors,
                               family=binomial(link="logit") )
logit.target.consideration <- glm( true_target_variables$consideration
                                   ~ true_target_variables$predictors,
                                   family=binomial(link="logit") )

sim_target_variables$familiarity = generate_outcomes(sim_target_variables$predictors,
                                                     logit.target.familiarity$coefficients,
                                                     nrow(simulated_population))
sim_target_variables$awareness = generate_outcomes(sim_target_variables$predictors,
                                                   logit.target.awareness$coefficients,
                                                   nrow(simulated_population))
sim_target_variables$consideration = generate_outcomes(sim_target_variables$predictors,
                                                       logit.target.consideration$coefficients,
                                                       nrow(simulated_population))

print( paste("True mean of familiarity:", mean(true_target_variables$familiarity)) )
print( paste("True mean of awareness:", mean(true_target_variables$awareness)) )
print( paste("True mean of consideration:", mean(true_target_variables$consideration)) )

print( paste("Simulated mean of familiarity:", mean(sim_target_variables$familiarity)) )
print( paste("Simulated mean of awareness:", mean(sim_target_variables$awareness)) )
print( paste("Simulated mean of consideration:", mean(sim_target_variables$consideration)) )






## MAIN SIMULATION ##

run_simulation <- function (N, Q) {
  
  
  
  targ_pred <- generate_predictors(N_targ, cat_prop_zero, cat_means, cat_sd, m_contact_corr)
  
}


#Q = c(.1, .15, 2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8)