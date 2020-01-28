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

## DEPENDENT VARIABLES / OUTCOMES ##

sigmoid_function <- function(x) 1/(1+exp(-x))

generate_response <- function(predictors, parameters, sample_size) {
  Pr_success <- sigmoid_function( predictors%*%parameters ) # compute Pr(Y=1 | X=x)
  response <- rbern( n=sample_size, prob=Pr_success ) # draw from corresponding Bernoulli distribution
  return (response)
}

## MAIN SIMULATION ##

run_simulation <- function (input) { NULL }



## TEST SCRIPT (For 30 January meeting)
path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"
setwd(path)

# Load and separate Pointlogic source data
source_data = read.csv("./cleaned_unified_sample.csv")
true_fullsample_variables = separate_predictors_responses(source_data)
subsamples = split_sample(source_data)
true_target_variables = separate_predictors_responses(subsamples$target)

# Load and manipulate simulated target population
load("./simulated_target_population.RData")
constant = rep( 1, max(nrow(simulated_population), ncol(simulated_population)) )
sim_target_variables = list()
sim_target_variables$predictors = cbind(constant, simulated_population)

# Fit KPI logits with true target data
logit.target.familiarity <- glm( true_target_variables$familiarity
                                 ~ true_target_variables$predictors,
                                 family=binomial(link="logit") )
logit.target.awareness <- glm( true_target_variables$awareness
                               ~ true_target_variables$predictors,
                               family=binomial(link="logit") )
logit.target.consideration <- glm( true_target_variables$consideration
                                   ~ true_target_variables$predictors,
                                   family=binomial(link="logit") )

# Fit outcomes for simulated target sample
sim_target_variables$familiarity = generate_response(sim_target_variables$predictors,
                                                     logit.target.familiarity$coefficients,
                                                     nrow(simulated_population))
sim_target_variables$awareness = generate_response(sim_target_variables$predictors,
                                                   logit.target.awareness$coefficients,
                                                   nrow(simulated_population))
sim_target_variables$consideration = generate_response(sim_target_variables$predictors,
                                                       logit.target.consideration$coefficients,
                                                       nrow(simulated_population))

# Create dataframe to compare proportions where KPI = 1
familiarity_means = c(mean(sim_target_variables$familiarity),
                         mean(true_target_variables$familiarity),
                         mean(true_fullsample_variables$familiarity))
awareness_means = c(mean(sim_target_variables$awareness),
                    mean(true_target_variables$awareness),
                    mean(true_fullsample_variables$awareness))
consideration_means = c(mean(sim_target_variables$consideration),
                        mean(true_target_variables$consideration),
                        mean(true_fullsample_variables$consideration))
comparison_df = rename(data.frame(rbind(familiarity_means,
                                        awareness_means,
                                        consideration_means
                                        )
                                  ),
                       "Sim. Targ" = X1,
                       "True Targ" = X2,
                       "True Full Smpl" = X3
                       )

round(comparison_df, 3)
