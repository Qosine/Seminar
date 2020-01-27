set.seed(200127)
library(mvtnorm)
library(simcausal)

## SETUP ##
load_data <- function(file_path) { NULL }

## EXPLANATORY VARIABLES / PREDICTORS ##
generate_predictors <- function(sample_size,
                                proportion_target_sample,
                                target_population,
                                nontarget_population) {
  
  N = sample_size; Q = proportion_target_sample
  target_sample = sample(target_population, (N*Q) )
  nontarget_sample = sample(nontarget_population, (N*(1-Q)) )
  
  total_sample = rbind(target_sample, nontarget_sample)
  return (total_sample)
}

## ESTIMATE PARAMETERS FOR DGP ##

estimate_params <- function(true_predictors, true_outcomes) {
  # logit_real_data <- glm( true_outcomes ~ true_predictors, family=binomial(link="logit") )
  # true_params <- logit_real_data$coefficients[,1]
  # return (true_params)
  NULL
}


## DEPENDENT VARIABLES / OUTCOMES ##

sigmoid_function <- function(x) 1/(1+exp(-x))

generate_outcomes <- function(predictors, parameters, sample_size) {
  Pr_success <- sigmoid_function( predictors%*%parameters ) # compute Pr(Y=1 | X=x)
  outcomes <- rbern( n=sample_size, prob=Pr_success ) # draw from corresponding Bernoulli distribution
  return (outcomes)
}


## MAIN SIMULATION ##

run_simulation <- function (N, Q) {
  
  
  
  targ_pred <- generate_predictors(N_targ, cat_prop_zero, cat_means, cat_sd, m_contact_corr)
  
}


N = 20
#Q = c(.1, .15, 2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8)
Q = .25
