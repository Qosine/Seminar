set.seed(200127)

#install.packages("pROC")
library(mvtnorm)
library(simcausal)
library(dplyr)
library(survey)
library(pROC)
library(ddpcr)

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

# Helper function that converts strings of form "male_25_34" to "Male" and "25-34"
separate_demographic_info <- function(input_string) {
  if (tolower(substring(input_string, 1, 1))=='f') {
    gender = "Female"; min_age = substring(input_string, 8, 9); max_age = substring(input_string, 11, 12)
  } else {
    gender = "Male"; min_age = substring(input_string, 6, 7); max_age = substring(input_string, 9, 10)
  }
  out = list(); out$gender = gender; out$age = paste(min_age, max_age, sep="-")
  return(out)
}

# Sample simulated data
sample_data <- function(target_group, nontarget_groups, target_predictors, nontarget_predictors, N, Q,
                        target_gender, target_age) {
  
  out = list(); 
  
  # Draw target train and test samples
  # For training set size should be N*Q, for test set we use CPS ratio
  out$target_train = sample( 1:nrow(target_predictors), N*Q )
  out$target_test = sample( setdiff(1:nrow(target_predictors), out$target_train),
                            floor(N*CPS[target_age, target_gender]))
  out$nontarget_train = out$nontarget_test = c()
  
  # Normalise sample sizes of nontarget groups for fact CPS proportions assume target group is present
  normalising_constant = 1 - CPS[target_age, target_gender]
  
  for (group in nontarget_groups) {
    group_info = separate_demographic_info(group)
    CPS_proportion = CPS[group_info$age, group_info$gender]
    
    # We encounter a problem here when using CPS proportions, since we might end up with a non-integer
    # number of index draws. To account for this we use the floor function for all groups except the last.
    # Then we can make sure the total number of non-target observations adds up to N*(1-Q) by adding the difference
    if (group != nontarget_groups[length(nontarget_groups)]) {
      # Sample indices as if all target group data was in separate matrices
      idx_train = sample(1:nrow(get(group)),
                         floor(N*(1-Q)*CPS_proportion/normalising_constant))
      idx_test = sample(setdiff(1:nrow(get(group)),idx_train),
                        floor(N*CPS_proportion))
    } else {
      # Sample indices as if all target group data was in separate matrices
      train_obs_remaining = N*(1-Q) - length(out$nontarget_train)
      test_obs_remaining = N - length(out$target_test) - length(out$nontarget_test)
      idx_train = sample(1:nrow(get(group)), train_obs_remaining)
      idx_test = sample(setdiff(1:nrow(get(group)),idx_train), test_obs_remaining)
    }
    
    # Account for the fact that all nontarget data is placed in 1 large matrix of 15k per group
    # Move row numbers ahead by appropriate multiple of 15k (subtract 1 since we start at multiple 0)
    idx_train = idx_train + nrow(get(group))*(match(group, nontarget_groups)-1)
    idx_test = idx_test + nrow(get(group))*(match(group, nontarget_groups)-1)
    
    out$nontarget_train = append(out$nontarget_train, idx_train)
    out$nontarget_test = append(out$nontarget_train, idx_test)
  }
  return(out)
  
}

add_constant <- function(sample) {
  constant = rep( 1, max(nrow(sample), ncol(sample)) )
  return( cbind(constant, sample) )
}

rename_interaction_columns <- function(column_names) {
  return( paste("ia", column_names, sep="_"))
}

add_interaction_variables <- function(full_sample_predictors,
                                      target_predictors,
                                      nontarget_predictors) {
  interaction = rbind(target_predictors,
                      matrix(0, nrow(nontarget_predictors), ncol(nontarget_predictors)))
  colnames(interaction) = rename_interaction_columns(colnames(interaction))
  return(cbind(full_sample_predictors, interaction))
}

## DEPENDENT VARIABLES / OUTCOMES ##
sigmoid_function <- function(x) 1/(1+exp(-x))

generate_response <- function(predictors, parameters, sample_size) {
  Pr_success <- sigmoid_function( predictors%*%parameters ) # compute Pr(Y=1 | X=x)
  response <- rbern( n=sample_size, prob=Pr_success ) # draw from corresponding Bernoulli distribution
  return (as.numeric(response))
}

predict_response <- function(predictors, parameters, threshold) {
  yhat = sigmoid_function( predictors%*%parameters )
  yhat[yhat<threshold] = 0
  yhat[yhat>threshold] = 1
  return(as.numeric(yhat))
}

## WEIGHTS ##
compute_weights <- function(target_sample_proportion,
                            target_CPS_proportion,
                            no_target_obs,
                            no_nontarget_obs){
  
  target_weight = target_CPS_proportion/target_sample_proportion
  nontarget_weight = (1-target_CPS_proportion)/(1-target_sample_proportion)
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
    func = paste("response ~ 0 +", predictor_names)
  } else {
    func = paste("response ~", predictor_names)
  }
  out = list(); out$data = data; out$func = func
  return(out)
}

## EVALUATION ##

RMSE <- function(y, yhat) {(mean((y - yhat)^2))^.5}
Bias <- function(beta_true, beta_hat) {colMeans(beta_hat) - beta_true}


## FITTING TRUE MODELS ##
compute_population_params <- function(target_gender = "Male", target_age = "25-34") {
  target_CPS_weight = CPS[target_age, target_gender]
  population_betas  = logit.target.consideration$coefficients*target_CPS_weight + logit.nontarget.consideration$coefficients*(1-target_CPS_weight)
  return(population_betas)
}

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
rownames(CPS) = c("25-34", "35-44", "45-54", "55-99")

# Construct population parameters as linear combinations of target and non-target groups
true_population_params = compute_population_params()


# Load simulated demographic subgroups

female_25_34 = readRDS("./female_25_34.Rds")
female_35_44 = readRDS("./female_35_44.RDS")
female_45_54 = readRDS("./female_45_54.RDS")
female_55_99 = readRDS("./female_55_99.RDS")
male_25_34 = readRDS("./male_25_34.RDS")
male_35_44 = readRDS("./male_35_44.RDS")
male_45_54 = readRDS("./male_45_54.RDS")
male_55_99 = readRDS("./male_55_99.RDS")

# Set simulation hyperparameters
N = 7500
Q = .80
reps = 25

###################### RUN SIMULATION FUNCTION
run_simulation <- function(N, Q, reps, target_gender = "Male", target_min_age = 25, target_max_age = 34){
  
  target_gender = "Male"; target_min_age = 25; target_max_age = 34
  
  
  # Using inputs, create some strings needed later
  target_group = paste( tolower(target_gender), toString(target_min_age), toString(target_max_age), sep="_" )
  target_age = paste(target_min_age, target_max_age, sep="-")
  all_demographic_groups = list("female_25_34", "female_35_44", "female_45_54", "female_55_99",
                                "male_25_34", "male_35_44", "male_45_54", "male_55_99")
  nontarget_groups = setdiff(all_demographic_groups, target_group)
  
  # Compile simulated target data; 15k observations
  target_population = list()
  target_population$predictors = add_constant( get(target_group) )
  target_population$consideration = generate_response(target_population$predictors,
                                                      logit.target.consideration$coefficients,
                                                      nrow(target_population$predictors))
  
  # Compile simulated nontarget data
  nontarget_population = list(); nontarget_population$predictors = c()
  for (group in nontarget_groups) {
    nontarget_population$predictors = rbind( nontarget_population$predictors, add_constant(get(group)) )
  }
  nontarget_population$consideration = generate_response(nontarget_population$predictors,
                                                         logit.nontarget.consideration$coefficients,
                                                         nrow(nontarget_population$predictors))
  
  # Allocate memory for simulation results
  unweighted_population_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))))
  weighted_population_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))))
  unweighted_subsample_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))*2))
  auc_results = data.frame(matrix(0, reps, 3))
  
  # Name columns of results df
  coefficient_names = c("Intercept", "Audio", "Digital", "Program", "TV",
                        "VOD", "Youtube", "Target", "Target*Audio", "Target*Digital",
                        "Target*Program", "Target*TV", "Target*VOD", "Target*Youtube")
  auc_colnames = c("unweighted_population", "weighted_population", "with_interaction")
  colnames(unweighted_population_results) = coefficient_names[1:(length(coefficient_names)/2)]
  colnames(weighted_population_results) = coefficient_names[1:(length(coefficient_names)/2)]
  colnames(unweighted_subsample_results) = coefficient_names
  colnames(auc_results) = auc_colnames
  
  for (i in 1:reps) {
    
    indices = sample_data(target_group,
                          nontarget_groups,
                          target_population$predictors,
                          nontarget_population$predictors,
                          N, Q, target_gender, target_age)
    
    # Compile target samples
    target_train = list(); target_test = list()
    target_train$predictors = target_population$predictors[indices$target_train,]
    target_train$consideration = target_population$consideration[indices$target_train]
    target_test$predictors = target_population$predictors[indices$target_test,]
    target_test$consideration = target_population$consideration[indices$target_test]
    
    # Compile nontarget samples
    nontarget_train = list(); nontarget_test = list()
    nontarget_train$predictors = nontarget_population$predictors[indices$nontarget_train,]
    nontarget_train$consideration = nontarget_population$consideration[indices$nontarget_train]
    nontarget_test$predictors = nontarget_population$predictors[indices$nontarget_test,]
    nontarget_test$consideration = nontarget_population$consideration[indices$nontarget_test]
    
    # Combine the target and non-target data into a single matrix
    train_sample = list(); test_sample = list()
    train_sample$predictors = rbind(target_train$predictors, nontarget_train$predictors)
    train_sample$consideration = append(target_train$consideration, nontarget_train$consideration)
    test_sample$predictors = rbind(target_test$predictors, nontarget_test$predictors)
    test_sample$consideration = append(target_test$consideration, nontarget_test$consideration)
    
    # Compute weights
    #target_CPS_weight = CPS[target_gender, target_age]
    weights = compute_weights(Q,
                              CPS[target_age, target_gender],
                              nrow(target_train$predictors),
                              nrow(nontarget_train$predictors))
    
    # To include target-group specific parameters we need to augment the design matrix
    # We do this by adding interaction variables. For the target sample this means
    # keeping the original data, for the non-target sample we must replace by zero
    train_sample_w_interact = test_sample_w_interact = list()
    train_sample_w_interact$predictors = add_interaction_variables(train_sample$predictors,
                                                                   target_train$predictors,
                                                                   nontarget_train$predictors)
    test_sample_w_interact$predictors = add_interaction_variables(test_sample$predictors,
                                                                  target_test$predictors,
                                                                  nontarget_test$predictors)
    train_sample_w_interact$consideration = train_sample$consideration
    test_sample_w_interact$consideration = test_sample$consideration
    
    # Fit regular logit for model without interactions
    logit.sim.no_interact.unweighted <- glm(train_sample$consideration
                                            ~ 0 + train_sample$predictors,
                                            family = binomial(link="logit"))
    
    # Fit weighted logit for model without interactions
    svy_inputs = create_svyglm_inputs(train_sample$predictors, train_sample$consideration)
    design_func <- svydesign(id = ~1, data = svy_inputs$data, weight = weights)
    logit.sim.no_interact.weighted <- svyglm(formula = svy_inputs$func,
                                             design = design_func,
                                             family = "quasibinomial")
    
    
    # Fit unweighted logit for model with interactions
    logit.sim.interact.unweighted <-  glm(train_sample_w_interact$consideration
                                          ~ 0 + train_sample_w_interact$predictors,
                                          family = binomial(link="logit"))
    
    # Fit weighted logit for model with interactions
    svy_inputs = create_svyglm_inputs(train_sample_w_interact$predictors, train_sample$consideration)
    design_func <- svydesign(id = ~1, data = svy_inputs$data, weight = weights)
    logit.sim.interact.weighted <- svyglm(formula = svy_inputs$func,
                                          design =  design_func,
                                          family = "quasibinomial")
    
    # Make predictions; use the proportion of 1s 
    predictions = list()
    #threshold = 0.5
    threshold = mean(test_sample$consideration)
    predictions$no_interact_unweighted = predict_response(test_sample$predictors,
                                                          logit.sim.no_interact.unweighted$coefficients,
                                                          threshold)
    predictions$no_interact_weighted = predict_response(test_sample$predictors,
                                                        logit.sim.no_interact.weighted$coefficients,
                                                        threshold)
    predictions$w_interact = predict_response(test_sample_w_interact$predictors,
                                              logit.sim.interact.unweighted$coefficients,
                                              threshold)
    
    # Compute AUC metric - run this using quiet() to avoid repeated (extremely annoying)
    # print function that is hardcoded in auc()
    quiet(auc_results[i,] <- c(auc(response = test_sample$consideration,
                                  predictor = predictions$no_interact_unweighted),
                              auc(response = test_sample$consideration,
                                  predictor = predictions$no_interact_weighted),
                              auc(response = test_sample$consideration,
                                  predictor = predictions$w_interact)))
    
    # Store results of current run
    unweighted_population_results[i,] = logit.sim.no_interact.unweighted$coefficients
    weighted_population_results[i,] = logit.sim.no_interact.weighted$coefficients
    unweighted_subsample_results[i,] = logit.sim.interact.unweighted$coefficients
    #anova_results[i] <- anova(logit.sim.no_interact.weighted,method = "Wald")$p
    
    # Keep track of which simulation run we are in
    if (i%%25 == 0) { print(paste("Currently at iteration:", i)) }
    
  }
  li_results = list()
  li_results$glm_results <- unweighted_population_results
  li_results$svyglm_results <- weighted_population_results
  li_results$interaction_model <- unweighted_subsample_results
  li_results$interaction_model.target <- unweighted_subsample_results[,1:7] + unweighted_subsample_results[,8:14]
  li_results$interaction_model.population <- (unweighted_subsample_results[,1:7]*(1-CPS[target_age, target_gender])
                                              + unweighted_subsample_results[,8:14]*CPS[target_age, target_gender])
  #li_results$anova_results <- anova_results
  li_results$auc <- auc_results
  return(li_results)
}

test = run_simulation(N, Q, reps, target_gender = "Male", target_min_age = 25, target_max_age = 34)

test<-matrix(data=0,nrow = 9,ncol = 3)
reps=50
for(N_s in 1:3){
  N_s2 = N_s*2500
  for(l in 1:9){
    R=l/10
    test[l,N_s] <- sum(ifelse((run_simulation(1000,0.9,reps, target_gender = "Male", target_age = "55+")$anova_results<0.05) == FALSE,1,0))/reps
  }
}
colnames(test) <- c(2500,5000,7500)
rownames(test) <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)


test = run_simulation(N,Q,reps)

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


#plot for procentual hit rate for weighted and unweighted glm and estimator plots
plot_weighted <- matrix(data = 0, nrow=100, ncol = 7)
plot_unweighted <- matrix(data = 0, nrow=100, ncol = 7)
N=7500
pred_unweighted <- vector()
pred_weighted <- vector()
pred_weighted_target <- vector()
strat <- ifelse(source_data$sd_age <=34 & source_data$sd_age >=25 & source_data$sd_gender == "male", 1,0)
for(i in 1:100){
  Q = i/100 # is percent target audience
  
  idx_rs_targets = sample( 1:nrow(simulated_population$predictors), N*Q )
  idx_rs_nontargets = sample( 1:nrow(subsamples$nontarget), (N*(1-Q)) )
  rs_targets$predictors = simulated_population$predictors[idx_rs_targets,]
  rs_targets$familiarity = simulated_population$familiarity[idx_rs_targets]
  rs_targets$awareness = simulated_population$awareness[idx_rs_targets]
  rs_targets$consideration = simulated_population$consideration[idx_rs_targets]
  rs_nontargets = separate_predictors_responses(subsamples$nontarget[idx_rs_nontargets,])
  rs_nontargets$predictors = add_constant(rs_nontargets$predictors)
  full_sample$predictors = rbind(rs_targets$predictors[,2:7],
                                 rs_nontargets$predictors[,2:7])
  full_sample$familiarity = append(rs_targets$familiarity,
                                   rs_nontargets$familiarity)
  full_sample$awareness = append(rs_targets$awareness,
                                 rs_nontargets$awareness)
  full_sample$consideration = append(rs_targets$consideration,
                                     rs_nontargets$consideration)
  weights = compute_weights(Q,
                            CPS["25-34", "Male"],
                            nrow(rs_targets$predictors),
                            nrow(rs_nontargets$predictors))
  
  # Fit regular logit
  logit.simulation.unweighted <- glm(full_sample$awareness
                                     ~ full_sample$predictors,
                                     family = binomial(link="logit"))
  
  # Fit weighted logit
  svy_inputs = create_svyglm_inputs(full_sample$predictors, full_sample$awareness)
  design_func <- svydesign(id = ~1,
                           data = svy_inputs$data,
                           weight = weights)
  logit.simulation.weighted <- svyglm(formula = svy_inputs$func,
                                      design =  design_func,
                                      family = "quasibinomial")
  pred_weighted[i] <- sum(ifelse(true_fullsample_variables$awareness == ifelse(predict.glm(logit.simulation.weighted, newdata=as.data.frame(true_fullsample_variables$predictors))>=.5,1,0),1,0))/N
  pred_weighted_target[i] <- sum(ifelse(subset(true_fullsample_variables$awareness, strat==1) == ifelse(predict.glm(logit.simulation.weighted, newdata=as.data.frame(subset(true_fullsample_variables$predictors,strat == 1)))>=.5,1,0),1,0))/791
  pred_unweighted[i] <- sum(ifelse(true_fullsample_variables$awareness == ifelse(predict.glm(logit.simulation.unweighted, newdata=as.data.frame(true_fullsample_variables$predictors))>=.5,1,0),1,0))/N
  #pred_weighted_target[i] <- sum(ifelse(full_sample$familiarity == ifelse(predict.glm(logit.simulation.weighted)>=.5,1,0),1,0))/(Q*N)
  #pred_weighted_nontarget[i] <- sum(ifelse(full_sample$familiarity == ifelse(predict.glm(logit.simulation.weighted)>=.5,1,0),1,0))/((1-Q)*N)
  #pred_weighted_nontarget[i] <- sum(ifelse(full_sample$familiarity == ifelse(predict.glm(logit.simulation.weighted)>=.5,1,0),1,0))/((1-Q)*N)
  
  #plot_unweighted[i,] = t(unname(logit.simulation.unweighted$coefficients))
  #plot_weighted[i,] = t(unname(logit.simulation.weighted$coefficients))
}
plot(cbind(c(1:100),pred_unweighted))
plot(cbind(c(1:100),pred_weighted))
plot(cbind(c(1:100),pred_weighted_target))

