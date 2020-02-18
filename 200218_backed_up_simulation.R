set.seed(200127)
library(mvtnorm)
library(simcausal)
library(dplyr)
library(survey)
library(ggplot2)

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
pctBias <- function(beta_true, beta_hat) {abs((colMeans(beta_hat) - beta_true)/beta_true)}

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
rownames(CPS) = c("25-34", "35-44", "45-55", "55+")

true_population_params = compute_population_params()

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
Q = .80
reps = 100

###################### RUN SIMULATION FUNCTION
run_simulation <- function(N, Q, reps, target_gender = "Male", target_age = "25-34"){
  
  # Allocate memory for simulation results
  unweighted_population_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))))
  weighted_population_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))))
  unweighted_subsample_results = data.frame(matrix(0, reps, ncol(add_constant(true_fullsample_variables$predictors))*2))
  
  # Name columns of results df
  coefficient_names = c("Intercept", "Audio", "Digital", "Program", "TV",
                        "VOD", "Youtube", "Target", "Target*Audio", "Target*Digital",
                        "Target*Program", "Target*TV", "Target*VOD", "Target*Youtube")
  colnames(unweighted_population_results) = coefficient_names[1:(length(coefficient_names)/2)]
  colnames(weighted_population_results) = coefficient_names[1:(length(coefficient_names)/2)]
  colnames(unweighted_subsample_results) = coefficient_names
  
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
                              CPS[target_age, target_gender],
                              nrow(rs_targets$predictors),
                              nrow(rs_nontargets$predictors))
    
    # To include target-group specific parameters we need to augment the design matrix
    # We do this by adding interaction variables. For the target sample this means
    # keeping the original data, for the non-target sample we must replace by zero
    full_sample_w_interact = list()
    interaction = rbind(rs_targets$predictors,
                        matrix(0,
                               nrow(rs_nontargets$predictors),
                               ncol(rs_nontargets$predictors)))
    colnames(interaction) = rename_interaction_columns(colnames(interaction))
    full_sample_w_interact$predictors = cbind(full_sample$predictors, interaction)
    full_sample_w_interact$consideration = full_sample$consideration
    
    # Fit regular logit for model without interactions
    logit.sim.no_interact.unweighted <- glm(rs_targets$consideration
                                            ~ 0 + rs_targets$predictors,
                                            family = binomial(link="logit"))
    
    # Fit weighted logit for model without interactions
    svy_inputs = create_svyglm_inputs(full_sample$predictors, full_sample$consideration)
    design_func <- svydesign(id = ~1,
                             data = svy_inputs$data,
                             weight = weights)
    logit.sim.no_interact.weighted <- svyglm(formula = svy_inputs$func,
                                             design =  design_func,
                                             family = "quasibinomial")
    
    # Fit unweighted logit for model with interactions
    logit.sim.interact.unweighted <-  glm(full_sample_w_interact$consideration
                                          ~ 0 + full_sample_w_interact$predictors,
                                          family = binomial(link="logit"))
    
    # Fit weighted logit for model with interactions
    svy_inputs = create_svyglm_inputs(full_sample_w_interact$predictors, full_sample$consideration)
    design_func <- svydesign(id = ~1,
                             data = svy_inputs$data,
                             weight = weights)
    logit.sim.interact.weighted <- svyglm(formula = svy_inputs$func,
                                          design =  design_func,
                                          family = "quasibinomial")
    
    # Store results of current run
    unweighted_population_results[i,] = logit.sim.no_interact.unweighted$coefficients
    weighted_population_results[i,] = logit.sim.no_interact.weighted$coefficients
    unweighted_subsample_results[i,] = logit.sim.interact.unweighted$coefficients
    #anova_results[i] <- anova(logit.sim.no_interact.weighted,method = "Wald")$p
    
    # Keep track of which simulation run we are in
    if (i%%100 == 0) { print(paste("Currently at iteration:", i)) }
    
  }
  li_results = list()
  li_results$unweighted_population <- unweighted_population_results
  li_results$weighted_population <- weighted_population_results
  li_results$unweighted_target <- unweighted_subsample_results[,1:7] + unweighted_subsample_results[,8:14]
  #li_results$anova_results <- anova_results
  return(li_results)
}

target_proportions = 10*(1:9)
for (prop in target_proportions) {
  cat(paste("\nRunning simulation for Q =", prop, "\n"))
  assign(paste("Q", prop, sep=""), run_simulation(N, Q = prop/100, reps = 50))
}

df_rows = paste("Q", target_proportions, sep = "")
df_cols = c("Intercept", "Audio", "Digital", "Program", "TV", "VOD", "Youtube")
targ_results_df = data.frame(matrix(0, length(df_rows), length(df_cols)))
rownames(targ_results_df) = df_rows; colnames(targ_results_df) = df_cols
total_results_df = targ_results_df

for (prop in target_proportions) {
  varname = paste("Q", prop, sep="")
  estimates = get(varname)
  targ_bias = pctBias(logit.target.consideration$coefficients, estimates$unweighted_population)
  total_bias = pctBias(true_population_params, estimates$weighted_population)
  targ_results_df[varname,] = targ_bias
  total_results_df[varname,] = total_bias
}

for (var in df_cols) {
  assign(paste(var, "df", sep="_"),
         data.frame(bias = append(targ_results_df[,match(var,df_cols)],
                                  total_results_df[,match(var,df_cols)]),
                    index=target_proportions,
                    Audience=rep(c("target", "total"),each=length(df_rows))))
}


# Intercept plot
(ggplot(data = Intercept_df, aes(x=index, y=bias*100, colour=Audience))
  + geom_line() + geom_point() + ylim(0, 5)
  + labs(x="Proportion target observations (%)",
         y="Bias (% of true parameter)",
         title="Bias of parameter estimates: Intercept")
  + theme(plot.title = element_text(hjust = 0.5)))

# Audio
(ggplot(data = Audio_df, aes(x=index, y=bias*100, colour=Audience))
  + geom_line() + geom_point() + ylim(0, 50)
  + labs(x="Proportion target observations (%)",
         y="Bias (% of true parameter)",
         title="Bias of parameter estimates: Audio")
  + theme(plot.title = element_text(hjust = 0.5)))

# Digital
(ggplot(data = Digital_df, aes(x=index, y=bias*100, colour=Audience))
  + geom_line() + geom_point() + ylim(0, 15)
  + labs(x="Proportion target observations (%)",
         y="Bias (% of true parameter)",
         title="Bias of parameter estimates: Digital")
  + theme(plot.title = element_text(hjust = 0.5)))

# Program
(ggplot(data = Program_df, aes(x=index, y=bias*100, colour=Audience))
  + geom_line() + geom_point() + ylim(0, 50)
  + labs(x="Proportion target observations (%)",
         y="Bias (% of true parameter)",
         title="Bias of parameter estimates: Program")
  + theme(plot.title = element_text(hjust = 0.5)))

# TV
(ggplot(data = TV_df, aes(x=index, y=bias*100, colour=Audience))
  + geom_line() + geom_point() + ylim(0, 20)
  + labs(x="Proportion target observations (%)",
         y="Bias (% of true parameter)",
         title="Bias of parameter estimates: TV")
  + theme(plot.title = element_text(hjust = 0.5)))

# VOD
(ggplot(data = VOD_df, aes(x=index, y=bias*100, colour=Audience))
  + geom_line() + geom_point() + ylim(0, 25)
  + labs(x="Proportion target observations (%)",
         y="Bias (% of true parameter)",
         title="Bias of parameter estimates: VOD")
  + theme(plot.title = element_text(hjust = 0.5)))

# Youtube
(ggplot(data = Youtube_df, aes(x=index, y=bias*100, colour=Audience))
  + geom_line() + geom_point() + ylim(0, 100)
  + labs(x="Proportion target observations (%)",
         y="Bias (% of true parameter)",
         title="Bias of parameter estimates: Youtube")
  + theme(plot.title = element_text(hjust = 0.5)))
