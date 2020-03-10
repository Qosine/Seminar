# Fix seed and load libraries
set.seed(123456)
library(mvtnorm); library(dplyr); library(survey); library(ggplot2); library(robustbase)

# Pathing - fix this on your machine first (set to local Git directory)
# path = "~/Documents/Econometrie/Masters/Seminar Nielsen"
# path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"
# setwd(path)
source("./200302_simulation_support_functions.R")

#######################################################################
####### CHOOSE TRUE PARAMETERS FOR TARGET AND NON-TARGET GROUPS #######
#######################################################################

# NOTE: If you want to use target and non-target parameters as ...
#       originally derived from the Nielsen data, set boolean ...
#       use_Nielsen_parameters below to TRUE

true_nontarget_params = c(-1.5, -3, 3.2, -1.0, 0.5, 1.0, 0.6)
true_target_params = c(-2.0, 0.6, 0.5, -0.8, 0.6, 0.4,0.8)

use_Nielsen_parameters = FALSE

#######################################################################
#######################################################################




#######################################################################
################## DO NOT CHANGE THE FOLLOWING ########################
#######################################################################

# NOTE: Target group/KPI choices are irrelevant anyway
#       We manually choose parameters of interest and generate own KPIs

CPS <- rbind.data.frame(c(0.203195,0.10298,0.100214),
                        c(0.185959,0.092719,0.09324),
                        c(0.186438,0.091954,0.094484),
                        c(0.424408,0.195766,0.228643))
colnames(CPS) = c("Total", "Male", "Female")
rownames(CPS) = c("25-34", "35-44", "45-54", "55-99")
target_gender = "Male"
target_age = "25-34"
kpi = "Consideration"




#######################################################################
####### LOAD POINTLOGIC SOURCE DATA AND DO SOME PRE-PROCESSING ########
#######################################################################

# Separate target age group for future reference
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

# If user chose to use parameters derived from Nielsen source data, override parameter choices
if (use_Nielsen_parameters == TRUE) {
  true_target_params = unname(necessary_info$target_params)
  true_nontarget_params = unname(necessary_info$nontarget_params)
}

# Compute population-level parameters as linear combination of target and non-target params
true_population_params = (CPS[target_age, target_gender]*true_target_params
                          + (1-CPS[target_age, target_gender])*true_nontarget_params)




#######################################################################
############# CONSTRUCT DATASETS USED DURING SIMULATION ###############
#######################################################################

real_targets = real_nontargets = simulated_targets = simulated_nontargets = list()

# Target data - real (source) as well as simulated (resampled)
real_targets$predictors = add_constant(separate_predictors_responses(subsamples$target)$predictors)
real_targets$predictors = (cbind(real_targets$predictors[,-(3:4)], rowSums(real_targets$predictors[,(3:4)])))
real_targets$kpi = generate_response(real_targets$predictors,
                                     true_target_params,
                                     nrow(real_targets$predictors))
load("./simulated_target_predictors.RData")
#simulated_target_predictors = necessary_info$target_data
simulated_targets$predictors = nthroot(add_constant(simulated_target_predictors),3)
simulated_targets$kpi = generate_response(simulated_targets$predictors,
                                          true_target_params,
                                          nrow(simulated_targets$predictors))

# Define variables for real non-target data as well as 'resampled' data
real_nontargets$predictors = add_constant(separate_predictors_responses(subsamples$nontarget)$predictors)
real_nontargets$predictors = (cbind(real_nontargets$predictors[,-(3:4)], rowSums(real_nontargets$predictors[,(3:4)])))^(1/3)
real_nontargets$kpi = generate_response(real_nontargets$predictors,
                                        true_nontarget_params,
                                        nrow(real_nontargets$predictors))
load("./simulated_nontarget_predictors.RData")
simulated_nontargets$predictors = nthroot(add_constant(simulated_nontarget_population),3)
simulated_nontargets$kpi = generate_response(simulated_nontargets$predictors,
                                             true_nontarget_params,
                                             nrow(simulated_nontargets$predictors))
res_target <- list()
res_nontarget <-list()

bias_nontarget <- matrix(0, nrow=20,ncol=7)
for(j in 1:20){
  res_target[[j]] <- matrix(0,nrow=1000,ncol =7)
  res_nontarget[[j]] <- matrix(0,nrow=1000,ncol =7)
  N= 500+j*100
  print(paste("Currently for N =:", N))
  for(i in 1:1000){
real_targets$predictors = add_constant(separate_predictors_responses(rbind(subsamples$target,subsamples$nontarge))$predictors[sample((1:7500), N),])
#real_targets$predictors = (cbind(real_targets$predictors[,-(3:4)], rowSums(real_targets$predictors[,(3:4)])))
real_targets$kpi = generate_response(real_targets$predictors,
                                     true_target_params,
                                     nrow(real_targets$predictors))
real_nontargets$predictors = add_constant(separate_predictors_responses(rbind(subsamples$target,subsamples$nontarge))$predictors)[sample((1:7500), N),]
#real_nontargets$predictors = (cbind(real_nontargets$predictors[,-(3:4)], rowSums(real_nontargets$predictors[,(3:4)])))
real_nontargets$kpi = generate_response(real_nontargets$predictors,
                                        true_nontarget_params,
                                        nrow(real_nontargets$predictors))
res_target[[j]][i,] <- glm(real_targets$kpi ~ 0 + real_targets$predictors, family=binomial(link="logit"))$coefficient
res_nontarget[[j]][i,] <-  glm(real_nontargets$kpi ~ 0 + real_nontargets$predictors, family=binomial(link="logit"))$coefficients
}
bias_target[j,] <- (true_target_params - colMeans(res_target[[j]]))/true_target_params
bias_nontarget[j,] <- (true_nontarget_params - colMeans(res_nontarget[[j]]))/true_nontarget_params
}
colnames(bias_target) <- colnames(real_targets$predictors)
colnames(bias_nontarget) <- colnames(real_targets$predictors)
rownames(bias_target) <- (c(1:20)*100+500)
rownames(bias_nontarget) <- (c(1:20)*100+500)
bias_nontarget <- 100*round(bias_nontarget,3)
bias_target <- 100*round(bias_target,3)
colMeans(res_target)
colMeans(res_nontarget)
#######################################################################
######## CHECK IF PARAMETER CHOICE DOES NOT CREATE RARE EVENTS ########
#######################################################################

if ( max(mean(simulated_targets$kpi), mean(simulated_nontargets$kpi)) > 0.9 |
     min(mean(simulated_targets$kpi), mean(simulated_nontargets$kpi)) < 0.1 ) {
  stop(cat("Parameter choice creates a rare event scenario:\n",
           100*round(mean(simulated_targets$kpi),3), "% of targets and ",
           100*round(mean(simulated_nontargets$kpi),3), "% of nontargets have a KPI equal to 1\n",
           "Breaking program, please try a less extreme parameter set", sep="")) }




#######################################################################
###################### MAIN SIMULATION FUNCTION ######################
#######################################################################

# Inputs: N                           : Total sample size
#         Q                           : Proportion of observations that are target group
#         reps                        : Number of replications
#         use_Nielsen_target_data     : Whether to use Nielsen source data of targets or
#                                       simulated (resampled) data. Default is resampled
#         use_Nielsen_nontarget_data  : Whether to use Nielsen source data of nontargets or
#                                       simulated (resampled) data. Default is resampled
#         target_group_gender         : Whether target group is male or female
#                                       Automatically determined in current implementation,
#                                       based on user choices at beginning of script
#         target_group_age            : Age range of the target group
#                                       Automatically determined in current implementation,
#                                       based on user choices at beginning of script
#         kpi                         : KPI to be investigated
#                                       Automatically determined in current implementation,
#                                       based on user choices at beginning of script


run_simulation <- function(N, Q, reps,
                           use_Nielsen_target_data = FALSE,
                           use_Nielsen_nontarget_data = FALSE,
                           target_group_gender = target_gender,
                           target_group_age = target_age,
                           kpi = kpi) {
  
  # If testing within run_simulation(), uncomment the below
  # target_group_gender = target_gender; target_group_age = target_age
  # use_Nielsen_target_data = use_Nielsen_nontarget_data = FALSE
  # N = 7500; Q = 0.5; reps = 100
  
  # For looped simulation, keep track of which value of Q we are currently on
  print(paste("Q:", Q))
  start_time = Sys.time()
  
  # Allocate memory for simulation results
  dimension = ncol(true_fullsample_variables$predictors) + 1 # add dimension for intercept
  beta.glm_target = data.frame(matrix(0, reps, dimension))
  beta.glm_nontarget = data.frame(matrix(0, reps, dimension))
  beta.svyglm = data.frame(matrix(0, reps, dimension))
  beta.glm_interaction = data.frame(matrix(0, reps, dimension*2))
  LRT.glm_target = rep(0, reps)
  LRT.svyglm = rep(0, reps)
  LRT.glm_interaction = rep(0, reps)
  ttest.svyglm = rep(0, reps)
  LRT.diffmodel_results = vector()
  
  # Name columns of results df
  coefficient_names = c("Intercept", "Audio", "Digital", "Program", "TV",
                        "VOD", "Youtube", "Target", "Target*Audio", "Target*Digital",
                        "Target*Program", "Target*TV", "Target*VOD", "Target*Youtube")
  colnames(beta.glm_target) = coefficient_names[1:dimension]
  colnames(beta.svyglm) = coefficient_names[1:dimension]
  colnames(beta.glm_interaction) = coefficient_names
  
  for (i in 1:reps) {
    

    # Compile explanatory variables
    rs_targets = rs_nontargets = list()
    
    if (use_Nielsen_target_data == TRUE) {

      idx_rs_targets = sample( 1:nrow(subsamples$target), (N*Q) )
      rs_targets$predictors = real_targets$predictors[idx_rs_targets,]
      rs_targets$kpi = real_targets$kpi[idx_rs_targets]
    } else {
      idx_rs_targets = sample( 1:nrow(simulated_targets$predictors), (N*Q) )

      rs_targets$predictors = simulated_targets$predictors[idx_rs_targets,]
      rs_targets$kpi = simulated_targets$kpi[idx_rs_targets]
    }
    
    if (use_Nielsen_nontarget_data == TRUE) {
      idx_rs_nontargets = sample( 1:nrow(subsamples$nontarget), (N*(1-Q)) )
      rs_nontargets$predictors = real_nontargets$predictors[idx_rs_nontargets,]
      rs_nontargets$kpi = real_nontargets$kpi[idx_rs_nontargets]
    } else {
      idx_rs_nontargets = sample( 1:nrow(simulated_nontargets$predictors), (N*(1-Q)) )
      rs_nontargets$predictors = simulated_nontargets$predictors[idx_rs_nontargets,]
      rs_nontargets$kpi = simulated_nontargets$kpi[idx_rs_nontargets]
    }
    
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
    interaction = rbind(rs_targets$predictors, matrix(0, nrow(rs_nontargets$predictors), ncol(rs_nontargets$predictors)))
    colnames(interaction) = rename_interaction_columns(colnames(interaction))
    interaction_predictors = cbind(full_sample$predictors, interaction)
    
    # Fit regular logit for target observations
    # Also fit model under null hypothesis (i.e. model with true parameters) for variance check
    glm.target_audience <- glm(rs_targets$kpi ~ 0 + rs_targets$predictors,
                               family = binomial(link="logit"))
    LRT.glm_target.H0 <- glm(rs_targets$kpi ~ 0 + offset(rs_targets$predictors%*%true_target_params),
                                   family = binomial(link = "logit"))
    LRT_statistic.glm_target = 2*abs(logLik(glm.target_audience) - logLik(LRT.glm_target.H0))
    df.LRT.glm_target = length(true_target_params)
    
    
    # Fit Horvitz-Thompson estimator for full sample with weights
    svy_inputs = create_svyglm_inputs(full_sample$predictors, full_sample$kpi)
    design_func <- svydesign(id = ~1,
                             data = svy_inputs$data,
                             weight = weights)
    svyglm.total_audience <- svyglm(formula = svy_inputs$func,
                                    design =  design_func,
                                    family = "quasibinomial")
    glm.nontarget_audience <- glm(rs_nontargets$kpi ~ 0 + rs_nontargets$predictors,
                                  family = binomial(link="logit"))
    # Check whether model rejects H0: parameter vector is DGP parameter vector (approximate LRT)
    # regTermTest uses either a chi-square or a F null distribution
    # df = Inf refers to denominator df, telling function to use chi-square distribution for LRT
    # Approximation method is chosen to be able to deal with negative true parameters
    # LRT.svyglm.pvalue = regTermTest(model = svyglm.total_audience,
    #                                 test.terms = (~ constant + v_audiosum + v_digitalsum
    #                                               + v_programsum + v_tvsum + v_vodsum + v_yousum),
    #                                 null = true_population_params,
    #                                 method = "LRT", df = Inf, lrt.approximation = "saddlepoint")$p
    LRT.svyglm.pvalue = 0
    
    # Further check whether model rejects H0: slope of a variable w/o information is zero (exact t-test)
    meaningless_variable.full_sample = rnorm(nrow(interaction_predictors))
    var_check_inputs = create_svyglm_inputs(cbind(full_sample$predictors, meaningless_variable.full_sample),
                                            full_sample$kpi)
    var_check_design_func <- svydesign(id = ~1,
                                       data = var_check_inputs$data,
                                       weight = weights)
    ttest.svyglm.pvalue <- summary(svyglm(formula = var_check_inputs$func,
                                          design = var_check_design_func,
                                          family = "quasibinomial"))$coefficients[ncol(full_sample$predictors)+1,
                                                                                  "Pr(>|t|)"]
    
    # For reference against the Horvitz-Thompson estimator, fit an ordinary logit for the entire sample
    glm.total_audience <- glm(full_sample$kpi ~ 0 + full_sample$predictors,
                              family = binomial(link="logit"))
    
    # Fit unweighted logit for model with interactions
    glm.interaction_model <-  glm(full_sample$kpi ~ 0 + interaction_predictors,
                                  family = binomial(link="logit"))
    true_interaction_params = append(true_nontarget_params, (true_target_params-true_nontarget_params) )
    LRT.glm_interaction.H0 <- glm(full_sample$kpi ~
                                          0 + offset(interaction_predictors%*%true_interaction_params),
                                        family = binomial(link = "logit"))
    LRT_statistic.glm_interaction = 2*abs(logLik(glm.interaction_model) - logLik(LRT.glm_interaction.H0))
    df.glm_interaction = length(true_interaction_params)
    
    # Store results of current run
    beta.glm_target[i,] = glm.target_audience$coefficients
    beta.glm_nontarget[i,] = glm.nontarget_audience$coefficients
    beta.svyglm[i,] = svyglm.total_audience$coefficients
    beta.glm_interaction[i,] = glm.interaction_model$coefficients
    LRT.glm_target[i] = ( LRT_statistic.glm_target > qchisq(0.95, df.LRT.glm_target) )
    LRT.svyglm[i] = ( LRT.svyglm.pvalue <= 0.05 )
    ttest.svyglm[i] = ( ttest.svyglm.pvalue <= 0.05 )
    LRT.glm_interaction[i] = ( LRT_statistic.glm_interaction > qchisq(0.95, df.glm_interaction) )
    LRT.diffmodel_results[i] = ifelse(lr.test(glm.total_audience, glm.interaction_model)$pvalue  == FALSE,0,1)
    
    # Keep track of which simulation run we are in
    if (i%%100 == 0) {
      current_time = Sys.time()
      elapsed = current_time - start_time
      print(paste("Currently at iteration:", i, "| Time elapsed:", round(elapsed, 2), attr(elapsed, "units")))
    }
    
  }
  
  # Interaction model does not immediately produce target- or population-level parameters, so derive these first
  beta.glm.interaction.target <- beta.glm_interaction[,1:dimension] + beta.glm_interaction[,(dimension+1):(2*dimension)]
  target_weight = CPS[target_group_age, target_group_gender]
  beta.glm.interaction.total <- (beta.glm_interaction[,1:dimension]*(1-target_weight)
                                 + target_weight*(beta.glm_interaction[,1:dimension] + beta.glm_interaction[,(dimension+1):(2*dimension)]))
  
  out = list()
  out$beta.glm.target_audience_only <- beta.glm_target
  out$beta.glm.nontarget_audience_only <- beta.glm_nontarget
  out$beta.svyglm.total_audience <- beta.svyglm
  out$beta.glm.interaction.target <- beta.glm.interaction.target
  out$beta.glm.interaction.total <- beta.glm.interaction.total
  out$LRT.glm_target <- LRT.glm_target
  out$LRT.svyglm <- LRT.svyglm
  out$ttest.svyglm <- ttest.svyglm
  out$LRT.glm_interaction <- LRT.glm_interaction
  out$LRT.diffmodel_results <- LRT.diffmodel_results
  
  return(out)
}


target_proportions = c(10,50,90)  #5*(2:18)[c(TRUE,FALSE)]
for (prop in target_proportions) {
  assign(paste("Q", prop, sep=""), c())
  assign(paste("Q", prop, sep=""), run_simulation(N = 7500, Q = prop/100, reps = 10,
                                                  target_group_gender = target_gender, target_group_age = target_age,
                                                  kpi = kpi))
}

df_rows = paste("Q", target_proportions, sep = "")
df_cols = c("Intercept", "Audio", "Digital", "Program", "TV", "VOD", "Youtube")
glm_target_results_df = data.frame(matrix(0, length(df_rows), length(df_cols)))
rownames(glm_target_results_df) = df_rows; colnames(glm_target_results_df) = df_cols
svyglm_total_results_df = interaction_targ_results_df = glm_nontarget_results_df = interaction_total_results_df = interaction_nontarg_results_df = glm_target_results_df
LRT.diffmodel_results_df = data.frame(matrix(0, 1,length(df_rows)))
colnames(LRT.diffmodel_results_df) = df_rows

for (prop in target_proportions) {
  varname = paste("Q", prop, sep="")
  estimates = get(varname)
  glm_targ_bias = pctBias(true_target_params, as.matrix(estimates$beta.glm.target_audience_only))
  glm_nontarg_bias = pctBias(true_nontarget_params, as.matrix(estimates$beta.glm.nontarget_audience_only))
  svyglm_total_bias = pctBias(true_population_params, as.matrix(estimates$beta.svyglm.total_audience))
  interaction_targ_bias = pctBias(true_target_params, as.matrix(estimates$beta.glm.interaction.target))
  interaction_total_bias = pctBias(true_population_params, as.matrix(estimates$beta.glm.interaction.total))
  
  interaction_nontarget_params = ((estimates$beta.glm.interaction.total
                                  - CPS[target_age, target_gender]*estimates$beta.glm.interaction.target)
                                  /(1-CPS[target_age, target_gender]))
  interaction_nontarg_bias = pctBias(true_nontarget_params, as.matrix(interaction_nontarget_params))
  LRT.diffmodel_results_df[,varname] <- sum(estimates$LRT.diffmodel_results)/length(estimates$LRT.diffmodel_results)
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
round(100*interaction_total_results_df,1)[1:3,]
round(100*svyglm_total_results_df,1)[1:3,]
round(100*(interaction_total_results_df-svyglm_total_results_df),1)[1:8,]

LRT.diffmodel_results_df

round(100*glm_target_results_df,1)[1:9,]
round(100*glm_nontarget_results_df,1)[1:9,]
round(100*interaction_targ_results_df,1)[1:9,]
round(100*interaction_nontarg_results_df,1)[1:9,]
