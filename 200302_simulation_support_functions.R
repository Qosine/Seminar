## SETUP ##
split_sample <- function(data,
                         target_gender,
                         min_age, # min_age and max_age are INCLUSIVE
                         max_age) {
  
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
  response <- rbinom( n=sample_size, size=1, prob=Pr_success ) # draw from corresponding Ber(p) (= Bin(1,p)) distribution
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

## FITTING TRUE MODELS ##
compute_population_params <- function(target_gender, target_age, dependent_variable) {
  
  target_CPS_weight = CPS[target_age, target_gender]
  if ( tolower(dependent_variable)=="consideration" ) {
    target_params = logit.target.consideration$coefficients
    nontarget_params = logit.nontarget.consideration$coefficients
  } else if ( tolower(dependent_variable)=="awareness" ) {
    target_params = logit.target.awareness$coefficients
    nontarget_params = logit.nontarget.awareness$coefficients
  } else if ( tolower(dependent_variable)=="familiarity" ) {
    target_params = logit.target.familiarity$coefficients
    nontarget_params = logit.nontarget.familiarity$coefficients
  } else {
    print("Invalid KPI chosen, breaking program"); break
  }
  return(target_params*target_CPS_weight + nontarget_params*(1-target_CPS_weight))
}

## COLLECT ALL NECESSARY TARGET INFO FROM USER INPUT ##
initialise_target_group <- function(target_gender = c("Male", "Female"),
                                    target_age = c("25-34", "35-44", "45-54", "55-99"),
                                    kpi = c("Familiarity", "Consideration", "Awareness")) {
  
  target_string = paste("./", tolower(target_gender), "_", substring(target_age,1,2),
                        "_", substring(target_age, nchar(target_age)-1, nchar(target_age)),
                        ".RDS", sep="")
  
  if (kpi=="Familiarity") {
    true_target_params = logit.target.familiarity$coefficients
    true_nontarget_params = logit.nontarget.familiarity$coefficients
  } else if (kpi=="Consideration") {
    true_target_params = logit.target.consideration$coefficients
    true_nontarget_params = logit.nontarget.consideration$coefficients
  } else if (kpi=="Awareness") {
    true_target_params = logit.target.awareness$coefficients
    true_nontarget_params = logit.nontarget.awareness$coefficients
  } else {
    print("Invalid KPI selected, breaking program"); break
  }
   
  
  return(list(target_data = readRDS(target_string),
              target_params = true_target_params,
              nontarget_params = true_nontarget_params))
}

## EVALUATION OF SIMULATION OUTPUT ##
MSPE <- function(y, yhat) {(mean((y - yhat)^2))^.5}
MSE <- function(true_beta, estimates) { (estimates-true_beta)%*%t(estimates-true_beta) }
Bias <- function(beta_true, beta_hat) {colMeans(beta_hat) - beta_true}
pctBias <- function(beta_true, beta_hat) {abs((colMeans(beta_hat) - beta_true)/beta_true)}
medianBias <- function(beta_true, beta_hat) {abs((colMedians(beta_hat) - beta_true)/beta_true)}

