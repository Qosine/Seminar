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
separate_predictors_responses <- function(data) {
  
  # Extract columns of interest
  df_contacts = data[,93:182]
  familiarity = data[,22]
  awareness = data[,23]
  consideration = data[,24]
  
  # Sum across categories
  audiosum = rowSums(df_contacts[,1:5])
  digitalsum = rowSums(df_contacts[,6:12])
  programsum = df_contacts[,13]
  tvsum = rowSums(df_contacts[,14:81])
  vodsum = rowSums(df_contacts[,82:89])
  yousum = df_contacts[,90]
  
  # Demographic controls
  male <- (ifelse(data[,5] == "male", 1,0))
  havechildren <- (ifelse(data[,7] == "yes", 1,0))
  age3544 <- (ifelse(data[,4] <=44 & data[,4] >=35, 1,0))
  age4554 <- (ifelse(data[,4] <=54 & data[,4] >=45, 1,0))
  age55plus <- (ifelse(data[,4] >=55, 1,0))
  employed <- (ifelse(data[,12] == "employed", 1,0))
  income3050 <- (ifelse(data[,13] == "[30,50)", 1,0))
  income5075 <- (ifelse(data[,13] == "[50,75)", 1,0))
  income75100 <- (ifelse(data[,13] == "[75,100)", 1,0))
  income100150 <- (ifelse(data[,13] == "[100,150)", 1,0))
  income150200 <- (ifelse(data[,13] == "[150,200)", 1,0))
  income2001000 <- (ifelse(data[,13] == "[200,1000]", 1,0))
  educ3 <- (ifelse(data[,14] == "tertiary", 1,0))
  etn_cauc <- (ifelse(data[,15] == "yes", 1,0))
  etn_afric <- (ifelse(data[,16] == "yes", 1,0))
  etn_hisp <- (ifelse(data[,17] == "yes", 1,0))
  etn_asian <- (ifelse(data[,18] == "yes", 1,0))
  etn_native <- (ifelse(data[,19] == "yes", 1,0))
  etn_other <- (ifelse(data[,20] == "yes", 1,0))
  married <- (ifelse(data[,6] == "married", 1,0))
  single <- (ifelse(data[,6] == "single", 1,0))
  separated <- (ifelse(data[,6] == "separated", 1,0))
  
  # Return single argument with predictor and response values
  out = list()
  out$predictors = cbind(audiosum, digitalsum, programsum, tvsum, vodsum, yousum,
                         male, havechildren, age3544, age55plus, employed,
                         income3050,income5075, income75100,income100150, income150200, income2001000,
                         educ3,
                         etn_cauc, etn_afric, etn_hisp, etn_asian, etn_native, etn_other,
                         married, single,separated)
  out$familiarity = familiarity
  out$awareness = awareness
  out$consideration = consideration
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

predict_response <- function(predictors, parameters, threshold) {
  fitted_probabilities = sigmoid_function(predictors%*%parameters)
  return(as.numeric( fitted_probabilities>threshold ))
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
standardisedBias <- function(beta_true, beta_hat) {
  return( t(colMeans(beta_hat)-beta_true)%*%solve(var(beta_hat))%*%(colMeans(beta_hat)-beta_true) )
}
standardisedBias <- function(beta_true, beta_hat) {
  return( (colMeans(beta_hat)-beta_true)/sqrt(diag(var(beta_hat))))
}

## PROCESS RESULTS ##

