#Logit Base model - full model
logit <- function(y,X){
  blogit <- glm(y ~ X, data= data, family = "binomial")
  return(summary(blogit))
}
#Weighted Logit Base model - Weights
Weights <- function(data,CPS_target,gender,upper, lower){
  CPS_nontarget <- 1-CPS_target
  data$target <- ifelse(data$sd_gender == gender &  data$sd_age<=upper & data$sd_age>=lower, 1,0)
  sample_weight_target <- sum(data$target)/nrow(data)
  sample_weight_nontarget <- 1-sample_weight_target
  Weights <- ifelse(data$sd_gender == gender &  data$sd_age<=upper & data$sd_age>=lower , CPS_target/sample_weight_target,CPS_nontarget/sample_weight_nontarget)
  return(Weights)
}
#actual Weighted Logit
Wlogit <- function(y, X, Weights){
  blogit <- glm(y ~ X, data= data, family = "binomial", weights = Weights)
  return(summary(blogit))
}
# lasso Logit
Lasso_logit <- function(y, X){
  RMSE <- function(y, yhat){(mean((y - yhat)^2))^.5}
  lasso_logit.cv <- cv.glmnet(X, y, alpha = 1,
                              standardize = FALSE, family = "binomial", intercept = TRUE, nfolds = 10)
  lasso_logit <- glmnet(X, y, alpha = 1,
                        standardize = FALSE, family = "binomial", intercept = TRUE) # path only for training data
  plot(lasso_logit, xvar = "lambda", label = TRUE)
  
  
  lasso_logit.best <- glmnet(X, y, alpha = 1,
                             lambda = lasso_logit.cv$lambda.min,
                             standardize = FALSE, family = "binomial",
                             intercept = TRUE)
  round(lasso_logit.best$beta, digits = 2)
  lasso_logit_fit <- predict(lasso_logit.best, X)
  RMSE(y, lasso_logit_fit)
  blogit <- glm(y ~ X, family = "binomial")
  logit_fit <- predict.glm(blogit, as.data.frame(X))
  RMSE(y, logit_fit)
  
  cvfit <- glmnet::cv.glmnet(X, y)
  return(coef(cvfit, s = "lambda.min"))
}

survey_strat_logit <- function(df_X, v_y, Weights_res){
  
  #data alterations
  df_glm_data <- as.data.frame(cbind(v_y,df_X))
  v_x_var <- colnames(df_X[,-which(colnames(df_X) == "v_strat")])
  str_x_var <- paste(v_x_var,collapse=" + ")
  stratfunction = paste("v_y ~ ", str_x_var)
  
  #define stratified sample design
  f_stratdesign <- 
    svydesign(
      id = ~1,
      data = df_glm_data  ,
      weight = Weights_res ,
      strata = strat ,
    )
  
  #weighted surbey glm estimator 
  glmest <- svyglm(stratfunction,
                   design =  f_stratdesign, family = "quasibinomial")
  
  #return summary
  return(summary(glmest))
}
