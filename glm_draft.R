GLM_prediction <- function(data = true_target_variables, train_size= 5/6, ...){
  library(MASS)
  library(caret)
  library(dplyr)
  install.packages("pROC")
  library(pROC)
  
  # generate_response(predictors, parameters, sample_size)
  set.seed(2020)
  
  #seperate train and test
  data_familiarity = data$consideration
  #sample train_size of the data
  idx_train = createDataPartition(data_familiarity, p = train_size, list = F)
  train_data = cbind( data_familiarity[idx_train] , data$predictors[idx_train,] ) 
  test_data = cbind( data_familiarity[-idx_train] , data$predictors[-idx_train,] )  
  
  corrplot(cor(train_data), type="upper", order="hclust", 
           sig.level = 0.01, insig = "blank")
  
  # training data
  # train.control <- trainControl(method = "cv", number = 10)
  train.1 =  train_data[train_data[,1]==1,] # train data with label 1
  train.0 =  train_data[train_data[,1]==0,] # train data with label 0
  train.data = data.frame(rbind(train.1,train.0)) # merge them
  # testing data from the same data generating process
  test.1 =  test_data[test_data[,1]==1,] # test data with label 1
  test.0 =  test_data[test_data[,1]==0,] # test data with label 0
  test.data = data.frame(rbind(test.1,test.0)) # merge them
  
  # Classification by Random Forest
  yf = as.factor(train.data[,1]) #IS FIRST COLUMN THE LABEL?
  # trnSpl =  data.frame(yf,train.data)
  
  glm.1 <- glm( yf ~ ., data = train.data[,-1], family=binomial(link="logit") )
  
  yf = as.factor(test.data[,1])
  testSpl = data.frame(yf, test.data)
  
  pred.glm = predict.glm(glm.1, test.data[,-1], type = "link")
  true_neg = sum(test.data[,1]==0 & pred.glm<0)
  true_pos = sum(test.data[,1]==1 & pred.glm>0)
  false_pos = sum(test.data[,1]==0 & pred.glm>0)
  false_neg = sum(test.data[,1]==1 & pred.glm<0)
  
  prediction_accuracy = (true_neg + true_pos ) / nrow(test.data)
  print(paste("prediction accuracy =",prediction_accuracy))
  
  auc(response = test_data[,1], predictor = pred.glm)
  
}
