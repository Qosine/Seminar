RandomForest <- function(train_set, test_set, ...){
  library(randomForest)
  library(MASS)
  library(caret)
  install.packages(rfUtilities)
  library(rfUtilities)
  
  generate_response(predictors, parameters, sample_size)
  
  
  set.seed(123)

  # training data
  # train.control <- trainControl(method = "cv", number = 10)
  train.1 =  ... # train data with label 1
  train.0 =  ... # train data with label 0
  train.data = data.frame(rbind(train.1,train.0)) # merge them
  # testing data from the same data generating process
  test.1 =  ... # test data with label 1
  test.0 =  ... # test data with label 0
  test.data = data.frame(rbind(test.1,test.0)) # merge them

  # Classification by Random Forest
  yf = as.factor(1 - train.data[,1]) #IS FIRST COLUMN THE LABEL?
  trnSpl =  data.frame(yf,train.data)
  rf.1 <- randomForest(yf ~ X2+X3, data = trnSpl, ntree = 2001, importance=T) #CHECK X's
  rf.cv <- rf.crossValidation(rf.1, xdata = as.data.frame(cbind(X2,X3)), p=0.10, n=10, ntree=2001) #CHECK X's
  yf = as.factor(1 - test.data[,1])
  testSpl = data.frame(yf, test.data)
  pred.rf = predict(rf.1, testSpl, type = "prob")[,1]
  true_neg = sum(test.data[,1]==0 & pred.rf<0.5)
  true_pos = sum(test.data[,1]==1 & pred.rf>0.5)
  false_pos = sum(test.data[,1]==0 & pred.rf>0.5)
  false_neg = sum(test.data[,1]==1 & pred.rf<0.5)
  
  prediction_accuracy = (true_neg + true_pos ) / nrow(train.data)
  

  # boxplot(rf00);boxplot(rf10); boxplot(rf01); boxplot(rf11);
}