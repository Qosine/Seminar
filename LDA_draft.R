LDA_prediction <- function(data = true_target_variables, train_size= 5/6, ...){
  library(MASS)
  library(caret)
  library(dplyr)
  # install.packages("pROC")
  library(pROC)
  
  
  # generate_response(predictors, parameters, sample_size)
  set.seed(2020)
  
  #seperate train and test
  data_familiarity = data$consideration
  #sample train_size of the data
  idx_train = createDataPartition(data_familiarity, p = train_size, list = F)
  train_data = cbind( data_familiarity[idx_train] , data$predictors[idx_train,] ) 
  test_data = cbind( data_familiarity[-idx_train] , data$predictors[-idx_train,] )  
  
  #corrplot(cor(train_data), type="upper", order="hclust", 
   #        sig.level = 0.01, insig = "blank")
  
  # training data
  # train.control <- trainControl(method = "cv", number = 10)
  train.1 =  train_data[train_data[,1]==1,] # train data with label 1
  train.0 =  train_data[train_data[,1]==0,] # train data with label 0
  train.data = data.frame(rbind(train.1,train.0)) # merge them
  # testing data from the same data generating process
  test.1 =  test_data[test_data[,1]==1,] # test data with label 1
  test.0 =  test_data[test_data[,1]==0,] # test data with label 0
  test.data = data.frame(rbind(test.1,test.0)) # merge them
  
  # Classification by Linear Discriminant Analysis
  lda.1 <- lda( V1 ~ ., data = train.data )
  test.data = as.data.frame(test.data)
  true_neg = sum(test.data[,1]==0 & predict(lda.1, test.data)$class==0) # correct predicted "KPI = 0", also called "specificity"
  true_pos = sum(test.data[,1]==1 & predict(lda.1, test.data)$class==1) # correct predicted "KPI = 1", also called "sensitivity"
  false_pos = sum(test.data[,1]==0 & predict(lda.1, test.data)$class==1) # error type I
  false_neg = sum(test.data[,1]==1 & predict(lda.1, test.data)$class==0) # error type II
  
  prediction_accuracy = (true_neg + true_pos ) / nrow(test.data)
  print(paste("prediction accuracy =", prediction_accuracy))
  
  # auc(response = test_data[,1], predictor = predict(lda.1, test.data)$class)
  
}
