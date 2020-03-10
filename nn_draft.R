NeuralNet_prediction <- function(data = true_target_variables, train_size= 5/6, ...){
  # install.packages(neuralnet)
  library(neuralnet)
  library(MASS)
  library(caret)

  set.seed(2020)
  
  
  #seperate train and test
  data_familiarity = data$consideration
  #sample train_size of the data
  idx_train = createDataPartition(data_familiarity, p = train_size, list = F)
  train_data = cbind( data_familiarity[idx_train] , data$predictors[idx_train,] ) 
  test_data = cbind( data_familiarity[-idx_train] , data$predictors[-idx_train,] )    
  
  # training data
  
  train.1 =  train_data[train_data[,1]==1,] # train data with label 1
  train.0 =  train_data[train_data[,1]==0,] # train data with label 0
  train.data = data.frame(rbind(train.1,train.0)) # merge them
  # testing data from the same data generating process
  test.1 =  test_data[test_data[,1]==1,] # test data with label 1
  test.0 =  test_data[test_data[,1]==0,] # test data with label 0
  test.data = data.frame(rbind(test.1,test.0)) # merge them
  
  # Classification by Neural Network
  yf = as.factor(train.data[,1]) 
  
  nn.1 <- neuralnet(yf ~., data = train.data[,-1], hidden = 3) #training neural net
  pred.nn = predict(nn.1, test.data[,-1])[,1]
  true_neg = sum(test.data[,1]==0 & pred.nn>0.5)
  true_pos = sum(test.data[,1]==1 & pred.nn<0.5)
  false_pos = sum(test.data[,1]==0 & pred.nn<0.5)
  false_neg = sum(test.data[,1]==1 & pred.nn>0.5)
  
  prediction_accuracy = (true_neg + true_pos ) / nrow(test.data)
  print(paste("prediction accuracy =", prediction_accuracy))
  
   # auc(nn.1, response = test_data[,1], predictor = pred.nn)
  
}