####################################################################
#
# evaluation of regression models
#
####################################################################

mae <- function(observed, predicted)
{
	mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
	sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
	mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val) 
{  
	sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}


##############################################################################
#
# PREDICTION: REGRESSION
#
##############################################################################

regression <- function(learn, test){
  # target variables
  ozone <- learn$O3
  PM10 <- learn$PM10
  
  learn$O3 <- NULL
  learn$PM10 <- NULL
  learn$Glob_radiation_min <- NULL
  
  # TESTING classes
  test.ozone <- test$O3
  test.PM10 <- test$PM10
  
  test$O3 <- NULL
  test$PM10 <- NULL
  test$Glob_radiation_min <- NULL

  regression.models(learn, ozone, test, test.ozone)
  
  regression.models(learn, PM10, test, test.PM10)
}

regression.models <- function(train.data, train.values, test.data, test.values) {
  observed <- test.values
  
  #
  # random forest
  #
  
  rf.model <- randomForest(train.values ~ ., train.data)
  predicted <- predict(rf.model, test.data)
  rmae.rf <- rmae(observed, predicted, mean(train.values))
  print(paste("Random forest: ", rmae.rf))
  flush.console()
  
  
  
  #
  # svm
  #
  
  svm.model <- svm(train.values ~ ., train.data)
  predicted <- predict(svm.model, test.data)
  rmae.svm <- rmae(observed, predicted, mean(train.values))
  print(paste("SVM: ", rmae.svm))
  flush.console()
  
  
  
  #
  # k-nearest neighbor
  #
  
  knn.model <- kknn(train.values ~ ., train.data, test.data, k = 10)
  predicted <- fitted(knn.model)
  rmae.knn <- rmae(observed, predicted, mean(train.values))
  print(paste("K-NN: ", rmae.knn))
  flush.console()
  
  
  
  #
  # neural network
  #
  
  #
  # important!!! 
  # in regression problems use linout = T
  
  #set.seed(6789)
  nn.model <- nnet(train.values ~ ., train.data, size = 5, decay = 1e-4, maxit = 10000, linout = T, trace = FALSE)
  predicted <- predict(nn.model, test.data)
  rmae.nn <- rmae(observed, predicted, mean(train.values))
  print(paste("NN: ", rmae.nn))
  flush.console()
}


