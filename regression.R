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
  
  print("Max ozone level models (O3): ")
  flush.console()

  predicted <- regression.models(learn, ozone, test, test.ozone)
  plot(test.ozone,type="l",col="red")
  points(predicted,col="green")
  
  print("Large pollution particles models (PM10): ")
  flush.console()
  
  predicted <- regression.models(learn, PM10, test, test.PM10)
  plot(test.PM10,type="l",col="red")
  points(predicted,col="green")
}

regression.models <- function(train.data, train.values, test.data, test.values) {
  observed <- test.values
  
  #
  # random forest
  #
  
  rf.model <- randomForest(train.values ~ ., train.data)
  rf.predicted <- predict(rf.model, test.data)
  rmae.rf <- rmae(observed, rf.predicted, mean(train.values))
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
  
  
  # the algorithm is more robust when scaled data is used
  norm.data <- scale.data(rbind(train.data,test.data[names(train.data)]))
  norm.learn <- norm.data[1:nrow(train.data),]
  norm.test <- norm.data[-(1:nrow(train.data)),]
  
  set.seed(6789)
  #nn.model <- nnet(train.values ~ ., train.data, size = 1, decay = 1e-4, maxit = 1000000, linout = T, trace = FALSE)
  #nn.model <- nnet(x = norm.learn, y = train.values, size = 1, decay = 1e-4, maxit = 1000000, linout = T, trace = FALSE)
  nn.model <- nnet(train.values ~ ., norm.learn, size = 6, decay = 1e-4, maxit = 10000000, linout = T, trace = FALSE)
  predicted <- predict(nn.model, norm.test)
  rmae.nn <- rmae(observed, predicted, mean(train.values))
  print(paste("Neural network: ", rmae.nn))
  flush.console()
  
  rf.predicted
}


