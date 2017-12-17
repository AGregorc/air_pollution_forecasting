##############################################################################
#
# PREDICTION: CLASSIFICATION
#
##############################################################################

getOzoneLevel <- function(ozone) {
  LOW <- 60.0 
  MODERATE <- 120.0 
  HIGH <- 180.0 
  
  ifelse (ozone < LOW, 1,
          ifelse (ozone < MODERATE, 2,
                  ifelse (ozone < HIGH, 3, 4)))
}

getPM10classes <- function(pm10) {
  LOW <- 35.0 
  
  ifelse (pm10 < LOW, 1, 2)
}

scale.data <- function(data) {
  norm.data <- data
  
  for (i in 1:ncol(data))
  {
    if (!is.factor(data[,i]))
      norm.data[,i] <- scale(data[,i])
  }
  
  norm.data
}

classification <- function(learn, test, calculate.crossValidation){
  learn.ozone <- learn$O3
  ozone <- factor(getOzoneLevel(learn$O3))
  
  learn.PM10 <- learn$PM10
  PM10 <- factor(getPM10classes(learn$PM10))
  
  learn$O3 <- NULL
  learn$Glob_radiation_min <- NULL
  
  # TESTING classes
  test.O3 <- test$O3
  test.ozone <- factor(getOzoneLevel(test$O3))
  test.PM10 <- test$PM10
  test.PM10 <- factor(getPM10classes(test$PM10))
  
  test$O3 <- NULL
  test$Glob_radiation_min <- NULL
  
  # TRAINING THE MAX OZONE LEVEL (O3)
  
  print("Max ozone level models (O3): ")
  flush.console()
  
  predictions <- classification.models(learn, ozone, test, test.ozone, calculate.crossValidation)
  #for (i in 1:nrow(predictions)) {
  #  print(paste("Pred: ", predictions[,i]))
  #  flush.console()
  #}
  
  # TRAINING the concentration of large pollution particles (PM10)
  
  learn$O3 <- learn.ozone
  learn$PM10 <- NULL
  
  test$O3 <- test.O3
  test$PM10 <- NULL
  
  print("Large pollution particles models (PM10): ")
  flush.console()
  
  predictions <- classification.models(learn, PM10, test, test.PM10, calculate.crossValidation)
  #for (i in 1:nrow(predictions)) {
  #  print(paste("Pred: ", predictions[,i]))
  #  flush.console()
  #}
  
  # Combined results
  #pred <- data.frame(predDT, predNB, predKNN, predRF, test.PM10)
  #pred
  
  
  
}

classification.models <- function(train.data, train.class, test.data, test.class, compute.crossValidation) {
  # force the CoreModel function to train a model of a given type (specified by the parameter "target.model")
  mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
  
  # force the predict function to return class labels only and also destroy the internal representation of a given model
  mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}
  
  
  
  #
  #
  # MAJORITY CLASSIFIER
  #
  #
  
  
  # The majority class is the class with the highest number of training examples
  majority.class <- names(which.max(table(train.class)))
  
  # The majority classifier classifies all test instances into the majority class.
  # The accuracy of the majority class
  major <- sum(test.class == majority.class) / length(test.class)
  print(paste("Majority classifier: ", major))
  flush.console()
  
  
  #
  #
  # DECISION TREES
  #
  #
  
  # build a decision tree using information gain as a splitting criterion
  modelDT <- CoreModel(train.class  ~ ., train.data, model="tree")
  #plot(modelDT, train)
  
  predDT <- predict(modelDT, test.data, type = "class")
  caDT <- CA(test.class, predDT)
  print(paste("Decision tree: ", caDT))
  flush.console() 
  
  if (compute.crossValidation) {
    # 10-fold cross validation 
    res <- errorest(train.class ~ ., data=train.data, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree")
    print(paste("Cross validation: ", 1-res$error))
    flush.console()
  }
  
  
  
  #
  #
  # NAIVE BAYES CLASSIFIER
  #
  #
  
  modelNB <- CoreModel(train.class ~ ., train.data, model="bayes")
  
  predNB <- predict(modelNB, test.data, type="class")
  caNB <- CA(test.class, predNB)
  print(paste("Naive bayes: ", caNB))
  flush.console()
  
  if (compute.crossValidation) {
    res <- errorest(train.class ~ ., data=train.data, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "bayes")
    print(paste("Cross validation: ", 1-res$error))
    flush.console()
  }
  
  
  #
  #
  # KNN
  #
  #
  
  modelKNN <- CoreModel(train.class ~ ., train.data, model="knn", kInNN = 5) 
  
  predKNN <- predict(modelKNN, test.data, type="class")
  caKNN <- CA(test.class, predKNN)
  print(paste("5-nearest neighbors: ", caKNN))
  flush.console()
  
  if (compute.crossValidation) {
    res <- errorest(train.class ~ ., data=train.data, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "knn")
    print(paste("Cross validation: ", 1-res$error))
    flush.console()
  }
  
  
  #
  #
  # RANDOM FOREST
  #
  #
  
  modelRF <- CoreModel(train.class ~ ., data = train.data, model="rf") # Random forest
  
  predRF <- predict(modelRF, test.data, type="class")
  caRF <- CA(test.class, predRF)
  print(paste("Random forest: ", caRF))
  flush.console()
  
  if (compute.crossValidation) {
    res <- errorest(train.class ~ ., data=train.data, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "rf")
    print(paste("Cross validation: ", 1-res$error))
    flush.console()
  }
  
  # new function for neural network just for testing
  #}
  
  #neural <- function(train.data, train.class, test.data, test.class) {
  
  #
  #
  # NEURAL NETWORKS
  #
  #
  
  # the algorithm is more robust when scaled data is used
  norm.data <- scale.data(rbind(train.data,test.data[names(train.data)]))
  norm.learn <- norm.data[1:nrow(train.data),]
  norm.test <- norm.data[-(1:nrow(train.data)),]
  
  modelNN <- nnet(train.class ~ ., data = norm.learn, size = 4, decay = 0.0001, maxit = 1000, trace = FALSE)
  predNN <- predict(modelNN, norm.test, type = "class")
  caNN <- CA(test.class, predNN)
  print(paste("Neural networks: ", caNN))
  flush.console()
  
  if (compute.crossValidation) {
    mypredict.nnet <- function(object, newdata){as.factor(predict(object, newdata, type = "class"))}
    res <- errorest(train.class~., data=norm.learn, model = nnet, predict = mypredict.nnet, size = 5, decay = 0.0001, maxit = 10000, trace = FALSE)
    print(paste("Cross validation: ", 1-res$error))
    flush.console()
  }
  
  #models <- c(modelDT, modelNB, modelKNN, modelRF, modelNN)
  predicted <- data.frame(test.class, predDT, predNB, predKNN, predRF, predNN)
}