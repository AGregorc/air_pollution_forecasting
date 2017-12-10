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

  # linear model
  lm.model <- lm(learn$O3 ~ ., data = learn)
  lm.model

  predicted <- predict(lm.model, test)
  mae(test$O3, predicted)
  rmae(test$O3, predicted, mean(learn$O3))

  # regression tree
  rt.model <- rpart(learn$O3 ~ ., learn)
  predicted <- predict(rt.model, test)
  rmae(test$O3, predicted, mean(learn$O3))

  plot(rt.model);text(rt.model, pretty = 0)

}