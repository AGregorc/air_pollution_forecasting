loadLibraries <- function()
{
  #install.packages('dummies')
  #install.packages('lubridate')
  #install.packages("CORElearn")  
  #install.packages('nnet')
  #install.packages('ipred')
  library(dummies)
  library(lubridate)
  library(CORElearn)
  library(nnet)
  library(rpart)
  
  # the library ipred is needed to perform cross-validation
  library(ipred)
  
  # regression libraries
  #install.packages(c('randomForest', 'e1071', 'kknn'))
  library(randomForest)
  library(e1071)
  library(kknn)
}