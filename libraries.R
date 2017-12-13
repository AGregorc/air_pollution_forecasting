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
}