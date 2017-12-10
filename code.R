# setwd(...)

#install.packages('dummies')
#install.packages('lubridate')
#install.packages("CORElearn")  
#install.packages('nnet')
library(dummies)
library(lubridate) # library to work with date-time
library(CORElearn)
library(nnet)
source("myfunctions.R")

##############################################################################
#
# DATA VISUALIZATION
#
##############################################################################



# To read data from a text file, use the "read.table" command.
# The parameter header=TRUE indicates that the file to be read includes a first line with the column names
md <- read.table(file="dataSem1.txt", sep=",", header=TRUE)

# Summarize data
summary(md)
# Get all header names
names(md)
# Number of rows
nrow(md)
# Number of columns
ncol(md)
# First 6 rows in data
head(md)

# Useful data visualization functions
plot(md$Glob_radiation_mean)
hist(md$Glob_radiation_mean)
plot(density(md$Glob_radiation_mean))
boxplot(md$Glob_radiation_mean)
barplot(table(md$Glob_radiation_mean))

##############################################################################
#
# CONSTRUCTING NEW ATTRIBUTES
#
##############################################################################

# Convert Site attribute with one hot encoding
md <- dummy.data.frame(md, names=c("Site"), sep="_")

date <- md$Date
md$Date <- NULL

a <- data.frame(wday(as.POSIXlt(date, format="%Y-%m-%d"), label=TRUE), month(as.POSIXlt(date, format="%Y-%m-%d")), year(as.POSIXlt(date, format="%Y-%m-%d")))
colnames(a) <- c("Dan", "Mesec", "Leto")
names(a)

a <- dummy.data.frame(a, names=c("Dan", "Mesec"), sep="_")
names(a)

md[names(a)] <- a

##############################################################################
#
# PREPARE LEARNING AND TESTING SUBSETS
#
##############################################################################

sel <- sample(1:nrow(md), size=as.integer(nrow(md)*0.7), replace=F)
learn <- md[sel,]
test <- md[-sel,]


##############################################################################
#
# PREDICTION: CLASSIFICATION
#
##############################################################################

getOzoneLevel <- function(ozone) {
  LOW <- 60.0 
  MODERATE <- 120.0 
  HIGH <- 180.0 
  
  ifelse (ozone < LOW, "LOW",
          ifelse (ozone < MODERATE, "MODERATE",
                  ifelse (ozone < HIGH, "HIGH", "EXTREME")))
}

getPM10classes <- function(pm10) {
  LOW <- 35.0 
  
  ifelse (pm10 < LOW, "LOW", "HIGH")
}

ozone <- factor(getOzoneLevel(learn$O3))

PM10 <- factor(getPM10classes(learn$PM10))

learn$O3 <- NULL
learn$Glob_radiation_min <- NULL

# attribute evaluation using information gain
att <- sort(attrEval(ozone ~ ., learn, "InfGain"), decreasing = TRUE)
att <- head(att, 2) # best n attributes
set <- learn[names(att)]

# TRAINING THE MAX OZONE LEVEL (O3)

# build a decision tree using information gain as a splitting criterion
modelDT <- CoreModel(ozone  ~ ., set, model="tree", selectionEstimator="InfGain")
#plot(modelDT, set)

modelNB <- CoreModel(ozone ~ ., set, model="bayes")
modelKNN <- CoreModel(ozone ~ ., set, model="knn", kInNN = 5)
modelRF <- CoreModel(ozone ~ ., data = set, model="rf") # Random forest

# TESTING O3
test.ozone <- getOzoneLevel(test$O3)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test.ozone, predDT)

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test.ozone, predNB)
caNB

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test.ozone, predKNN)
caKNN

predRF <- predict(modelRF, test, type="class")
caRF <- CA(test.ozone, predRF)
caRF

# Combined results
pred <- data.frame(predDT, predNB, predKNN, predRF, test.ozone)
pred

# TRAINING the concentration of large pollution particles (PM10)

# build a decision tree using information gain as a splitting criterion
modelDT <- CoreModel(PM10  ~ ., set, model="tree", selectionEstimator="InfGain")
#plot(modelDT, set)

modelNB <- CoreModel(PM10 ~ ., set, model="bayes")
modelKNN <- CoreModel(PM10 ~ ., set, model="knn", kInNN = 5) 
modelRF <- CoreModel(PM10 ~ ., data = set, model="rf") # Random forest

# TESTING PM10
test.PM10 <- getPM10classes(test$PM10)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test.PM10, predDT)

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test.PM10, predNB)
caNB

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test.PM10, predKNN)
caKNN

predRF <- predict(modelRF, test, type="class")
caRF <- CA(test.PM10, predRF)
caRF


# Combined results
pred <- data.frame(predDT, predNB, predKNN, predRF, test.PM10)
pred




