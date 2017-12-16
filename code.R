# setwd(...)

source("myfunctions.R")
source("libraries.R")
source("classification.R")
source("regression.R")

loadLibraries()

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
#plot(md$Glob_radiation_mean)
#hist(md$Glob_radiation_mean)
#plot(density(md$Glob_radiation_mean))
#boxplot(md$Glob_radiation_mean)
#barplot(table(md$Glob_radiation_mean))

##############################################################################
#
# CONSTRUCTING NEW ATTRIBUTES
#
##############################################################################

# Convert Site attribute with one hot encoding
md <- dummy.data.frame(md, names=c("Site"), sep="_")

date <- md$Date
md$Date <- NULL

a <- data.frame(wday(as.POSIXlt(date, format="%Y-%m-%d")), month(as.POSIXlt(date, format="%Y-%m-%d")), year(as.POSIXlt(date, format="%Y-%m-%d")))
colnames(a) <- c("Dan", "Mesec", "Leto")
#a$Leto <- NULL
names(a)

a <- dummy.data.frame(a, names=c("Dan", "Mesec"), sep="_")
names(a)

md[names(a)] <- a

##############################################################################
#
# PREPARE LEARNING AND TESTING SUBSETS
#
##############################################################################

#sel <- sample(1:nrow(md), size=as.integer(nrow(md)*0.7), replace=F)
learning <- md[md$Leto != 2016,]
testing <- md[md$Leto == 2016,]
summary(testing)
##############################################################################
#
# PREDICTION: CLASSIFICATION
#
##############################################################################

classification(learning, testing, TRUE)
regression(learning, testing)


