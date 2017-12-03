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
# ATTRIBURE EVALUATION AND CONSTRUCTING NEW ATTRIBUTES
#
##############################################################################

#install.packages('dummies')
library(dummies)


# Convert Site attribute with one hot encoding
md <- dummy.data.frame(md, names=c("Site"), sep="_")


# library to work with date-time
# install.packages('lubridate')
library(lubridate)

date <- md$Date
md$Date <- NULL

month(as.POSIXlt(date, format="%Y-%m-%d"))
a <- data.frame(wday(as.POSIXlt(date, format="%Y-%m-%d"), label=TRUE))
colnames(a) <- c("Dan")
names(a)

a <- dummy.data.frame(a, names=c("Dan"), sep="_")
names(a)

md[names(a)] <- a





