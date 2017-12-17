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


md['Glob_radiation_min'] <- NULL
sort(attrEval(md$O3 ~ ., md, "MSEofMean"), decreasing = TRUE)
best <- sort(attrEval(md$O3 ~ ., md, "RReliefFexpRank"), decreasing = TRUE)
o3 <- md['O3']
md <- md[names(best[1:20])]
md['O3'] <- o3

date <- md$Date
md$Date <- NULL

a <- data.frame(wday(as.POSIXlt(date, format="%Y-%m-%d")))
b <- data.frame(month(as.POSIXlt(date, format="%Y-%m-%d")))
c <- data.frame(year(as.POSIXlt(date, format="%Y-%m-%d")))
d <- data.frame(toSeason(date))
e <- data.frame(as.POSIXlt(date)$mday)
f <- data.frame(as.numeric(as.POSIXlt(date, format="%Y-%m-%d")) - as.numeric(as.POSIXlt("2013-1-1", format="%Y-%m-%d")))
colnames(a) <- "Dan"
colnames(b) <- "Mesec"
colnames(c) <- "Leto"
colnames(d) <- "LetniCas"
colnames(e) <- "DanVMesecu"
colnames(f) <- "Čas"
#a$Leto <- NULL

ad <- dummy.data.frame(a, names="Dan", sep="_")
bd <- dummy.data.frame(b, names="Mesec", sep="_")
dd <- dummy.data.frame(d, names="LetniCas", sep="_")
names(ad)
names(bd)
names(c)
names(dd)


md[names(ad)] <- ad
md[names(bd)] <- bd
md[names(dd)] <- dd

md[names(a)] <- a
md[names(b)] <- b
md[names(c)] <- c
md[names(d)] <- d
md[names(e)] <- e
md[names(f)] <- f

# This is just for testing purpose! 
md[names(a)] <- NULL
md[names(b)] <- NULL
md[names(c)] <- NULL
md[names(d)] <- NULL
md[names(e)] <- NULL
md[names(f)] <- NULL

md[names(ad)] <- NULL
md[names(bd)] <- NULL
md[names(dd)] <- NULL

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

classification(learning, testing, FALSE)
regression(learning, testing)
