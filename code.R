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
