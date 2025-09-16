# James Taylor
# Exercise 2

# this is working with CreditApproval dataset

# set wd 
setwd("/Users/jean_taylor_1/Downloads")
# find csv of dataset
dir()
# load data
credit <- read.csv(file="CreditApproval_.csv", head=TRUE, sep=",")
# View first 10 rows
head(credit, 10)
# check structures of features
str(credit)
# summary of dataset
summary(credit)

# Discretization
# Unsupervised discretization 
credit$Age <- discretize(credit$Age, method = 'interval', breaks = 9)
summary(credit$Age)
# equal frequency
credit$YearsEmployed <- discretize(credit$YearsEmployed, method="frequency", breaks=7)
summary(credit$YearsEmployed)
# K-means clustering
credit$Income <- discretize(credit$Income, method="cluster", breaks=10)
summary(credit$Income)

# Removing variable
dim(credit)
newcredit <- credit[, -1]
dim(newcredit)

# missing values
nrow(credit[!complete.cases(credit),])
apply(credit, 2, function (credit) sum(is.na(credit)))

# replace empty values with mean of that variable
# DISCLAIMER: the Age variable is discretized at this point, rerun the read.csv if error occurs below 
credit$Age[is.na(credit$Age)]<-mean(credit$Age, na.rm=TRUE)

# sorting by age
credit<-credit[order(credit$Age), ]
head(credit, 10)

# plot
plot(table(credit$Married), type = 'h')

     