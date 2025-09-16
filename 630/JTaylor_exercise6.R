# Exercise 6
# James Taylor

# get data
setwd('/Users/jean_taylor_1/Downloads')
df <- read.csv(file="column.csv", head=TRUE, sep=",")

# normalize all input variables
df[1:6]<-scale(df[1:6])
head(df)

# partition data
set.seed(12345)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
df.train <- df[ind == 1, ]
df.test <- df[ind == 2, ]
rm(ind)
dim(df.train)
dim(df.test)

# train neural network
nn<-neuralnet(formula = class~.,
              data = df.train,
              hidden=4,
              err.fct="ce",
              linear.output = FALSE)

nn
nn$result.matrix
nn$net.result[[1]][1:10]

# plot
plot(nn)

# get predictions on train data
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
mypredict[1:10]
# get confusion on train data
table(mypredict, df.train$class, dnn =c("Predicted", "Actual"))
mean(mypredict==df.train$class)

# get predictions and confusion of test data
testPred <- compute(nn, df.test[, 0:6])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, df.test$class, dnn =c("Predicted", "Actual"))
mean(testPred==df.test$class)



