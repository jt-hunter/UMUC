# James Taylor
# exercise 5

setwd("/Users/jean_taylor_1/Downloads")

df = read.csv(file = 'CreditApproval_.csv')
str(df)

# finding missing variables
apply(df, 2, function (df) sum(is.na(df)))

# remove Nas
df <- na.omit(df)

df$Key <- NULL


# train and test data
set.seed(1234)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
df.train <- df [ind == 1, ]
df.test <- df [ind == 2, ]
dim(df.train)
dim(df.test)

install.packages("party")
# make model
model <- ctree(class~., data = df.train)
print(model)
# visualization
plot(model)
# confusion matrix on train data
prop.table(table(predict(model), df.test$class))
# confusion matrix on test data
testPred <- predict(model, newdata = df.test)
prop.table(table(testPred, df.test$class))

