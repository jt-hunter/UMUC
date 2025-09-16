# James Taylor
# Exercise 4

setwd("/Users/jean_taylor_1/Downloads")
# import csv
imports <- read.csv(file= 'imports-85.csv', head=TRUE, sep=",")
# removing unwanted 
dim(imports)
imports[c('engine_type', 'make', 'num_of_cylinders', 'fuel_system')] <- list(NULL)
dim(imports)

# additional preprocessing
imports[c('symboling', 'normalized_losses')] <- list(NULL)

apply(imports, 2, function (imports) sum(is.na(imports)))


set.seed(1234)
ind <- sample(2, nrow(imports), replace = TRUE, prob = c(0.7, 0.3))
train.imports <- imports [ind == 1, ]
test.imports <- imports [ind == 2, ]
dim(test.imports)
dim(train.imports)

myFormula<-Price~.
model<-lm(myFormula, data=train.imports)
summary(model)


pred <- predict(model,  newdata=test.imports)
plot(test.imports$Price, pred, xlab = "Observed", ylab = "Prediction")
abline(a = 0, b = 1)

plot (model)

model2<-step(model, direction="backward")

summary(model2)



