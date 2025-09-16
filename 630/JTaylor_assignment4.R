# james taylor
# assignmnat 4


setwd('/Users/jean_taylor_1/Downloads')
df <- read.csv(file="wdbc.csv", head=TRUE, sep=",")

summary(df)
# finding missing values
apply(df, 2, function (df) sum(is.na(df)))
# removing uneeded variable
df$ID <- NULL
# convert dependent variable to 0s and 1s
df$diagnosis <- as.character(df$diagnosis)
df <- df %>%
  mutate(diagnosis = ifelse(diagnosis == "M",1,0))
# noramalize all variables besides dependent variable
df[2:31]<-scale(df[2:31])
# partiion into train and test
set.seed(1234)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.8, 0.2))
df.train <- df [ind == 1, ]
df.test <- df [ind == 2, ]
rm(ind)
# plots
ggplot(df, aes(diagnosis)) +
  geom_bar() +
  scale_x_discrete(name= '',
                      breaks=c('M', 'B'),
                      labels=c('Malignant', 'Benign'))

ggplot(df, aes(x = diagnosis, y = texture)) +
  geom_violin() +
  scale_x_discrete(name= '',
                   breaks=c('M', 'B'),
                   labels=c('Malignant', 'Benign'))

ggplot(df, aes(x = diagnosis, y = symmetry)) +
  geom_violin() +
  scale_x_discrete(name= '',
                   breaks=c('M', 'B'),
                   labels=c('Malignant', 'Benign'))

ggplot(df, aes(x = diagnosis, y = compactness)) +
  geom_violin() +
  scale_x_discrete(name= '',
                   breaks=c('M', 'B'),
                   labels=c('Malignant', 'Benign'))

ggplot(df[df$diagnosis == 'M',]) +
  geom_violin(aes(x = diagnosis, y = concavity)) +
  scale_y_continuous(limits = c(0,1.2))

# first model
nn<-neuralnet(formula = diagnosis~., data = df.train, hidden = c(3,2), err.fct ="ce", linear.output = FALSE)

nn$response[1:10]
nn$net.result[[1]][1:10] 
# round predictions
nn.predictions<-compute(nn, nn$covariate)$net.result
nn.predictions<-apply(nn.predictions, c(1), round)
nn.predictions[1:10]

# confusion matrix for the training set
table(nn.predictions, df.train$diagnosis, dnn =c("Predicted", "Actual"))
mean(nn.predictions==df.train$diagnosis)
# finding which observations are wrong
nn.test.predictions <- compute(nn, df.test)$net.result
nn.test.predictions <- apply(nn.test.predictions, c(1), round)
table(nn.test.predictions, df.test$diagnosis, dnn =c("Predicted", "Actual"))
mean(nn.test.predictions == df.test$diagnosis)
nn.test.predictions
df.test$diagnosis
# second model
rm(nn2)
nn2 <- neuralnet(formula = diagnosis~., data = df.train, hidden = c(5,3,2), err.fct ="ce", linear.output = FALSE)
# sonfusion of test data for 2nd model
nn.test.predictions2 <- compute(nn2, df.test)$net.result
nn.test.predictions2 <- apply(nn.test.predictions2, c(1), round)
table(nn.test.predictions2, df.test$diagnosis, dnn =c("Predicted", "Actual"))
mean(nn.test.predictions2 == df.test$diagnosis)

nn.test.predictions2
df.test$diagnosis

