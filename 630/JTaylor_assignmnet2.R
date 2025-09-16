# James Taylor
# Assignmnet 2

dir()
setwd('/Users/jean_taylor_1/Downloads')

df <- read.csv(file = 'whas500.csv', header = TRUE, sep = ',')

# check structure
str(df)
# checking for NA's
apply(df, 2, function (df) sum(is.na(df)))

# removing uneeded variables
df$id <- NULL
# removing dates variables
df$admitdate <- NULL
df$disdate <- NULL
df$fdate <- NULL
df$year <- NULL

# how many observations had patient die in hospital
sum(df$dstat) # 39

# removes observation where dstat is 1 - observations where patient died in hospital
df <- subset.data.frame(df, subset = df$dstat==0)
df$dstat <- NULL

summary(df)

# graphics
ggplot(df, aes(x=age, fill=as.factor(gender))) +
  geom_histogram(binwidth = 5, alpha=.5) +
  scale_fill_discrete(name= 'Gender',
                      breaks=c(0, 1),
                      labels=c('Male', 'Female')) +
  ggtitle("Age Distribution by Gender")


ggplot(df, aes(x = age, fill = as.factor(fstat))) +
  geom_bar(binwidth = 5, position = 'fill', alpha=.5) +
  scale_fill_discrete(name= 'At Follow-up',
                      breaks=c(0, 1),
                      labels=c('Alive', 'Dead')) +
  scale_y_continuous(name = 'percent',
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Percent of Alive/Dead at Follow-up by Age")



ggplot(df, aes(x= age, y= bmi, color = as.factor(miord))) +
  geom_point() +
  scale_color_discrete(name= 'At Follow-up',
                      breaks=c(0, 1),
                      labels=c('Alive', 'Dead')) +
  scale_y_continuous(name = 'BMI ( kg/m^2 )')
  
  
ggplot(df, aes(lenfol, fill = as.factor(fstat))) +
  geom_histogram(binwidth = 100,  alpha = .5) +
  scale_x_continuous(name = 'Length of Follow-up (Days)') +
  scale_fill_discrete(name= 'At Follow-up',
                      breaks=c(0, 1),
                      labels=c('Alive', 'Dead'))
  
str(df)

# getting into train and test data
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.85, 0.15))
train.df <- df [ind == 1, ]
test.df <- df [ind == 2, ]

# first model
model <- glm(fstat ~. , data = train.df, family = binomial())

summary(model)
# predictins of fisrst model
predictions <- round(predict(model, type="response"), )
# table showing ccuracy on trainning data
table(predictions, train.df$fstat, dnn=c("predicted", "actual")) #333 correct


# model 2 is model 1 but reduced to significant coefficents
model2 <- step(model, direction = 'backward')

summary(model2)

# checking accuarcy of medel 2
predictions2 <- round(predict(model2, type="response"))

table(predictions2, train.df$fstat, dnn=c("predicted", "actual")) #336 correct


# TEST DATA
# model 1
prediction.test.1 <- round(predict(model, test.df, type="response"))
# model 2
prediction.test.2 <- round(predict(model2, test.df, type="response"))

# check accuracy
# model 1
table(prediction.test.1, test.df$fstat, dnn=c("predicted", "actual")) #56 correct
# model 2
table(prediction.test.2, test.df$fstat, dnn=c("predicted", "actual")) #56 correct

# round threshold to .35 on reduced model
prediction.test.2.80percent.threshold <- ifelse((predict(model2, test.df, type="response")) <= .8, 0, 1)
# confusion matrix
table(prediction.test.2.80percent.threshold, test.df$fstat, dnn=c("predicted", "actual"))

# ROC curve for model 2 round up at .5
ROCRpred <- prediction(prediction.test.2, test.df$fstat)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray") 

# residuals plot 0f model 2 on training data
plot(predict(model2, type = 'response'),residuals(model2, type = 'response'), col=c("blue"))
lines(lowess(predict(model2, type = 'response'),residuals(model2, type = 'response')), col=c("black"), lwd=2)
abline(h=0, col="grey")

