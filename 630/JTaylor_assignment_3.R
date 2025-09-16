# James Taylor
# assignmnet 3


setwd("/Users/jean_taylor_1/Downloads")

df = read.csv(file = 'bank-marketing.csv')

apply(df, 2, function (df) sum(is.na(df)))

summary(df)
str(df)

# preprocessing
df$yearly.balance <- as.integer(df$yearly.balance)

levels(df$passed.days) <- c(levels(df$passed.days), '0')
df$passed.days[df$passed.days == '?'] <- '0'
df$passed.days <- as.integer(df$passed.days)
df$passed.days[df$passed.days == 293] <- 0

df$last.contact.day <- NULL
df$last.contact.type <- NULL
df$last.contact.month <- NULL
df$last.contact.duration.seconds <- NULL

# partition into test and train set
set.seed(1234)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
df.train <- df [ind == 1, ]
df.test <- df [ind == 2, ]
rm(ind)


# Exploratory visualizations
ggplot(df, aes(x= age, fill= as.factor(suscribed.term.deposited))) +
  geom_bar(binwidth = 5, position = 'fill', alpha=.5) +
  scale_fill_discrete(name= '',
                      breaks=c('yes', 'no'),
                      labels=c('Yes', 'No')) +
  scale_y_continuous(name = 'Percent',
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Percent of CDs Achieved by End of Campaign") +
  scale_x_continuous(name= 'Age')



ggplot(df, aes(x = suscribed.term.deposited, 
               fill= as.factor(education))) +
  geom_bar(position = 'fill', alpha=.5) +
  scale_fill_discrete(name= 'Education Level') +
  scale_y_continuous(name = 'Percent',
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Percent of CDs")


ggplot(df, aes(x = yearly.balance, 
               fill= as.factor(suscribed.term.deposited))) +
  geom_bar(binwidth = 100, position = 'fill', alpha=.5) +
  scale_y_continuous(name = 'Percent',
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_discrete(name = '',
                      breaks = c('yes', 'no'),
                      labels = c('Got CD', 'Didnt get CD')) +
  ggtitle('Percent Who got CDs by Account Balance') +
  scale_x_continuous(name = 'Yearly Account Balance')

ggplot(df, aes(x = job, 
               fill= as.factor(suscribed.term.deposited))) +
  geom_bar(position = 'fill', alpha=.5) +
  scale_y_continuous(name = 'Percent',
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_discrete(name = '',
                      breaks = c('yes', 'no'),
                      labels = c('Got CD', 'Didnt get CD')) +
  ggtitle('Percent Who got CDs by Job') +
  scale_x_discrete(name = 'Job') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

ggplot(df, aes(x = outcome.previous.campaign, 
               fill= as.factor(suscribed.term.deposited))) +
  geom_bar(position = 'fill', alpha=.5) +
  scale_y_continuous(name = 'Percent',
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_discrete(name = '',
                      breaks = c('yes', 'no'),
                      labels = c('Got CD', 'Didnt get CD')) +
  ggtitle('Percent Who got CDs by Previous Campaigns') +
  scale_x_discrete(name = 'Outcome of Previous Campaign')
  theme(axis.text.x = element_text(angle = 0, hjust = .5))


ggplot(df[df$current.campaign.contacts < 25,], aes(x = current.campaign.contacts, 
               fill= as.factor(suscribed.term.deposited))) +
  geom_bar(position = 'fill', alpha=.5, binwidth = 2) +
  scale_y_continuous(name = 'Percent',
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_discrete(name = '',
                      breaks = c('yes', 'no'),
                      labels = c('Got CD', 'Didnt get CD')) +
  ggtitle('Percent Who got CDs by Campaigns Contacts') +
  scale_x_continuous(name = 'Number of Campaign Contacts')
theme(axis.text.x = element_text(angle = 0, hjust = .5))

# model with all variables available
model <- ctree(suscribed.term.deposited ~. , data = df.train)

plot(model)  

# confusion on train data
prediction1train <- (predict(model))  
table(prediction1train, df.train$suscribed.term.deposited, dnn=c("predicted", "actual")) 

# confusion on test data
prediction1test <- (predict(model, df.test))
table(prediction1test, df.test$suscribed.term.deposited, dnn=c("predicted", "actual")) 


# 2nd model with certian variables included
model2 <- ctree(suscribed.term.deposited ~ age + education + outcome.previous.campaign + job,
               data = df.train)

plot(model2, type = 'simple')

# confusion on test data of 2nd model
prediction2test <- (predict(model2, df.test)) 
table(prediction2test, df.test$suscribed.term.deposited, dnn=c("predicted", "actual")) 


