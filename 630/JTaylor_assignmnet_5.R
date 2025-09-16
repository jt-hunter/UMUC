# James Taylor
# assignmnet 5

# set WD and load data
setwd('/Users/jean_taylor_1/Downloads')
df <- read.csv(file = 'ecoli.csv')

# look fr missing data
apply(df, 2, function (df) sum(is.na(df)))

# load data into data frame for trainning
# normalize the data
df.train <- df
df.train$class <- NULL
df.train[1:7] <- scale(df.train[1:7])

# new df with 2 variables removed
df.train.minus.lip.chg <- df.train
df.train.minus.lip.chg$lip <- NULL
df.train.minus.lip.chg$chg <- NULL

# counts of the bacteria types
table(df$class)

# plots
ggplot(df, aes(x = class, y = mcg, color = class)) +
  geom_violin() +
  geom_jitter()


ggplot(df, aes(x = class, y = gvh, color = class)) +
  geom_violin() +
  geom_jitter()

  
ggplot(df, aes(x = class, y = alm1, color = class)) +
  geom_violin() +
  geom_jitter()


table(df$lip)

# first model all variables 8 clusters
kc<-kmeans(df.train, 8) 
# test accuracy
table(df$class, kc$cluster)

# seond model with less variables
kc2 <- kmeans(df.train.minus.lip.chg, 8)
# test accuracy
table(df$class, kc2$cluster)

# third model with less varibles and 12 clusters
kc3 <- kmeans(df.train.minus.lip.chg, 12)
# test accuracy
table(df$class, kc3$cluster)

# custer plot
clusplot(df.train.minus.lip.chg, kc3$cluster, color=TRUE, shade=TRUE, labels=4, lines=0)

