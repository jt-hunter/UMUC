# James Taylor
# Exercise 7

install.packages('cluster')
setwd('/Users/jean_taylor_1/Downloads')
df <- read.csv(file = 'vehicle.csv')


set.seed(1234)
myvehicles <- df
myvehicles$Class <- NULL

myvehicles[1:18]<-scale(myvehicles[1:18])

kc<-kmeans(myvehicles, 10)
kc
kc$iter
kc$totss
kc$betweenss
kc$tot.withinss


table(df$Class, kc$cluster)

clusplot(myvehicles, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
