# James Taylor
# Assignment 1


# setting the wd
setwd("/Users/jean_taylor_1/Downloads")
# checking the files in wd
dir()
# reading both CSV's
student1 <- read.csv(file= 'student-mat.csv', head=TRUE, sep=";")
student2 <- read.csv(file= 'student-por.csv', head=TRUE, sep=",")
#Combining CSV's
student <- rbind(student1, student2)

# check for redundant students in both CSV based on variables below
sum(duplicated(student[c("school","sex","age","address","famsize","Pstatus",
                           "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
                           "guardian","guardian","traveltime","studytime","failures",
                           "schoolsup","famsup","activities","higher","romantic",
                           "famrel","freetime","goout","Dalc","Walc","health","absences")]))

# remove redundant students
student <- student[!duplicated(student[c("school","sex","age","address","famsize","Pstatus",
                                         "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
                                         "guardian","guardian","traveltime","studytime","failures",
                                         "schoolsup","famsup","activities","higher","romantic",
                                         "famrel","freetime","goout","Dalc","Walc","health","absences")]),]

dim(student)

# inspecting structure of variables
str(student)
# find NA's
apply(student, 2, function (student) sum(is.na(student)))

# pre-processing

# discretization or as factorial from numerical
student$age <- discretize(student$age, method = 'interval', breaks = 4)
student$Medu <- as.factor(student$Medu)
student$Fedu <- as.factor(student$Fedu)
student$traveltime <- as.factor(student$traveltime)
student$studytime <- as.factor(student$studytime)
student$failures <- as.factor(student$failures)
student$famrel <- as.factor(student$famrel)
student$freetime <- as.factor(student$freetime)
student$goout <- as.factor(student$goout)
student$Dalc <- as.factor(student$Dalc)
student$Walc <- as.factor(student$Walc)
student$health <- as.factor(student$health)
student$absences <- discretize(student$absences, method = 'interval', breaks = 15)
student$G1 <- discretize(student$G1, method = 'interval', breaks = 5)
student$G2 <- discretize(student$G2, method = 'interval', breaks = 5)
student$G3 <- discretize(student$G3, method = 'interval', breaks = 5)

# check structure of df after transformations
str(student)



# Exploritory Analysis

# counts of weekday alc consumption
ggplot(student, aes(Dalc)) +
  geom_bar() + 
  scale_x_discrete(name = "Weekday Alcohol Consumption", breaks=c("1","2", "3", "4", "5"),
                   labels=c("Very Low", "Low", "Medium", "High", "Very High"))


ggplot(student[student$Dalc == 5 | student$Dalc == 4,], aes(absences)) +
  geom_bar() +
  scale_x_discrete(name = "Absences with 'High' or 'Very High' Weekday Alcohol Consumption")
      

ggplot(student[student$Dalc == 5 | student$Dalc == 4,], aes(sex)) +
  geom_bar() +
  scale_x_discrete(name = "Sex", breaks=c("M", "F"),
                  labels=c("Male", "Female"))


ggplot(student[student$Dalc == 5 | student$Dalc == 4,], aes(famrel)) +
  geom_bar() +
  scale_x_discrete(name = "Family Relationship", breaks=c('1', '2', '3', '4', '5'),
                   labels=c("Very Low", "Low", "Medium", "High", "Very High"))

# apriori algorythm
rules <- apriori(student, appearance=list(rhs=c("Dalc=5", "Dalc=4", "Walc=5", "Walc=4"), default="lhs"),
                 parameter= list(supp=0.01, conf=.9))
#removing redundant rules
rules.sorted <- sort(rules, by="lift")
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1

# placing non-redundant rules in new variable
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
#plot
plot(rules.pruned[1:6], method = "graph")


