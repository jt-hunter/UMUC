# James Taylor
# Exercise 3


# setting the wd
setwd("/Users/jean_taylor_1/Downloads")
# checking the files in wd
dir()
# reading the csv
credit <- read.csv(file= 'CreditApproval_.csv', head=TRUE, sep=",")
# removing uneeded variable
credit$Key <- NULL

# Discretization of numerical variables
# Age
credit$Age <- discretize(credit$Age, method = 'interval', breaks = 6)
summary(credit$Age)

# Debt 
credit$Debt <- discretize(credit$Debt, method = 'interval', breaks = 8)
summary(credit$Debt)

# Years Employed
credit$YearsEmployed <- discretize(credit$YearsEmployed, method = 'interval', breaks = 6)
summary(credit$YearsEmployed)

# Credit Score
credit$CreditScore <- factor(credit$CreditScore, ordered = TRUE)
summary(credit$CreditScore)

# Zip Code
credit$Zipcode <- factor(credit$Zipcode)
str(credit$Zipcode)

# Income
credit$Income <- discretize(credit$Income, method = 'interval', breaks = 8)
summary(credit$Income)

# Apriori methods
rules <- apriori(credit)
inspect(rules[1:10])

# apriori #2
rules <- apriori(credit, parameter= list(supp=0.4, conf=0.99))
rules <- sort(rules, by= "lift")
inspect(rules[1:10])

# apriori #3
rules <- apriori(credit, parameter= list(supp=0.8, conf=.8))
rules <- sort(rules, by= "confidence")

# apriori with class as +/- on rhs
rules <- apriori(credit, appearance=list(rhs=c("class=-", "class=+"), default="lhs"))
rules <- sort(rules, by= "lift")
inspect(rules[1:10])

# marking which rules are redundant
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)


# placing non-redundant rules in new variable
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

# plot
plot(rules.pruned[1:2], method="paracoord", control=list(reorder=TRUE))

