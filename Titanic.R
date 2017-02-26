rm(list=ls())

library(arules)
library(arulesViz)

#load the dataset
load("titanic.raw.rdata")
str(titanic.raw)

head(titanic.raw)
summary(titanic.raw)


#build the apriori algorithm
rules.all <-  apriori(titanic.raw,control = list(verbose=F))

inspect(rules.all)


#build the apriori algorithm with certain constraints
rules <- apriori(titanic.raw,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No",
                                         "Survived=Yes"),
                                   default="lhs"))
inspect(rules)

#sort the rules by lift
rules.sorted = sort(rules,by="lift")
inspect(rules.sorted)

#plot the rules
plot(rules,method="graph",interactive=TRUE,control=list(type="items"))

#removing redundancy
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1

## which rules are redundant
which(redundant)

#removing the redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#The rule states only that all children of class 2 survived, but provides no information at all to compare the survival rates of diferent classes.
rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child", "Age=Adult")))
rules.sorted = sort(rules,by="confidence")
inspect(rules.sorted)

