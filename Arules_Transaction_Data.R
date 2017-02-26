rm(list=ls(all=TRUE))


require(arules)

# Read transaction 'Transactions.csv' data in the way arules package should treat the transaction data
# data<-read.csv("Transactions.csv",header=F)
trans = read.transactions(file="Transactions.csv", 
                          rm.duplicates= FALSE, 
                          format="single",sep=",",
                          cols =c(1,2))


# Check the data read formata
inspect(trans)

# Explore and understand the data and items of transaction data
trans

#How many times each item appeared in the totla number of transactions
itemFrequency(trans)  
itemFrequencyPlot(trans)

# Implementing association mining using 'Apriori' algorithm to extract rules
rules <- apriori(trans,parameter = list(sup = 0.2, conf = 0.6,target="rules"),control = list(verbose=F))

# Understanding the rules
inspect(rules)


# Order of rules by decreasing confidence
as(rules[sort(rules, by = "confidence", order = TRUE)], "data.frame")
rules = as(rules, "data.frame")

# ##Writing rules to a file
# require(stringr)
# m=str_split(rules$rules,"=>")
# Class = data.frame(Class = unlist(lapply(m,function(x){str_trim(x[2])})))
# 
# Rule = data.frame(Rule = unlist(lapply(m,function(x){str_trim(x[1])})))
# 
# rules2= data.frame(rules,Rule,Class)
# rules2 = rules2[,-(which(colnames(rules2)=="rules"))]
# Rulesset = unique(rules2)
# write.csv(Rulesset,"Rules.csv")


