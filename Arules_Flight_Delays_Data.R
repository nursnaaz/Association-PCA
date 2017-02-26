rm(list=ls(all=TRUE))



library("arules")

# Read the data
flight_Delays = read.csv("FlightDelays.csv", header=T)
str(flight_Delays)

# removing the time
cat_Data <- subset(flight_Delays, select=-c(1))
cat_Data <- data.frame(sapply(cat_Data, function(x){as.factor(x)}))
str(cat_Data)

time_Bins <- ifelse(flight_Delays$CRS_DEP_TIME < 600, 1,
                    ifelse(flight_Delays$CRS_DEP_TIME < 1200, 2,
                           ifelse(flight_Delays$CRS_DEP_TIME < 1800, 3, 4)))
time_Bins <- as.factor(time_Bins)


data <- data.frame(time_Bins, cat_Data)

str(data)
summary(data)
rm(time_Bins, cat_Data, flight_Delays)

# Converting as transactions data
flight <- as(data, "transactions")
itemFrequency(flight)
itemFrequencyPlot(flight)


rules <- apriori(flight,
                 parameter = list(support = 0.06, confidence = 0.6),control = list(verbose=F))

#inspect the rules
inspect(rules)

#find only the subset of rules where the rhs is Flight.Status=0
rules.classfilter1 <- as(subset(rules, subset = rhs %in% "Flight.Status=0"),
                         "data.frame")


#find only the subset of rules where the rhs is Flight.Status=0 & supoort >0.8
rules.classfilter2 <- as(subset(rules, 
                                subset = rhs %in% "Flight.Status=0" & 
                                         support > 0.8),"data.frame")

ruleslisted <- rbind(rules.classfilter1,rules.classfilter2)
#write.csv(ruleslisted,"ruleslisted.csv")
                   


rules.sorted <- sort(rules,by="lift")
rulesImp <- rules.sorted[1:20]
inspect(rulesImp)

install.packages("arulesViz")
library(arulesViz)
library(tcltk)
plot(rulesImp,method="graph",interactive=TRUE,control=list(type="items"))
