rm(list=ls(all=TRUE))


# Load required libraries

library(dummy)
library(vegan)

# Set  working directory

setwd("~/Desktop/GNQ3/20170128_Batch25_CSE7405c_Rules_Lab")


attr = c('id', 'age', 'exp', 'inc', 'zip', 'family', 
         'ccavg', 'edu', 'mortgage', 'loan', 
         'securities', 'cd', 'online', 'cc')

# Read the data from csv file
data = read.csv(file = "UniversalBank.csv", 
                header = TRUE, col.names = attr)

str(data)
# Removing the id, zip and experience. 
drop_Attr = c("id", "zip", "exp")
attr = setdiff(attr, drop_Attr)
data = data[, attr]
rm(drop_Attr)

# Convert attribute to appropriate type  
cat_Attr = c("family", "edu", "securities", 
             "cd", "online", "cc", "loan")
num_Attr = setdiff(attr, cat_Attr)
tar_Attr = "loan"

cat_Data = data.frame(sapply(data[,cat_Attr], as.factor))
num_Data = data.frame(sapply(data[,num_Attr], as.numeric))
data <- cbind(cat_Data,num_Data)
rm(cat_Data,num_Data)

#Standardizing all attributes
data[,num_Attr] <- decostand(data[,num_Attr],'range')

# Convert categorical into numerical type using dummy
family <- dummy(x = as.data.frame(data$family),int = T)
edu <- dummy(x=as.data.frame(data$edu),int = T)
securities <- dummy(x=as.data.frame(data$securities),int = T)
cd <- dummy(x=as.data.frame(data$cd),int = T)
online <- dummy(x=as.data.frame(data$online),int = T)
cc <- dummy(x=as.data.frame(data$cc),int = T)


data[,setdiff(cat_Attr,'loan')] <- NULL
data <- cbind(data,family,edu,securities,cd,online,cc)
rm(family,edu,securities,cd,online,cc)


# Logistic Regression Model on the original data
LogReg <- glm(loan~.,data = data,family = "binomial")
summary(LogReg)

pred <- predict(LogReg,data[,-1],type = "response")
logReg_Data <- ifelse(pred > 0.5, 1, 0)
cm_LogReg_Data <- table(logReg_Data,data$loan)
sum(diag(cm_LogReg_Data))/sum(cm_LogReg_Data)

#Applying Principal Component Analysis
data_Pca <- prcomp(data[,-1])
summary(data_Pca)
data_Pca_All_Comp <- data.frame(cbind(data_Pca$x,data$loan))
names(data_Pca_All_Comp)[20] <- 'loan'
data_Pca_All_Comp$loan <- as.factor(data_Pca_All_Comp$loan)

#Logistic Regression using all Components from P.C.A
LogReg <- glm(loan~.,data = data_Pca_All_Comp,family = "binomial")
summary(LogReg)

pred <- predict(LogReg,data_Pca_All_Comp[,-20],type = "response")
logReg_Data_Pca <- ifelse(pred > 0.5, 1, 0)
cm_LogReg_Data_Pca <- table(logReg_Data_Pca,data_Pca_All_Comp$loan)
sum(diag(cm_LogReg_Data_Pca))/sum(cm_LogReg_Data_Pca)

#Taking 95% of variance using P.C.A
summary(data_Pca)
#Taking 11 Components
data_Pca_less_Comp <- data.frame(cbind(data_Pca$x[,c(1:11)],data$loan))
names(data_Pca_less_Comp)[12] <- 'loan'
data_Pca_less_Comp$loan <- as.factor(data_Pca_less_Comp$loan)

#Logistic Regression using 11 Components from P.C.A
LogReg <- glm(loan~.,data = data_Pca_less_Comp,family = "binomial")
summary(LogReg)

pred <- predict(LogReg,data_Pca_less_Comp[,-12],type = "response")
logReg_Data_Pca <- ifelse(pred > 0.5, 1, 0)
cm_LogReg_Data_Pca <- table(logReg_Data_Pca,data_Pca_All_Comp$loan)
sum(diag(cm_LogReg_Data_Pca))/sum(cm_LogReg_Data_Pca)
