# Course      : CS 513
# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : MIDTERM Question 6
###########################################################################################################
##########################################################################################################


rm(list=ls())
library(class)

setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/Midterm")
ibm <- read.csv(file = 'IBM_Attrition_v2.csv',header=TRUE, sep=",")
ibm <- na.omit(ibm)

summary(ibm)
class(ibm)

ibm$Attrition<- factor(ibm$Attrition , levels = c("Yes","No") , labels = c("1","0"))
class(ibm$Attrition)

as.numeric(ibm$Attrition)
train_index <- sample(nrow(ibm),as.integer(.70*nrow(ibm)))


train_data<-ibm[train_index,]
test_data<-ibm[-train_index,]
View(ibm)

target = ibm["Attrition"]
trainTarget = target[train_index,]

testTarget = target[-train_index,]

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

clf <- knn(train_data,test_data,cl=trainTarget,k=3)

conf_matrix <- table(clf, testTarget)
print(conf_matrix)
accuracy(conf_matrix)

