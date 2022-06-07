rm(list=ls())
library(class)

setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/Midterm")
ibm <- read.csv(file = 'IBM_Attrition_v2.csv',header=TRUE, sep=",")
ibm <- na.omit(ibm)

summary(ibm)



#Training & Test Data (30% test & 70% Training)
trainIndex <- sample(1:nrow(ibm), round(0.7 * nrow(ibm))) 


# output column(expected)
target = ibm['Attrition']

#train set
train <- ibm[trainIndex,] 
trainTarget <- target[trainIndex,]

##test set
test <- ibm[-trainIndex,] 
testTarget <- target[-trainIndex,]


#KNN with k = 3
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
clf <- knn(train,test,cl=trainTarget,k=3)

conf_matrix <- table(clf, testTarget)
print("confidence matrix for k=3:")
print(conf_matrix)
accuracy(conf_matrix) # accuracy from confidence matrix

