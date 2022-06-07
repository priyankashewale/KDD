# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : HW9 SVM
###########################################################################################################
##########################################################################################################

rm(list=ls())
setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/HW9_SVM")

# Reading and removing NAs and iD column
db = read.csv('wisc_bc_ContinuousVar.csv',header=TRUE, sep=",")
db = db[,-1]
db<-na.omit(db)
summary(db)

#Splitting into training and testing
train_index <- sample(nrow(db),as.integer(.70*nrow(db)))
train_data<-db[train_index,]
test_data<-db[-train_index,]

#Factorizing
train_data$diagnosis <- factor(train_data$diagnosis)
test_data$diagnosis <- factor(test_data$diagnosis)

#SVM model building
library(e1071)
svm.model <- svm( diagnosis~ ., data = train_data  )
svm.pred <- predict(svm.model, test_data )

#Model parameters
table(actual=test_data[,1],svm.pred )
SVM_wrong<- (test_data$diagnosis!=svm.pred)
rate<-sum(SVM_wrong)/length(SVM_wrong)
accuracy = 1-rate
accuracy
print(svm.model)
