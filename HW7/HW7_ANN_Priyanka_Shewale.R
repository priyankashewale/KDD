# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : HW7 ANN
###########################################################################################################
##########################################################################################################

rm(list=ls())

library(neuralnet)
library(NeuralNetTools)

setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/HW7")

db = read.csv('wisc_bc_ContinuousVar.csv',header=TRUE, sep=",")
db = db[,-1]
db<-na.omit(db)
summary(db)
View(db)

train_index <- sample(nrow(db),as.integer(.70*nrow(db)))
train_data<-db[train_index,]
test_data<-db[-train_index,]

model <- neuralnet(diagnosis~.,data=train_data,hidden=5, threshold=0.01) # WITH hIDDEN LAYER= 10 accuracy increases to 0.9707 from 0.6374 at hidden layer=5
plotnet(model)

pred<-compute(model ,test_data[,-1])
ann<-c('B','M')[apply(pred$net.result,1,which.max)]


table(Actual=test_data$diagnosis,predition=ann)

inc = (test_data$diagnosis != ann)
accuracy <-1 - sum(inc)/length(inc)
accuracy
