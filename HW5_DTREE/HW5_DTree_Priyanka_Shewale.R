# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : HW05_DTree
###########################################################################################################
##########################################################################################################
rm(list=ls())

library(class)
library(rpart)
library(rattle)

setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/R_code")

dat = read.csv('breast-cancer-wisconsin.csv',header=TRUE, sep=",")
summary(dat)

dat<-na.omit(dat)

dat$Class <- factor(dat$Class, levels = c(2, 4), labels = c("benign", "malignant"))
dat<- dat[2:11]
class(dat$Class)

View(dat)

train_index <- sample(nrow(dat),as.integer(.70*nrow(dat)))
train_data<-dat[train_index,]
test_data<-dat[-train_index,]


Dtree_class <- rpart(Class ~ ., data = train_data)
prediction <- predict(Dtree_class, test_data, type = "class")

conf_matrix <- table(prediction,test_data$Class)
print("confidence matrix ")
print(conf_matrix)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
print("accuracy is ")
accuracy(conf_matrix)
wrong<- sum()
error_rate <- 
fancyRpartPlot(Dtree_class)
rm(list=ls())
