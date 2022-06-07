# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : MIDTERM Question 4
###########################################################################################################
##########################################################################################################

rm(list=ls())
library(e1071)

setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/Midterm")
ibm <- read.csv(file = 'IBM_Attrition_v2.csv',header=TRUE, sep=",")
ibm <- na.omit(ibm)

summary(ibm)

ibm$MonthlyIncome <- cut(ibm$MonthlyIncome, breaks = c(-1,2999, 5000, 8500,19566),
                         labels = c("up to3,000", "3000 up to 5,000.00", "5000 up to 8,500.00", "8500 or more"))


ibm$Age <- cut(ibm$Age, breaks = c(-1,30, 37, 47,95),
               labels = c("less than 31", "31 to 38", "38 to 48", "48 or over"))

train_index <- sample(nrow(ibm),as.integer(.70*nrow(ibm)))
train_data<-ibm[train_index,]
test_data<-ibm[-train_index,]

View(ibm)

model <- naiveBayes(Attrition~., data=train_data)
prediction <- predict(model, test_data)
conf_matrix <- table(predict_nb = prediction, class = test_data$Attrition)
print(conf_matrix)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)
rm(list=ls())

