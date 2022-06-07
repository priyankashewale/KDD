# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : Final Exam Question 1 & Question 2
###########################################################################################################
##########################################################################################################

#install.packages("writexl")
rm(list=ls())
setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/Finalexam")

# Reading and removing NAs and iD column
db = read.csv('IBM_Attrition_v3.csv',header=TRUE, sep=",")
#db = db[,-1]
db<-na.omit(db)                                                           # a)	Delete all the rows with missing value.
summary(db)
View(db)

db$MonthlyIncome <- cut(db$MonthlyIncome, breaks = c(-1,2900, 5000, 8500,19566),  #b)	Create four categories (income1, income2, income3, income4) based on monthly income
                        labels = c("income1", "income2", "income3", "income4"))

db$YearsAtCompany <- cut(db$YearsAtCompany, breaks = c(-1,6,32),         #c)	Create two categories (senior, not-senior) for years at the company 
                         labels = c("not-senior", "senior"))

db$Age <- cut(db$Age, breaks = c(-1,37,95),                             #d)	Create two categories (young, mature) for age
              labels = c("young", "mature"))
View(db)


library("writexl")
write_xlsx(db,"C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/Finalexam\\Attrition_Modified.xlsx")

################################################################################################################
################################################################################################################
#################################     Q2  RANDOM FOREST    #####################################################
################################################################################################################
################################################################################################################
# Random Forest
library('randomForest')
require(sqldf)
#install.packages("sqldf")
library(sqldf)
#install.packages("dplyr")              # Install & load dplyr package
library("dplyr")

#db$Attrition <-factor(db$Attrition,levels = c(1, 0), labels = c("Yes", "No"))
#str(db$Attrition)
#db = db[-1]

View(db)

db_test <- db[seq(1, nrow(db), 4), ]    #test and training datasets, by selecting every fourth record,
db_train<-anti_join(db, db_test)       #starting from the first observation, as the test dataset and the remaining records as the training dataset


#db_train <- setdiff(db,db_test)
#db_train <- sqldf('SELECT * FROM db EXCEPT SELECT * FROM db_test')
#help(sample)



rf<-randomForest(factor(Attrition)~.,data = db_train, importance = TRUE, ntree=1000)
varImpPlot(rf)
importance(rf)

pred <- predict(rf, db_test)
table(actual=db_test[,6],pred)


inc <- sum(db_test[,6] != pred)
accuracy <- 1 - inc/length(db_test[,6])
print("Accuracy is : ")                              # ACCURACY 
accuracy


wrong<- (db_test[,6]!=pred )
error_rate<-sum(wrong)/length(wrong)
print("Error rate   : ")
error_rate 


rm(list=ls())



