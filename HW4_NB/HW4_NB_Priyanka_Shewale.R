# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : HW04_NB
###########################################################################################################
##########################################################################################################

rm(list = ls())

getwd()
setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/R_code")
library(class)

df = read.csv('breast-cancer-wisconsin.csv',header=TRUE, sep=",")
summary(df)


# Summary of F6 is missing as it has non numeric values
temp <- as.numeric(as.character(df$F6))
df$F6 <- temp
df <- na.omit(df) # F6 NA values omitted.
summary(df)


df$Class <- factor(df$Class, levels = c(2, 4), labels = c("benign", "malignant"))
df<- df[2:11]
class(df$Class)

View(df)

data_div <- sample(nrow(df),as.integer(.70*nrow(df)))   # 70 % training data & 30 % test data 
train<-df[data_div,]
test<-df[-data_div,]

model <- naiveBayes(Class~., data=train)         # NB model implementation
prediction <- predict(model, test)
print("confidence matrix ")
conf_matrix <- table(predict_nb = prediction, class = test$Class)
print(conf_matrix)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
print("NB model accuracy is ")
accuracy(conf_matrix)

rm(list=ls())
