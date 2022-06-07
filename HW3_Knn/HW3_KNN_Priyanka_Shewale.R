# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : HW03_KNN
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
df <- na.omit(df)
summary(df)

# F6 NA values omitted.
df$Class<- factor(df$Class , levels = c("2","4") , labels = c("benign","malignant"))
View(df) # Class column has values 2 and 4, now converting that to the benign or malignant.

#Training & Test Data (30% test & 70% Training)
trainIndex <- sample(1:nrow(df), round(0.7 * nrow(df))) 

#min-max Normalization
features <- as.data.frame(lapply(df[,c(2,3,4,5,6,7,8,9,10)], function(x) {(x -min(x))/(max(x)-min(x))}))

# output column(expected)
target = df['Class']

#train set
train <- features[trainIndex,] 
trainTarget <- target[trainIndex,]

##test set
test <- features[-trainIndex,] 
testTarget <- target[-trainIndex,]


#KNN with k = 3
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
clf <- knn(train,test,cl=trainTarget,k=3)

conf_matrix <- table(clf, testTarget)
print("confidence matrix for k=3:")
print(conf_matrix)
accuracy(conf_matrix) # accuracy from confidence matrix

#KNN with k = 5
clf2 <- knn(train,test,cl=trainTarget,k=5)

conf_matrix2 <- table(clf2, testTarget)
print("confidence matrix for k=5:")
print(conf_matrix2)
accuracy(conf_matrix2)# accuracy from confidence matrix for K =5

#KNN with k = 10
clf3 <- knn(train,test,cl=trainTarget,k=10)

conf_matrix3 <- table(clf3, testTarget)
print("confidence matrix for k=10:")
print(conf_matrix3)
accuracy(conf_matrix3)# accuracy from confidence matrix for K =10

