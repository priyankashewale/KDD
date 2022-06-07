# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : HW02_EDA

rm(list = ls())

getwd()
setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/R_code")
data<- read.csv(file ='breast-cancer-wisconsin.csv',na.string = "?") # Read the data file
summary(data)           # 1. Summary of Each column
                        # 2. NA is identified in only F6 column

F6 = sum(data$F6 , na.rm = TRUE)
data$F6[is.na(data$F6)] <- round(F6 / sum(!is.na(data$F6)))   #3.	Replacing the missing values with the "mean" of the column.

View(data)

t <- table(data$Class, data$F6)             # 4. frequency table of F6 vs Class
ftable(t)

plot(data[2:7],main = "scatter plot(F1 to F6)",col="blue")   # 5.scatter plot for column F1 to F6


boxplot(data[8:10], main = " Histogram plot F7 to F9", col = "Yellow")   # 6. histogram plot for column F7 to F9


rm(list = ls())     #7. Removed all the variable from environment list


data_no_na <- read.csv(file = 'breast-cancer-wisconsin.csv',header = TRUE , na.string = "?")


?na.omit.default

data_no_na <- na.omit(data_no_na)

View(data_no_na)
