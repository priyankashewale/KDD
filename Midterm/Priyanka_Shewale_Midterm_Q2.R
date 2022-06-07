# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : MIDTERM Question 2
###########################################################################################################
##########################################################################################################

rm(list = ls())

# Load and summarize
setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/Midterm")
ibm <- read.csv(file = 'IBM_Attrition_v3.csv',header=TRUE, sep=",")
summary(ibm)

# From summary, missing values (NA) exist only in MonthlyIncome

# function to calculate mean of the column

# Replacing NA of the column with mode of the column

ibm$MonthlyIncome[is.na(ibm$MonthlyIncome)] <- mean(ibm$MonthlyIncome, na.rm = TRUE)

View(ibm)

# Scatter plots
graph1Data = data.frame(ibm$Age,ibm$MonthlyIncome,ibm$YearsAtCompany)
plot(graph1Data,main="Scatter plot of Age, Exposure and MonthAtHospital",col="blue")

# box plots
graph2Data = data.frame(ibm$Age,ibm$MonthlyIncome,ibm$YearsAtCompany)
boxplot(graph2Data,main="box plots for columns: Age, and MonthAtHospital",col="green")

rm(list = ls())
