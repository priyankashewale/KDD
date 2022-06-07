# Course      : CS_513_KDD
# First Name  : Priyanka Manoj
# Last Name   : Shewale
# CWID        : 10477984
# Assignment  : HW8 Cluster(hclust & kmeans)
###########################################################################################################
##########################################################################################################

rm(list=ls())
setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/HW8_cluster")

db = read.csv('wisc_bc_ContinuousVar.csv',header=TRUE, sep=",")
db = db[,-1]
db<-na.omit(db)
summary(db)
View(db)

db2<-dist(db[,-1])
final<-hclust(db2)
plot(final)
db3<-cutree(final,2)
table(db3,db[,1])

rm(list=ls())
# KNN

setwd("C:/Priyanka/Stevens/SEM2/CS 513 KDD/Assignments/HW8_cluster")

db = read.csv('wisc_bc_ContinuousVar.csv',header=TRUE, sep=",")
db = db[,-1]
db<-na.omit(db)
summary(db)
View(db)

kmodel<- kmeans(db[,-1],2,nstart = 10)
table(kmodel$cluster,db[,1])

