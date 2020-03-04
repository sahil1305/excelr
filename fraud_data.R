library(tree)
library(C50)
library(caret)
library(dplyr)
library(plyr)
library(lattice)
library(gmodels)
library(party)
Fraud<-read.csv(file.choose())
View(Fraud)

#Converting the strings to integers
Fraud$Undergrad<-as.numeric(revalue(Fraud$Undergrad,c("YES"="1","NO"="2")))
Fraud$Urban<-as.numeric(revalue(Fraud$Urban,c("YES"="1","NO"="0")))
Fraud$Marital.Status<-as.numeric(revalue(Fraud$Marital.Status,c("Single"="1","Married"="2","Divorced"="3")))


hist(Fraud$Taxable.Income)

#Creating the column defining Risky or Good
Risky_Good<-ifelse(Fraud$Taxable.Income<=30000,"Risky","Good")
Fraud_Check<-data.frame(Fraud,Risky_Good)

#Splitting the data into train and test
fraud_train<-Fraud[1:420,]
fraud_test<-Fraud[421:600,]

#Building the model
op<-ctree(Risky_Good~Undergrad+Marital.Status+Taxable.Income+City.Population+Work.Experience+Urban,data=Fraud_Check)
windows()
plot(op)
