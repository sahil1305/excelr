install.packages("randomForest")
library(randomForest)
library(caret)
library(C50)
library(dplyr)
library(plyr)
library(lattice)
library(gmodels)

#Reading the CSv file
Fraud<-read.csv(file.choose())

#Converting strings to numbers
Fraud$Undergrad<-as.numeric(revalue(Fraud$Undergrad,c("YES"="1","NO"="0")))
Fraud$Marital.Status<-as.numeric(revalue(Fraud$Marital.Status,c("Single"="1","Divorced"="2","Married"="3")))
Fraud$Urban<-as.numeric(revalue(Fraud$Urban,c("YES"="1","NO"="0")))

#Creating a output column for good or risky
Risky_Good<-ifelse(Fraud$Taxable.Income<=30000,"Risky","Good")
Fraud_check <- cbind(Fraud,Risky_Good)

#Splitting the data into train and test
fraud_train<-Fraud_check[1:299,]
fraud_test<-Fraud_check[300:600,]

#Building Random Forest model on training data
rf<-randomForest(Risky_Good~.,data=fraud_train,na.action=na.roughfix,importance=TRUE)

#Training accuracy
mean(Fraud_check$Risky_Good==predict(rf,fraud_train))

#Prediction of train data
pred_train<-predict(rf,fraud_train)
View(pred_train)

RiskyGood1<-Fraud_check$Risky_Good
RiskyGood1<-RiskyGood1[1:299]
View(RiskyGood1)

#Confusion Matrix
confusionMatrix(RiskyGood1,pred_train)

#Predicting on test data
#Building Random Forest model on Test Data
rf_test<-randomForest(Risky_Good~.,data=fraud_test,na.action=na.roughfix,importance=TRUE)

#Test accuracy
mean(Fraud_check$Risky_Good==predict(rf,fraud_test))

#Prediction on Test data
pred_test<-predict(rf_test,fraud_test)
View(pred_test)

RiskyGoodTest<-Fraud_check$Risky_Good
RiskyGoodTest<-RiskyGoodTest[300:600]

#Confusion Matrix
confusionMatrix(RiskyGoodTest,pred_test)

#visualization Plot 
plot(rf,lwd=2)
legend("topright", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)