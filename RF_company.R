library(dplyr)
library(plyr)
library(lattice)
library(randomForest)
library(caret)

#Reading the CSV file
company<-read.csv(file.choose())

#Plotting histogram for sales
hist(company$Sales)

SALES<-ifelse(company$Sales<8,"LOW","HIGH")
Company_df<-cbind(company[-1],SALES)
View(Company_df)
str(Company_df)

#Converting the strings from the file to integers

Company_df$ShelveLoc<-as.numeric(revalue(Company_df$ShelveLoc,c("Bad"="1","Good"="2","Medium"="3")))
Company_df$Urban<-as.numeric(revalue(Company_df$Urban,c("Yes"="1","No"="0")))
Company_df$US<-as.numeric(revalue(Company_df$US,c("Yes"="1","No"="0")))

#Splitting the data into train and test
company_train<-Company_df[1:279,]
company_test<-Company_df[280:400,]

#Building the model for train data
rf<-randomForest(SALES~.,data=company_train,na.action = na.roughfix,importance=TRUE)
pred<-predict(rf,company_train)
View(pred)

sales<-Company_df$SALES
sales<-sales[1:279]
View(sales)

#Train accuracy
mean(sales==predict(rf,company_train))

#Prediction of data
pred_train<-predict(rf,company_train)

#Confusion Matrix
confusionMatrix(sales,pred)

#Building the model for test data
rf_test<-randomForest(SALES~.,data=company_test,na.action=na.roughfix,importance=TRUE)

sales_test<-Company_df$SALES
sales_test<-sales_test[280:400]
View(sales_test)

#Prediction of data
pred_test<-predict(rf_test,company_test)

#Test accuracy
mean(sales_test==predict(rf_test,company_test))
View(pred_test)

#Confusion Matrix
confusionMatrix(sales_test,pred_test)

#Visualization
plot(rf_test,lwd=2)
legend("topright", colnames(rf_test$err.rate),col=1:4,cex=0.8,fill=1:4)