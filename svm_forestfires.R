mydata <- forestfires[-c(1,2)]
colnames(mydata)<-c("FFMC","DMC","DC","ISI","temp","RH","wind","rain","area","dayfri","daymon","daysat","daysun","daythu","daytue","daywed","monthapr","monthaug","monthdec","monthfeb","monthjan","monthjul","monthjun","monthmar","monthmay","monthnov","monthoct","monthsep","size_category")
train <- mydata[1:258,]
test <- mydata[259:517,]
library(kernlab)
library(caret)
model1 <- ksvm(size_category ~.,data = train,kernel = "rbfdot")
model1
pred_rbfdot<-predict(model1,newdata=test)
mean(pred_rbfdot==test$size_category)
table(test$size_category)
table(pred_rbfdot,test$size_category)


model2 <- ksvm(size_category ~.,data = train,kernel = "vanilladot")
model2
pred_vanilladot<-predict(model2,newdata=test)
mean(pred_vanilladot==test$size_category)
table(test$size_category)
table(pred_vanilladot,test$size_category)


model3 <- ksvm(size_category ~.,data = train,kernel = "polydot")
model3
pred_polydot<-predict(model3,newdata=test)
mean(pred_polydot==test$size_category)
table(test$size_category)
table(pred_polydot,test$size_category)


model4 <- ksvm(size_category ~.,data = train,kernel = "tanhdot")
model4
pred_tanhdot<-predict(model4,newdata=test)
mean(pred_tanhdot==test$size_category)
table(test$size_category)
table(pred_tanhdot,test$size_category)

model5 <- ksvm(size_category ~.,data = train,kernel = "laplacedot")
model5
pred_laplacedot<-predict(model5,newdata=test)
mean(pred_laplacedot==test$size_category)
table(test$size_category)
table(pred_laplacedot,test$size_category)

model6 <- ksvm(size_category ~.,data = train,kernel = "besseldot")
model6
pred_besseldot<-predict(model5,newdata=test)
mean(pred_besseldot==test$size_category)
table(test$size_category)
table(pred_laplacedot,test$size_category)

#salary dataset
salary <- rbind(SalaryData_Test,SalaryData_Train)
mydata <- salary[-c(2,3,5,6,7,8,9,13)]
colnames(salary)<-c("workclass","education","maritalstatus","occupation","relationship","race","sex","native","age","educationno","capitalgain","capitalloss","hoursperweek","Salary")
test <- mydata[1:15060,]
train <- mydata[15061:45221,]
library(kernlab)
library(caret)
model1 <- ksvm(Salary ~.,data = SalaryData_Train,kernel = "vanilladot")
model1
pred_vanilldot<-predict(model1,newdata=SalaryData_Test)
mean(pred_vanilldot==SalaryData_Test$Salary)
table(SalaryData_Test$Salary)
table(pred_vanilldot,SalaryData_Test$Salary)
