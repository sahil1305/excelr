install.packages("C50")
install.packages("tree")
library(C50)


#Importing the dataset
sales=read.csv(file.choose())
View(sales)


#Converting the output varible to categorial variable
a<-sales$Sales
View(a)
sales$Sales<-as.factor(sales$Sales)



#Spltting the data into train and test
train<-sales[1:90,]
test<-sales[91:200,]




#Building the model on training data
train_C5.0<-C5.0(train[,-1],train$Sales)
windows()
#Plotting the Tree Graph
plot(train_C5.0)

#Training accuracy
pred_train<-predict(train_C5.0,train)

mean(train$Sales==pred_train)  

library(caret)
confusionMatrix(pred_train,train$Sales)

#Predicting the test data
test_C5.0<-C5.0(test[,-1],test$Sales)
plot(test_C5.0)

#Test accuracy
pred_test<-predict(test_C5.0,test)

mean(test$Sales==pred_test) 

confusionMatrix(pred_test,test$Sales)

library(gmodels)
CrossTable(test$Sales,pred_test)




