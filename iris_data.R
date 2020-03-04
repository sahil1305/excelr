install.packages("C50")
install.packages("tree")
library(C50)
#Loading the dataset
data()
data("iris")
head(iris)

#Splitting the data set based on species
iris_setosa<-iris[iris$Species=="setosa",]
iris_versicolor<-iris[iris$Species=="versicolor",]
iris_virginica<-iris[iris$Species=="virginica",]

#creating train and test
iris_train <- rbind(iris_setosa[1:35,],iris_versicolor[1:35,],iris_virginica[1:35,])
iris_test <- rbind(iris_setosa[36:50,],iris_versicolor[36:50,],iris_virginica[36:50,])


#Building the model for training data
irisc5_train <- C5.0(iris_train[,-5],iris_train$Species)
windows()
plot(irisc5_train)

#training accuracy
pred_train <- predict(irisc5_train,iris_train[,-5])
mean(iris_train$Species==pred_train)

library(caret)
confusionMatrix(pred_train,iris_train$Species)

pred5_test <- predict(irisc5_train,newdata=iris_test[,-5])
mean(pred5_test==iris_test$Species)
confusionMatrix(pred5_test,iris_test$Species)
library(gmodels)
# Cross tablez
CrossTable(iris_test$Species,pred5_test)

#Using tree function 
library(tree)
# Building a model on training data 
iris_tree <- tree(Species~.,data=iris_train)
plot(iris_tree)
text(iris_tree,pretty = 0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(iris_tree,newdata=iris_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(iris_tree,newdata=iris_test)
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
mean(pred_tree$final==iris_test$Species) 
CrossTable(iris_test$Species,pred_tree$final)
