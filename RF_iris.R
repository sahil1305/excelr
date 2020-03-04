library(randomForest)
data(iris)
View(iris)
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] 
iris_versicolor <- iris[iris$Species=="versicolor",] 
iris_virginica <- iris[iris$Species=="virginica",] 
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,importance=TRUE)
#training accuracy 
pred_t1 <- fit.forest$predicted
table(pred_t1,iris_train$Species)
mean(iris_train$Species==predict(fit.forest,iris_train)) 
library(caret)
# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)
# Predicting test data 
pred_t2 <- predict(fit.forest,newdata=iris_test)
mean(pred_t2==iris_test$Species) 
# Confusion Matrix 
confusionMatrix(iris_test$Species, pred_t2)
# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)
#cross table
library(gmodels)
rf_pred <- CrossTable(iris_train$Species,fit.forest$predicted,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn = c("actual default","predicted default"))
