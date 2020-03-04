library(AER)
library(plyr)
library(readr)
Bank_data <- read.csv(file.choose())
View(Bank_data)
Bank <- Bank_data[,-c(2,3,4,9,11,16)]
attach(Bank)
summary(Bank)
#finding missing values
sum(is.na(Bank))
dim(Bank)
Bank$y <- as.factor(revalue(y,c("yes"=1, "no"=0)))
Bank$default <- as.factor(revalue(default,c("yes"=1, "no"=0)))
Bank$housing <- as.factor(revalue(housing,c("yes"=1, "no"=0)))
Bank$loan <- as.factor(revalue(loan,c("yes"=1, "no"=0)))
View(Bank)
# Preparing a linear regression 
mod_lm <- lm(y~age+default+housing+loan+day+balance+duration+campaign+pdays+previous,data=Bank)
pred1 <- predict(mod_lm,Bank)
pred1
plot(pred1)
# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(y~age+default+housing+loan+day+balance+duration+campaign+pdays+previous, data = Bank,family = "binomial")
summary(model)
# To calculate the odds ratio manually we are going to take exp of coef(model)
exp(coef(model))
# Confusion matrix table: 
prob <- predict(model,Bank,type="response")
# Confusion matrix and considering the threshold value as 0.5 
confusion <- table(prob>0.5,y)
confusion
# Accuracy of model:
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")
# Creating new column to store the above values
Bank[,"prob"] <- prob
Bank[,"pred_values"] <- pred_values
Bank[,"yes_no"] <- yes_no
View(Bank[,c(1,12:14)])
table(y,Bank$pred_values)
# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,y)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
