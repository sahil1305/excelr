#I have a dataset containing family information of married couples, which have around 10 variables & 600+ observations. 
#Independent variables are ~ gender, age, years married, children, religion etc.
#I have one response variable which is number of extra marital affairs.
#Now, I want to know what all factor influence the chances of extra marital affair.
#Since extra marital affair is a binary variable (either a person will have or not), 
#so we can fit logistic regression model here to predict the probability of extra marital affair.



install.packages("AER")
library(AER)
library(plyr)


affairs1<-read.csv(file.choose())
View(affairs)

summary(affairs1)

table(affairs1$affairs)

affairs1$affairs[affairs1$affairs > 0] <- 1
affairs1$affairs[affairs1$affairs == 0] <- 0
affairs1$gender <- as.factor(revalue(affairs1$gender,c("male"=1, "female"=0)))
affairs1$children <- as.factor(revalue(affairs1$children,c("yes"=1, "no"=0)))

View(affairs1)



#colnames(affairs1)

#class(affairs1)

attach(affairs1)


# Preparing a linear regression 
mod_lm <- lm(affairs ~ factor(gender) + age+ yearsmarried+ factor(children) + religiousness+
               education+occupation+rating, data = affairs1)
summary(mod_lm)

#Predicting.
pred1 <- predict(mod_lm,affairs1)
pred1


# plot(affairs,pred1)

plot(pred1)

#glm is the function that tells R to run a generalized linear model.
# The output of sigmoid function lies in between 0-1.
model <- glm(affairs ~ factor(gender) + age+ yearsmarried+ factor(children) + religiousness+
               education+occupation+rating, data = affairs1,family = "binomial")


# for calculating the odds ratio manually we going are going to take exp of coef(model)
exp(coef(model))


# Confusion matrix table 
prob <- predict(model,affairs1,type="response")
summary(model)


#considering the threshold value as 0.5 
confusion<-table(prob>0.5,affairs1$affairs)
confusion

#Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 


# Creating empty vectors to store predicted classes based on threshold value
pred_values<-NULL
yes_no<-NULL

pred_values<-ifelse(prob>=0.5,1,0)
yes_no<-ifelse(prob>=0.5 ,"yes","no")



affairs1[,"prob"] <- prob
affairs1[,"pred_values"] <- pred_values
affairs1[,"yes_no"] <- yes_no

View(affairs1[,c(1,9:11)])



table(affairs1$affairs,affairs1$pred_values)

install.packages('ROCR')
library(ROCR)

rocrpred<-prediction(prob,affairs1$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

str(rocrperf)

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
