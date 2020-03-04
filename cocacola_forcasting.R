library(forecast)
library(fpp)
library(smooth)
library(readxl)
View(coke)
plot(coke$Sales,type = "o")
Q1 <- ifelse(grepl("Q1",coke$Quarter),'1','0')
Q2 <- ifelse(grepl("Q2",coke$Quarter),'1','0')
Q3 <- ifelse(grepl("Q3",coke$Quarter),'1','0')
Q4 <- ifelse(grepl("Q4",coke$Quarter),'1','0')
cokedata <- cbind(coke,Q1,Q2,Q3,Q4)
View(cokedata)
colnames(cokedata)
cokedata["t"] <- 1:42
View(cokedata)
cokedata["log_Sales"] <- log(cokedata["Sales"])
cokedata["t_square"] <- cokedata["t"]*cokedata["t"]
attach(cokedata)
train <- cokedata[1:36,]
test <- cokedata[37:42,]

#linear model
linear <- lm(Sales~t,data=train)
summary(linear)
linear_pred <- data.frame(predict(linear,interval='predict',newdata=test))
View(linear_pred)
rmse_linear <- sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm=T))
rmse_linear

#exponential model
expo <- lm(log_Sales~t,data=train)
summary(expo)
expo_pred <- data.frame(predict(expo,interval='predict',newdata=test))
View(expo_pred)
rmse_expo <- sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm=T))
rmse_expo

#Quadratic model
quad <- lm(Sales~t+t_square,data=train)
summary(quad)
quad_pred <- data.frame(predict(quad,interval='predict',newdata=test))
View(quad_pred)
rmse_quad <- sqrt(mean((test$Sales-quad_pred$fit)^2,na.rm=T))
rmse_quad

#additive seasonality
sea_add <- lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add)
sea_add_pred <- data.frame(predict(sea_add,interval='predict',newdata=test))
View(sea_add_pred)
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm=T))
rmse_sea_add

#additive seasonality with linear
add_sea_linear <- lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(add_sea_linear)
add_sea_linear_pred <- data.frame(predict(add_sea_linear,interval='predict',newdata=test))
View(add_sea_linear_pred)
rmse_add_sea_linear <- sqrt(mean((test$Sales-add_sea_linear_pred$fit)^2,na.rm=T))
rmse_add_sea_linear

#additive seasonality with quadratic
add_sea_quad <- lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(add_sea_quad )
add_sea_quad_pred <- data.frame(predict(add_sea_quad ,interval='predict',newdata=test))
View(add_sea_quad_pred)
rmse_add_sea_quad <- sqrt(mean((test$Sales-add_sea_quad_pred$fit)^2,na.rm=T))
rmse_add_sea_quad

#multiplicative seasonality
multi_sea <-  lm(log_Sales~Q1+Q2+Q3+Q4,data=train)
summary(multi_sea)
multi_sea_pred <-  data.frame(predict(multi_sea ,interval='predict',newdata=test))
View(multi_sea_pred)
rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm=T))
rmse_multi_sea

#preparing table on models
table_rmse <- data.frame(c("rmse_multi_sea","rmse_add_sea_quad","rmse_add_sea_linear","rmse_sea_add","rmse_quad","rmse_expo","rmse_linear"),c(rmse_multi_sea,rmse_add_sea_quad,rmse_add_sea_linear,rmse_sea_add,rmse_quad,rmse_expo,rmse_linear))
View(table_rmse)
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

#additive seasonality with quadrati trend has the least RMSE value.

new_model <- lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=cokedata)
new_model_pred <- data.frame(predict(new_model,newdata=cokedata,interval='predict'))


#final model
new_model_final <- new_model$fitted.values
View(new_model_final)
Quarter <- as.data.frame(cokedata$Quarter)
Final <- as.data.frame(cbind(Quarter,cokedata$Sales,new_model_final))
colnames(Final)<- c("Quarter","Sales","new_pred_value")
plot(Final$Sales,main = "Actualgraph",xlab = "Sales(Actua)",ylab = "Quarter",col.axis="blue",type = "o")
plot(Final$new_pred_value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")
