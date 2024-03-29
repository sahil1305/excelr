library(readxl)
Airlines_Data<-read_xlsx(file.choose()) 
View(Airlines_Data) # Seasonality 12 months 
windows()
plot(Airlines_Data$Passengers,type="o")
# So creating 12 dummy variables 

X<-data.frame(outer(rep(month.abb,length=96),month.abb,"==") +0)
View(X)

?month.abb
colnames(X)<-month.abb # Assigning month names 
View(X)
trakdata<-cbind(Airlines_Data,X)
View(trakdata)
trakdata["t"]<- 1:96
View(trakdata)
trakdata["log_Passengers"]<-log(trakdata["Passengers"])
trakdata["t_square"]<-trakdata["t"]*trakdata["t"]
attach(trakdata)

train<-trakdata[1:48,]

test<-trakdata[49:90,]

#LINEAR MODEL
linear_model<-lm(Passengers~t,data=train)

summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear


# Exponential 
expo_model<-lm(log_Passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

#Quadratic 
Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

# Additive Seasonality
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

#Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

# Additive Seasonality with Quadratic 
Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

#Multiplicative Seasonality
multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

#Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

#Multiplicative Seasonality quadratic trend 
multi_quad_sea_model<-lm(log_Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_quad_sea_model) 
multi_quad_sea_pred<-data.frame(predict(multi_quad_sea_model,newdata=test,interval='predict'))
rmse_multi_quad_sea<-sqrt(mean((test$Passengers-exp(multi_quad_sea_pred$fit))^2,na.rm = T))
rmse_multi_quad_sea

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_Add_sea_Linear","rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea","rmse_multi_quad_sea"),c(rmse_Add_sea_Linear,rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea,rmse_multi_quad_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# rmse_Add_sea_Linear trend  has least RMSE value
new_model <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)


resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(trakdata,file="trakdata.csv",col.names = F,row.names = F)