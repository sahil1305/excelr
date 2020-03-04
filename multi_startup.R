library(dplyr)
library(plyr)
library(lattice)
library(e1071)
startup <-read.csv(file.choose())
startup$State <- as.numeric(revalue(startup$State,c("New York"="1", "California"="2", "Florida"="3")))
str(startup)
attach(startup)

#EDA

#1.measure of central tendency
summary(startup)

#2.measure of dispersion
sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(State)
sd(Profit)
var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(State)
var(Profit)

#3. third moment of business decision
skewness(R.D.Spend)
skewness(Administration)
skewness(Marketing.Spend)
skewness(State)
skewness(Profit)

#4. fourth moment of busines decision
kurtosis(R.D.Spend)
kurtosis(Administration)
kurtosis(Marketing.Spend)
kurtosis(State)
kurtosis(Profit)

#5.graphical representation
plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
hist(R.D.Spend)
hist(Profit)
hist(Administration)
hist(Marketing.Spend)
hist(State)
boxplot(R.D.Spend)
boxplot(Administration)
boxplot(Marketing.Spend)
boxplot(State)
boxplot(Profit)

#6. finding missing values
sum(is.na(startup))

#Correlation Coefficient matrix - Strength & Direction of Correlation
cor(startup)

library(GGally)
ggpairs(startup)
#Partial Correlation matrix - Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(startup))
# The Linear Model of interest with all the columns 
Model.startup <- lm(Profit~.,data = startup)
summary(Model.startup)

library(car)
vif(Model.startup)
mean(Model.startup$residuals)
sqrt(mean(Model.startup$residuals**2))
# Added Variable plot to check correlation b/n variables and o/p variable
avPlots(Model.startup)
#measuring influence records
influence.measures(Model.startup)
# plotting Influential measures 
influenceIndexPlot(Model.startup,id.n=3) # index plots for infuence measures
influencePlot(Model.startup,id.n=3) # A user friendly representation of the above

# Regression after deleting the 50th observation, which is influential observation
model_1<-lm(Profit~.,data=startup[-50])
summary(model_1)

# Regression after deleting the 49th & 50th observation, which is influential observation
model_2 <- lm(Profit~.,data=startup[-c(49,50),])
summary(model_2)

#Logarithmic Transformation :
model_log <- lm(Profit~(R.D.Spend)+log(Administration)+(Marketing.Spend)+log(State),data = startup[-c(49,50),])
# here we can't apply log to R.D.Spend & Marketing.Spend,bcoz they contain 0 value for some obs and log 0 is not defined
summary(model_log)

#Exponential Transformation :
model_exp <- lm(log(Profit)~(R.D.Spend)+(Administration)+(Marketing.Spend)+(State),data = startup[-c(49,50),])
summary(model_exp)

#Quadratic Transformation :
model_quad <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+Administration+I(Administration^2)+Marketing.Spend+I(Marketing.Spend^2)+State+I(State^2),data = startup[-c(49,50),])
summary(model_quad)

#Polynomial Transformation :
model_poly <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3)+Administration+I(Administration^2)+I(Administration^3)+Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3)+State+I(State^2)+I(State^3),data = startup[-c(49,50),])
summary(model_poly)

#the polynomial transformation gives the best R square value 

#Final Model :
final_model <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3)+Administration+I(Administration^2)+I(Administration^3)+Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3)+State+I(State^2)+I(State^3),data = startup[-c(49,50),])
summary(final_model) 
profit_predict <- predict(final_model,interval = "predict")
summary(profit_predict)
pred_profit <- predict(final_model)
final <- cbind(R.D.Spend,Administration,Marketing.Spend,State,Profit,pred_profit)
