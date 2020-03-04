library(dplyr)
library(plyr)
library(lattice)

##reading dataset
computer <- read.csv(file.choose())

View(computer)
attach(computer)

#EDA

#1.measure of central tendency
summary(computer)

#2.measure of dispersion
sd(price)
sd(speed)
sd(hd)
sd(ram)
sd(screen)
sd(ads)
sd(trend)
var(price)
var(speed)
var(hd)
var(ram)
var(screen)
var(ads)
var(trend)

#3.third moment business decision
skewness(price)
skewness(speed)
skewness(hd)
skewness(ram)
skewness(screen)
skewness(ads)
skewness(trend)

#4.fourth moment business decision
kurtosis(price)
kurtosis(speed)
kurtosis(hd)
kurtosis(ram)
kurtosis(screen)
kurtosis(ads)
kurtosis(trend)

#5.graphical representations
hist(speed)
hist(price)
hist(hd)
hist(ram)
hist(screen)
hist(ads)
boxplot(speed)
boxplot(price)
boxplot(hd)
boxplot(ram)
boxplot(screen)
boxplot(ads)

#plotting relation with each x & y.
plot(speed,price)
plot(hd,price)
plot(ram,price)
plot(screen,price)
plot(ads,price)
plot(trend,price)
plot(cd,price)
plot(multi,price)
plot(premium,price)

#plotting of all the pairs of variables
pairs(computer)

#finding missing values
sum(is.na(computer))

computer$cd <- as.numeric(revalue(computer$cd,c("yes"= "2", "no"= "1")))
computer$multi <- as.numeric(revalue(computer$multi,c("yes"= "2", "no"= "1")))
computer$premium <- as.numeric(revalue(computer$premium,c("yes"= "2", "no"= "1")))
View(computer)

#Correlation Coefficient matrix - Strength & Direction of Correlation
cor(computer)

library(GGally)
windows()
ggpairs(computer)
#Partial Correlation matrix - Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(computer))

# The Linear Model of interest with all the columns 
Model.computer <- lm(price~.,data = computer)
summary(Model.computer)

library(car)
vif(Model.computer)
# vif>10 then there exists collinearity among all the variables 
mean(Model.computer$residuals)
sqrt(mean(Model.computer$residuals**2))
# Added Variable plot to check correlation b/n variables and o/p variable
avPlots(Model.computer)

#measuring influence records
influence.measures(Model.computer)
# plotting Influential measures 
influenceIndexPlot(Model.computer,id.n=3) # index plots for infuence measures
influencePlot(Model.computer,id.n=3) # A user friendly representation of the above

# Regression after deleting the 1441th observation, which is influential observation
model_1<-lm(price~.,data=computer[-1441])
summary(model_1)
# Regression after deleting the 1441th & 1701th observation, which is influential observation
model_2 <- lm(price~.,data=computer[-c(1441,1701),])
summary(model_2)

#Logarithmic Transformation :
model_log <- lm(price~log(X)+log(speed)+log(hd)+log(ram)+log(screen)+log(cd)+log(multi)+log(premium)+log(ads)+log(trend),data = computer[-c(1441,1701),])
summary(model_log)
vif(model_log)
confint(model_log,level = 0.95)
predict(model_log,interval="predict")

# Exponential Transformation :
model_exp <- lm(log(price)~.,data = computer[-c(1441,1701),])
summary(model_exp)
confint(model_exp,level = 0.95)
predict(model_exp,interval="predict")

# Quadratic Model :
model_quad <- lm(price~X+I(X^2)+speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)+ads+I(ads^2)+trend+I(trend^2),data = computer[-c(1441,1701),])
summary(model_quad)
confint(model_quad,level = 0.95)
predict(model_quad,interval="predict")

# Polynomial Model
model_poly <- lm(price~X+I(X^2)+I(X^3)+speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)+screen+I(screen^2)+I(screen^3)+cd+I(cd^2)+I(cd^3)+multi+I(multi^2)+I(multi^3)+premium+I(premium^2)+I(premium^3)+ads+I(ads^2)+I(ads^3)+trend+I(trend^2)+I(trend^3),data = computer[-c(1441,1701),])
summary(model_poly)
confint(model_poly,level = 0.95)
predict(model_poly,interval = "predict")

#final
final <- computer[-c(1441,1701),]
final_model <- cbind(final$price,price_predict)
View(final_model)
colnames(final_model) <- c("price","predicted_price")
View(final_model)
