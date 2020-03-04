View(Computer_Data)
library(e1071)
library(dummies)
attach(Computer)

#EDA

#1.measure of central tendency
summary(Computer_Data)

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
pairs(Computer_Data)

#6.finding missing values
is.na(Computer_Data)
sum(is.na(Computer_Data))

final_data <- dummy.data.frame(Computer_Data, c("cd","premium","multi") , sep = "_")
final_data <- final_data[-c(1)]

#finding correlation of all variables
cor(final_data)

#we can also see correlation coefficient and scatter plot together
install.packages("GGally")
install.packages("stringi")
library(GGally)
ggpairs(final_data)

#partial correlation matrix- pure correlation between the matrix
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(final_data))

# The Linear Model of interest with all the columns
model.price <- lm(price~.,data=final_data)
summary(model.price)

# Multicollinearity check
# Model based on only cd_yes
model.cd <- lm(price~cd_yes,data=final_data)  #significant
summary(model.cd)

# Multicollinearity check
# Model based on only multi_yes
model.multi <- lm(price~multi_yes,data=final_data)   #insignificant
summary(model.multi)

# Multicollinearity check
# Model based on only premium_yes
model.premium <- lm(price~premium_yes,data=final_data)   #significant
summary(model.premium)

# Multicollinearity check
# Model based on premium_yes,multi_yes and cd_yes
model.cdpremmulti <- lm(price~cd_yes+premium_yes+multi_yes,data=final_data)  #significant
summary(model.cdpremmulti)

install.packages("car")
library(car)
influence.measures(model.price)

## plotting Influential measures 
windows()
influenceIndexPlot(model.price,id.n=5) # index plots for infuence measures
influencePlot(model.price,id.n=5) # A user friendly representation of the above

# Regression after deleting the 1441th observation, which is influential observation
model_1<-lm(price~.,data=final_data[-1441])
summary(model_1)
# Regression after deleting the 1441th & 1701th observation, which is influential observation
model_2 <- lm(price~.,data=final_data[-c(1441,1701),])
summary(model_2)
##Logarithmic Transformation :
model_log <- lm(price~log(X)+log(speed)+log(hd)+log(ram)+log(screen)+log(cd_no)+log(multi)+log(premium)+log(ads)+log(trend),data = final_data[-c(1441,1701),])
summary(model_log)