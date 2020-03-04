glass <- read.csv(file.choose())
table(glass$Type)
glass$Type <- factor(glass$Type, levels = c("1","2","3","4","5","6","7"), labels = c("type1","type2","type3","type4","type5","type6","type7"))

norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
#applying normalization
glassn <- as.data.frame(lapply(glass[-10], norm))

#splitting the data into train and test
glass_train <- glassn[1:149,]
glass_test <- glassn[150:214,]

#Get labels for training and test datasets
glass_train_labels <- glass[1:149,10]
glass_test_labels <- glass[150:214,10]


library(class)

test_acc <- NULL
train_acc <- NULL

for (i in 1:30)
{
  train_glass_pred <- knn(train=glass_train,test=glass_test,cl=glass_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==glass_train_labels))
  test_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==glass_test_labels))
}
library(gmodels)
CrossTable(x=glass_test_labels,y=test_glass_pred)
