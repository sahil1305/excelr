zoo <- read.csv(file.choose())
View(zoo)
zoo <- zoo[-1]

zoo$type <- factor(zoo$type, levels = c("1","2","3","4","5","6","7"), labels = c("type1","type2","type3","type4","type5","type6","type7"))

norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#applying normalization
zoon <- as.data.frame(lapply(zoo[-17], norm))

#splitting the data into train and test
zootrain <- zoon[1:70,]
zootest <- zoon[71:101,]

#Get labels for training and test datasets
zootrain_labels <- zoo[1:70,17]
zootest_labels <- zoo[71:101,17]

library(class)

test_acc <- NULL
train_acc <- NULL

for (i in 1:20)
{
  train_zoo_pred <- knn(train=zootrain,test=zootest,cl=zootrain_labels,k=i)
  train_acc <- c(train_acc,mean(train_zoo_pred==zootrain_labels))
  test_zoo_pred <- knn(train = zootrain, test = zootest, cl = zootrain_labels, k=i)
  test_acc <- c(test_acc,mean(test_zoo_pred==zootest_labels))
}

library(gmodels)
CrossTable(x=zootest_labels,y=test_zoo_pred)
