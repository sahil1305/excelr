wine <- read.csv(file.choose())
View(wine)

attach(wine)

cor(wine)

pcaobj <- princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)

summary(pcaobj)

str(pcaobj)

loadings(pcaobj)

sumweights <- sum(pcaobj$loadings[,1]**2)
sumweights
sumweights <- sum(pcaobj$loadings[,2]**2)
sumweights

#relationship between PCA 1 and PCA 2
plot(pcaobj$scores[,1],pcaobj$scores[,2])

#checking correlation
cor(pcaobj$scores[,1],pcaobj$scores[,2])

#plot the principal components
plot(pcaobj)

#top 3 PCA's
pcaobj$scores[,1:3]

#Binding the PCA scores with original data
wine<-cbind(wine,pcaobj$score[,1:3])
View(wine)

#Preparing the data for clustering
clus_data<-wine[,15:17]
View(clus_data)

#Normalizing the data
norm_clus<-scale(clus_data)
dist1<-dist(norm_clus,method ="euclidean")

#screw plot for selecting the optimum k value
wss= (nrow(norm_clus)-1)*sum(apply(norm_clus,2,var))
for (i in 2:10) wss[i]= sum(kmeans(norm_clus, centers = i)$withinss)
plot(1:10, wss, type = "b",xlab = "Number of clusters",ylab = "Within group sum of squares")
title(sub= "k-means Clustering screw plot")

#Clustering the data using Hierarchical function
fit1<-hclust(dist1,method="complete")

plot(fit1, hang = -1)

#from the screw plot and elbow curve select k=7
rect.hclust(fit1,k=7,border="green")

#Cutting the dendrogram
groups<-cutree(fit1,7)
View(groups)

membership_1<-as.matrix(groups)
View(membership_1)

#Binding columns with original data
final1<-cbind(membership_1,wine) 
View(final1)
View(aggregate(final1[,-c(16:18)],by=list(membership_1),FUN=mean))


#k- means clustering on normalized data
library(plyr)
summary(norm_clus)
#selecting k value as 7 from the screw plot elbow curve
km <- kmeans(norm_clus,7)
str(km)
km$cluster
library(animation)
km <- kmeans.ani(norm_clus,7)
km$centers





#Hierarchical clustering for entire data
data_full<-read.csv(file.choose())
View(data_full)

#Normalizing the data
norm_clusfull<-scale(data_full)
distfull<-dist(data_full,method ="euclidean")

#Clustering the data using Hierarchical function
fitfull<-hclust(distfull,method="complete")

plot(fitfull, hang = -1)
rect.hclust(fitfull,k=7,border="green")

#Cutting the dendrogram
groups_full<-cutree(fitfull,7)
membership_full<-as.matrix(groups_full)
View(membership_full)
plot(membership_full)

full_data<-cbind(membership_full,data_full)
View(full_data)
View(aggregate(full_data,by=list(membership_full),FUN=mean))


#k-means clustering on entire data
library(plyr)
summary(norm_clusfull)
#selecting k value as 7 from the screw plot elbow curve
km <- kmeans(norm_clusfull,7)
str(km)
km$cluster
library(animation)
km <- kmeans.ani(norm_clusfull,7)
km$centers

