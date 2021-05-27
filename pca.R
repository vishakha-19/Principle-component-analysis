summary(wine)

nrow(wine)
ncol(wine)
is.null(wine)

wine1<-scale(wine)   #scale
View(wine1)

pca<-princomp(wine1,cor= TRUE,scores=TRUE,covmat=NULL)
summary(pca)

pca$scores
newdata<-pca$scores[,1:3]
plot(pca$scores[,1:3],col="Blue",cex=0.5)
text(pca$scores[,1:3],labels=c(1:178),cex=1)

library(kselection)
k<-kselection(wine1,parallel = TRUE)#original data
k

k<-kselection(newdata,parallel = TRUE)#pca components
k
library(plyr)
km=kmeans(wine1,2)
str(km)
km$cluster
km$centers
library(animation)
km<-kmeans.ani(wine1,2)

km=kmeans(newdata,3)
str(km)
km$cluster
km$centers
library(animation)
km<-kmeans.ani(newdata,3)

#h clustering

d <- dist(newdata, method = "euclidean")
fit <- hclust(d, method="complete") 
fit
plot(fit)
plot(fit, hang=-1)

groups <- cutree(fit, k=3)
rect.hclust(fit, k=3, border="blue")


