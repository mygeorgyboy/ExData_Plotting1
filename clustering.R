
set.seed(1234); par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
par(mfrow=c(1,1))
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

dataFrame <- data.frame(x=x,y=y)
dist(dataFrame)



suppressMessages(library(fields))
dataFrame <- data.frame(x=x,y=y)
rdistxy <- rdist(dataFrame)
diag(rdistxy) <- diag(rdistxy) + 1e5

# Find the index of the points with minimum distance
ind <- which(rdistxy == min(rdistxy),arr.ind=TRUE)
par(mfrow=c(1,2),mar=rep(0.2,4))
# Plot the points with the minimum overlayed
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
points(x[ind[1,]],y[ind[1,]],col="orange",pch=19,cex=2)

# Make a cluster and cut it at the right height
distxy <- dist(dataFrame)
hcluster <- hclust(distxy)
dendro <- as.dendrogram(hcluster)
cutDendro <- cut(dendro,h=(hcluster$height[1]+0.00001) )
plot(cutDendro$lower[[11]],yaxt="n")



# Find the index of the points with minimum distance
ind <- which(rdistxy == min(rdistxy),arr.ind=TRUE)
par(mar=rep(0.2,4))
# Plot the points with the minimum overlayed
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
points(x[ind[1,]],y[ind[1,]],col="orange",pch=19,cex=2)
points(mean(x[ind[1,]]),mean(y[ind[1,]]),col="black",cex=3,lwd=3,pch=3)
points(mean(x[ind[1,]]),mean(y[ind[1,]]),col="orange",cex=5,lwd=3,pch=1)



library(fields)
dataFrame <- data.frame(x=x,y=y)
rdistxy <- rdist(dataFrame)
diag(rdistxy) <- diag(rdistxy) + 1e5

# Find the index of the points with minimum distance
ind <- which(rdistxy == rdistxy[order(rdistxy)][3],arr.ind=TRUE)
par(mfrow=c(1,3),mar=rep(0.2,4))
# Plot the points with the minimum overlayed
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
points(x[c(5,6)],y[c(5,6)],col="orange",pch=19,cex=2)
points(x[ind[1,]],y[ind[1,]],col="red",pch=19,cex=2)

# Make dendogram plots
distxy <- dist(dataFrame)
hcluster <- hclust(distxy)
dendro <- as.dendrogram(hcluster)
cutDendro <- cut(dendro,h=(hcluster$height[2]) )
plot(cutDendro$lower[[10]],yaxt="n")
plot(cutDendro$lower[[5]],yaxt="n")



dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)


myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
  y <- y[order(x)]; x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
        labels=lab[hclust$order], col=lab.col[hclust$order], 
        srt=90, adj=c(1,0.5), xpd=NA, ... )
}



dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))



dataFrame <- data.frame(x=x,y=y)
par(mar=rep(0.1,4))
plot(x,y,col="blue",pch=19,cex=2)
points(x[8],y[8],col="orange",pch=3,lwd=3,cex=3)
points(x[1],y[1],col="orange",pch=3,lwd=3,cex=3)
segments(x[8],y[8],x[1],y[1],lwd=3,col="orange")

dataFrame <- data.frame(x=x,y=y)
par(mar=rep(0.1,4))
plot(x,y,col="blue",pch=19,cex=2)
points(mean(x[1:4]),mean(y[1:4]),col="orange",pch=3,lwd=3,cex=3)
points(mean(x[5:8]),mean(y[5:8]),col="orange",pch=3,lwd=3,cex=3)
segments(mean(x[1:4]),mean(y[1:4]),mean(x[5:8]),mean(y[5:8]),lwd=3,col="orange")


dataFrame <- data.frame(x=x,y=y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)














set.seed(1234); par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))




set.seed(1234); par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))





par(mar=rep(0.2,4))
plot(x,y,col="blue",pch=19,cex=2)
cols1 <- c("red","orange","purple")
text(x+0.05,y+0.05,labels=as.character(1:12))
cx <- c(1,1.8,2.5)
cy <- c(2,1,1.5)
points(cx,cy,col=cols1,pch=3,cex=2,lwd=2)

## Find the closest centroid
distTmp <- matrix(NA,nrow=3,ncol=12)
distTmp[1,] <- (x-cx[1])^2 + (y-cy[1])^2
distTmp[2,] <- (x-cx[2])^2 + (y-cy[2])^2
distTmp[3,] <- (x-cx[3])^2 + (y-cy[3])^2
newClust <- apply(distTmp,2,which.min)
points(x,y,pch=19,cex=2,col=cols1[newClust])





par(mar=rep(0.2,4))
plot(x,y,col="blue",pch=19,cex=2)
cols1 <- c("red","orange","purple")
text(x+0.05,y+0.05,labels=as.character(1:12))

## Find the closest centroid
distTmp <- matrix(NA,nrow=3,ncol=12)
distTmp[1,] <- (x-cx[1])^2 + (y-cy[1])^2
distTmp[2,] <- (x-cx[2])^2 + (y-cy[2])^2
distTmp[3,] <- (x-cx[3])^2 + (y-cy[3])^2
newClust <- apply(distTmp,2,which.min)
points(x,y,pch=19,cex=2,col=cols1[newClust])
newCx <- tapply(x,newClust,mean)
newCy <- tapply(y,newClust,mean)

## Old centroids

cx <- c(1,1.8,2.5)
cy <- c(2,1,1.5)

points(newCx,newCy,col=cols1,pch=3,cex=2,lwd=2)











par(mar=rep(0.2,4))
plot(x,y,col="blue",pch=19,cex=2)
cols1 <- c("red","orange","purple")
text(x+0.05,y+0.05,labels=as.character(1:12))


cx <- c(1,1.8,2.5)
cy <- c(2,1,1.5)


## Find the closest centroid
distTmp <- matrix(NA,nrow=3,ncol=12)
distTmp[1,] <- (x-cx[1])^2 + (y-cy[1])^2
distTmp[2,] <- (x-cx[2])^2 + (y-cy[2])^2
distTmp[3,] <- (x-cx[3])^2 + (y-cy[3])^2
newClust <- apply(distTmp,2,which.min)
newCx <- tapply(x,newClust,mean)
newCy <- tapply(y,newClust,mean)

## Old centroids

points(newCx,newCy,col=cols1,pch=3,cex=2,lwd=2)


## Iteration 2
distTmp <- matrix(NA,nrow=3,ncol=12)
distTmp[1,] <- (x-newCx[1])^2 + (y-newCy[1])^2
distTmp[2,] <- (x-newCx[2])^2 + (y-newCy[2])^2
distTmp[3,] <- (x-newCx[3])^2 + (y-newCy[3])^2
newClust2 <- apply(distTmp,2,which.min)

points(x,y,pch=19,cex=2,col=cols1[newClust2])




par(mar=rep(0.2,4))
plot(x,y,col="blue",pch=19,cex=2)
cols1 <- c("red","orange","purple")
text(x+0.05,y+0.05,labels=as.character(1:12))


cx <- c(1,1.8,2.5)
cy <- c(2,1,1.5)

## Find the closest centroid
distTmp <- matrix(NA,nrow=3,ncol=12)
distTmp[1,] <- (x-cx[1])^2 + (y-cy[1])^2
distTmp[2,] <- (x-cx[2])^2 + (y-cy[2])^2
distTmp[3,] <- (x-cx[3])^2 + (y-cy[3])^2
newClust <- apply(distTmp,2,which.min)
newCx <- tapply(x,newClust,mean)
newCy <- tapply(y,newClust,mean)



## Iteration 2
distTmp <- matrix(NA,nrow=3,ncol=12)
distTmp[1,] <- (x-newCx[1])^2 + (y-newCy[1])^2
distTmp[2,] <- (x-newCx[2])^2 + (y-newCy[2])^2
distTmp[3,] <- (x-newCx[3])^2 + (y-newCy[3])^2
finalClust <- apply(distTmp,2,which.min)


## Final centroids
finalCx <- tapply(x,finalClust,mean)
finalCy <- tapply(y,finalClust,mean)
points(finalCx,finalCy,col=cols1,pch=3,cex=2,lwd=2)



points(x,y,pch=19,cex=2,col=cols1[finalClust])






dataFrame <- data.frame(x,y)

plot(dataFrame)
kmeansObj <- kmeans(dataFrame,centers=3)

names(kmeansObj)


kmeansObj$cluster


par(mar=rep(0.2,4))
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)



dataFrame

