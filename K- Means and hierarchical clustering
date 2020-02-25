
hprice2 <- wpull('hprice2')
head(hprice2)
hprice2$index <- NULL

#################
## K-means clustering
#################
var(scale(hprice2))
set.seed(90210)
#kmeans(hprice2,centers=5)
kmodel <- kmeans(scale(hprice2),centers=3,nstart=10)
summary(kmodel)
kmodel$iter
kmodel$tot.withinss
hprice2[,lapply(.SD,sd),]
1.0115090 * 9208.856 + 22511.51
hprice2[,lapply(.SD,mean),]
kmodel$centers
kmodel$cluster
table(kmodel$cluster)

hprice2[,lapply(.SD,mean),by=kmodel$cluster] 
ggplot(hprice2,aes(x=nox,y=price,color=as.factor(kmodel$cluster))) + geom_point()

kmodel$tot.withinss
kmeans.wss(scale(hprice2))
# [1] 4545.000 2703.341 2183.948 1850.740 1621.996 1421.727 1293.463
# [8] 1181.626 1099.631 1035.744

plot.wss <- function(wss){
  plot(1:NROW(wss),wss,type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")
}

plot.wss(kmeans.wss(scale(hprice2)))  ## Requires you to build a kmeans wss function

eratio <- function(wss) { # USE MINUS 1 FOR PCA
  # Creates the eigenvalue ratio estimator for the number of clusters
  n <- NROW(wss)
  dss <- -diff(wss) # Create differences in wss (eigenvalues)
  dss <- c(wss[1]/log(n),dss) # Assign a zero case
  erat <- dss[1:(n-1)]/dss[2:n] # Build the eigenvalue ratio statistic
  gss <- log(1+dss/wss) # Create growth rates
  grat <- gss[1:(n-1)]/gss[2:n] # Calucluate the growth rate statistic
  return(c(which.max(erat),which.max(grat))) # Find the maximum number for each estimator
}
eratio(kmeans.wss(scale(hprice2)))

#################
## Hierarcical clustering
#################
dist(scale(hprice2))
dist(head(scale(hprice2)))
?hclust()
model <- hclust(dist(scale(hprice2)))

plot(model)
rect.hclust(model,k=7,border="red")
rect.hclust(model,k=6,border="purple")
rect.hclust(model,k=5,border="blue")
rect.hclust(model,k=4,border="green")
rect.hclust(model,k=3,border="yellow")
rect.hclust(model,k=2,border="orange")
summary(cutree(model,k=2))   ## cutree to give different number of clusters with the k- value
cutree(model,k=5)

hclust.wss <- function(data,model=hclust(dist(data)),maxclu=10) {
  # Within sum of squares function for hierarchical clustering
  wss <- rep(NA,maxclu)
  for(i in 1:maxclu){
    gps <- cutree(model,i) # get groups for i clusters
    means <- data[,lapply(.SD,mean),by=gps] # get means
    demeaned <- data-means[gps,2:(ncol(data)+1)] # difference data from means
    wss[i] <- sum(demeaned^2) # sum squared distanaces
  }
  return(wss)
}
hprice2[,lapply(.SD,mean),by=cutree(model,k=4)]
hclust.wss(data.table(scale(hprice2)))
# [1] 4545.000 3719.729 3594.778 2418.374 2048.348 1658.079 1583.734
# [8] 1402.939 1361.605 1193.001
plot.wss(hclust.wss(data.table(scale(hprice2))))
eratio(hclust.wss(data.table(scale(hprice2))))

#################
## Applying clustering
#################

set.seed(90210)
hprice2$kmean <- kmeans(scale(hprice2),centers=2,nstart=10)$cluster
hprice2$hier <- cutree(hclust(dist(data.table(scale(hprice2)))),k=4)
table(hprice2$hier,hprice2$kmean) # Two-way table to see relationship
hprice2
# k-means clustering models
# Pooled:
summary(lm(log(price)~log(nox)+rooms+stratio,data=hprice2))
# Within:
summary(lm(log(price)~as.factor(kmean)+log(nox)+rooms+stratio-1,data=hprice2))
glance(lm(log(price)~as.factor(kmean)+log(nox)+rooms+stratio,data=hprice2))

# hierarchical clustering models
# Pooled:
summary(lm(log(price)~log(nox)+rooms+stratio,data=hprice2))
# Within:
summary(lm(log(price)~as.factor(hier)+log(nox)+rooms+stratio,data=hprice2))

hprice2[,lapply(.SD,mean),by=hier]
summary(lm(log(price)~log(nox)+rooms+stratio,data=hprice2[hier==1]))
summary(lm(log(price)~log(nox)+rooms+stratio,data=hprice2[hier==2]))
summary(lm(log(price)~log(nox)+rooms+stratio,data=hprice2[hier==3]))
summary(lm(log(price)~log(nox)+rooms+stratio,data=hprice2[hier==4]))
hprice2[,tidy(lm(log(price)~log(nox)+rooms+stratio)),by=as.factor(hier)][order(as.factor)]

table(hprice2$hier)

summary(lm(log(price)~(log(nox)+as.factor(hier))^2+(log(rooms)+as.factor(hier))^2+stratio,data=hprice2))



