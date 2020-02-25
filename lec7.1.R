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
plot.wss <- function(wss) {
  plot(1:NROW(wss), wss, type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")
}
plot.pca <- function(wss) {
  plot(0:(NROW(wss)-1), wss, type="b", xlab="Number of PCs", ylab="Eigenvalues")
}
pca.select <- function(pca.model,n.max=10){
  eigen <- pca.model$sdev^2
  if(NROW(eigen)>=n.max) eigen<-eigen[1:n.max]
  plot.pca(eigen)
  return(eratio(eigen)-1)
}
pc.test <- function(data,test.var,max.factor=NULL){
  residual.data <- data.frame(data)
  n <- ncol(residual.data)
  if(is.null(max.factor))max.factor<-max(pca.select(prcomp(residual.data,scale=TRUE)))+2
  for(c in 1:n){
    col <- residual.data[,c]
    if(is.null(ncol(test.var))){ # Single variable case
      model <- lm(col~test.var)
    }else{ # Multiple variables
      subdata <- cbind(col,test.var)
      model <- lm(col~.,data=subdata)
    }
    residual.data[,c] <- residuals(model) # Obtain residuals
  }
  return(pca.select(prcomp(residual.data,scale=TRUE),n.max=max.factor))
}


hprice2 <- wpull('hprice2')
summary(hprice2)
summary(scale(hprice2))


hprice2$index <- NULL
xdata <- hprice2[,.(crime,rooms,dist,radial,proptax,stratio,lowstat)]


summary(xdata)
cov(xdata)
cor(xdata)
pca <- prcomp(xdata,scale=T)
summary(pca)
attributes(pca)
screeplot(pca,type="lines") # looks like there are 2 principal components
pca.select(pca)

pca$rotation
xxdata <- scale(xdata)

PC1 <- xxdata$crime*0.3726041+ -0.2714822*xxdata$rooms
#pca$sdev
head(pca$x)
pca$x[,1:2]
pca$x[,1]  
mean(pca$x[,1])
var(pca$x[,1])

hprice2$fact <- scale(pca$x[,1])
hprice2$fact <- -hprice2$fact
var(hprice2$fact)
# modeling with the factors
model0 <- lm(log(price)~log(nox),data=hprice2)
summary(model0) 
model1 <- lm(log(price)~fact,data=hprice2)
summary(model1) 
model2 <- lm(log(price)~log(nox)+fact,data=hprice2)
summary(model2) 

model3<- lm(log(price)~log(nox) + log(dist) + rooms+ stratio, data = hprice2)
summary(model3)
c(AIC(model0),AIC(model1),AIC(model2))
c(BIC(model0),BIC(model1),BIC(model2))


c(AIC(model0),AIC(model1),AIC(model2),AIC(model3))
c(BIC(model0),BIC(model1),BIC(model2),BIC(model3))
#A 1% increase in nox =>  0.15904% increase in price controlling for the first principal component of crime, rooms, stratio, dist, ...
