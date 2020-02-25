tt <- function(n,test.ratio=0.1,seed=-1){    ## For pulling test training split
  if(seed>0) set.seed(seed)
  n.test     <- floor(n*test.ratio)
  perm       <- rank(runif(n))
  outp       <- list()
  outp$test  <- perm[1:n.test]
  outp$train <- perm[(n.test+1):n]
  return(outp)
}
dist.tt <- function(test,train,scale=T,d.fun=dist,...){ 
  n.test  <- nrow(test)
  data    <- rbind(test,train)
  if(scale) data <- scale(data)
  n.total <- nrow(data)
  dmat    <- as.matrix(d.fun(data,...))
  dmat    <- dmat[1:n.test,(n.test+1):n.total]
  return(dmat)
}
knn.ranks <- function(dist,k){     ##
  rnks   <- apply(as.matrix(dist),1,rank)
  mylist <- lapply(1:k,function(x) which(rnks==x,arr.ind=T)[,1])
  return(matrix(unlist(mylist),byrow=F,ncol=k))
}
rnk <- apply(as.matrix(dist(hprice2)),1,rank)
rnk
mylist <- lapply(1:3,function(x) which(rnk==x,arr.ind=T)[,1])
mylist
matrix(unlist(mylist),byrow=F,ncol=5)
  
rank(as.matrix(dist(hprice2))[1,]) 

knn.ranks(dist(hprice2),k=10)


hprice2 <- wpull('hprice2')
xdata <- copy(hprice2)
xdata[,price:=NULL]
k <- 5

## in-sample knn
dmat <- dist(scale(xdata))
kmat <- knn.ranks(dmat,k+1)
kmat ##ranks associated wth all those data points
kmat <- kmat[,2:(k+1)]
dset <- copy(hprice2)
## apply(kmat,1,function(x)dset[x,mean(price)])
dset$pred <- apply(kmat,1,function(x)dset[x,mean(price)])
dset$err  <- dset$price-dset$pred
rmse1 <- sqrt(mean(dset$err^2))
rmse1

model <- price~pred+nox+rooms+dist+stratio
model <- lm(model,data=dset)
model <- step(model)
glance(model)
tidyw(model)

## out-of-sample knn
samp <- tt(nrow(hprice2),seed=75080)
samp
dmat <- dist.tt(xdata[samp$test],xdata[samp$train])
kmat <- knn.ranks(dmat,k)
kmat
dtrain <- copy(hprice2)[samp$train]
dtest <- copy(hprice2)[samp$test]
dtest$pred <- apply(kmat,1,function(x)dtrain[x,mean(price)])
dtest$err  <- dtest$price-dtest$pred
rmse2 <- sqrt(mean(dtest$err^2))
rmse2

model <- price~pred+nox+rooms+dist+stratio
model <- lm(model,data=dtest)
model <- step(model)
glance(model)
tidyw(model)


## out-of-sample knn manhattan distance
dmat <- dist.tt(xdata[samp$test],xdata[samp$train],method="manhattan")
kmat <- knn.ranks(dmat,k)
dtrain <- copy(hprice2)[samp$train]
dtest <- copy(hprice2)[samp$test]
dtest$pred <- apply(kmat,1,function(x)dtrain[x,mean(price)])
dtest$err  <- dtest$price-dtest$pred
rmse3 <- sqrt(mean(dtest$err^2))
rmse3

model <- price~pred+nox+rooms+dist+stratio
model <- lm(model,data=dtest)
model <- step(model)
glance(model)
tidyw(model)