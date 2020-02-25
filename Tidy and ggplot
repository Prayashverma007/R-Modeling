## MAKE THIS lec3.1.R

view.sim <- function(result,column=quote(estimate)){
  dt <- result[,.(mean(eval(column)),var(eval(column))),by=n]
  names(dt) <- c("n","mean","variance")
  print(dt)
  ggplot(result,aes(x=eval(column),fill=as.factor(n))) + geom_density()
}

dt <- data.table(index=1:1000)
bigN <- nrow(dt)
dt$x <- rnorm(bigN,2,1)
dt$y <- 2*dt$x + rnorm(bigN,0,1)


lm(y~x,data=dt)
summary(lm(y~x,data=dt))
tidy(lm(y~x,data=dt))[2,]
dt[,tidy(lm(y~x))[2,]]
glance(lm(y~x,data=dt))
dt[,glance(lm(y~x))]
augment(lm(y~x,data=dt))

dt <- build.nsim(1000,c(10,25,50,100,250))
bigN <- nrow(dt)
dt$x <- rnorm(bigN,2,1)
dt$y <- 2*dt$x + rnorm(bigN,0,1)
dt[,glance(lm(y~x)),by=.(n,sim)]
dt[,tidy(lm(y~x))[2,],by=.(n,sim)]
view.sim(dt[,tidy(lm(y~x))[2,],by=.(n,sim)])
ggplot(result,aes(x=estimate,fill=as.factor(n)))+geom_density()

dt$x <- rnorm(bigN,2,1)
dt$z <- rnorm(bigN,1,1)
dt$y <- 2*dt$x + 3*dt$z + rnorm(bigN,0,1)
view.sim(dt[,tidy(lm(y~x))[2,],by=.(n,sim)])

dt$w <- rnorm(bigN,0,0.5)
dt$x <- rnorm(bigN,2,0.5) + dt$w
dt$z <- rnorm(bigN,1,0.5) + dt$w
dt$y <- 2*dt$x + 3*dt$z + rnorm(bigN,0,1)
view.sim(dt[,tidy(lm(y~x))[2,],by=.(n,sim)])
view.sim(dt[,tidy(lm(y~x+z))[2,],by=.(n,sim)])

dt$x <- rnorm(bigN,2,1)
dt$xstar <- dt$x + rnorm(bigN,0,1)
dt$y <- 2*dt$x + rnorm(bigN,0,1)
dt$ystar <- dt$y + rnorm(bigN,0,1)
view.sim(dt[,tidy(lm(ystar~xstar))[2,],by=.(n,sim)])
view.sim(dt[,tidy(lm(y~xstar))[2,],by=.(n,sim)])
view.sim(dt[,tidy(lm(ystar~x))[2,],by=.(n,sim)])

dt$x <- rnorm(bigN,2,1)
dt$y <- 2*dt$x + rnorm(bigN,0,1)
result <- dt[,tidy(lm(x~y))[2,],by=.(n,sim)]
result$estimate <- 1/result$estimate
view.sim(result[estimate<5])
