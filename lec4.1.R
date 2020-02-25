library(broom)
minwage <- wpull('minwage')
minwage2 <- minwage[,.(emp232,wage232,unem,cpi,minwage)]

ts.plot(minwage2$emp232)
ts.plot(minwage2$wage232)
ts.plot(minwage2$cpi)
minwage2$time <- seq(1950,2001-1/12,1/12)
ggplot(minwage2,aes(x=time,y=emp232)) + geom_line() +
  scale_x_continuous('Year (Monthly)') +
  scale_y_continuous('Employment in Thousands')
ggplot(minwage2,aes(x=time,y=wage232)) + geom_line() +
  scale_x_continuous('Year (Monthly)') +
  scale_y_continuous('Wages in Dollars per Hour')

model <- lm(emp232~wage232+time,data=minwage2)
summary(model)  ## Spurious correlation

library(tseries)
kpss.test(minwage2$emp232)  ## H_0: employment is level stationary (with no differencing)
kpss.test(minwage2$emp232,null="Trend")
kpss.test(diff(minwage2$emp232))
kpss.test(diff(minwage2$emp232),null="Trend")
# Employment in 232 is trend stationary after first-differencing

kpss.test(minwage2$wage232)
kpss.test(minwage2$wage232,null="Trend")
kpss.test(diff(minwage2$wage232))
kpss.test(diff(minwage2$wage232),null="Trend")
# Wages in 232 are also trend stationary after first-differencing

rep.kpss <- function(series){
  for(diffs in 0:5){
    suppressWarnings(
      pval <- kpss.test(series)$p.value
    )
    if(pval>=0.05){return(c(diffs,0,pval))}
    suppressWarnings(
      pval <- kpss.test(series,null="Trend")$p.value
    )
    if(pval>=0.05){return(c(diffs,1,pval))}
    series <- diff(series)
  }
}
rep.kpss(minwage2$emp232)
rep.kpss(minwage2$wage232)

rep.kpss <- function(series,max.reps=5,level=0.95,trend=TRUE){
  obj <- list(diffs = 0, trend=0, p.value=0)
  class(obj) <- "repkpss"
  for(i in 0:max.reps){
    suppressWarnings(pval <- tseries::kpss.test(series)$p.value)
    if(pval >= 1-level){
      obj$diffs <- i
      obj$trend <- 0
      obj$p.value <- pval
      return(obj)
    }
    if(trend){
      suppressWarnings(pval <- tseries::kpss.test(series,null="Trend")$p.value)
      if(pval >= 1-level){
        obj$diffs <- i
        obj$trend <- 1
        obj$p.value <- pval
        return(obj)
      }
    }
    series <- diff(series)
  }
}
rep.kpss(minwage2$emp232)
rep.kpss(minwage2$wage232)

modelb <- lm(diff(emp232)~diff(wage232)+time,data=minwage2)
modelb <- lm(diff(emp232)~diff(wage232)+time[2:nrow(minwage2)],data=minwage2)
summary(modelb)
NROW(minwage2$time)
NROW(minwage2$emp232)
NROW(diff(minwage2$emp232))


modelc <- lm(diff(log(emp232))~diff(log(wage232))+time[2:nrow(minwage2)],data=minwage2)
summary(modelc)

sandwich::NeweyWest(modelb)
tidynw <- function(model,...)tidyg(model,sandwich::NeweyWest(model),...)
tidynw(modelb)  ## Corrects for both autocorrelation and heteroskedasiticity but only in time series

library(forecast)
arima(minwage2$emp232,c(1,1,0))
ggtsdisplay(diff(minwage2$emp232))

modeld <- Arima(minwage2$emp232,c(5,1,0),include.drift = TRUE)
modeld
AIC(modeld)
BIC(modeld)


acf(diff(minwage2$emp232))
maxpq <- 5
outp <- matrix(0,(maxpq+1)^2,6)
count <- 1
for(i in 0:maxpq){
  for(j in 0:maxpq){
    modeld <- Arima(minwage2$emp232,c(i,1,j),include.drift = TRUE)
    outp[count,] <- c(i,1,j,AIC(modeld),BIC(modeld),modeld$aicc)
    count <- count + 1
  }
}
outp <- data.table(outp)
names(outp) <- c('p','d','q','aic','bic','aicc')
outp
outp[aic==0,]$aic <- 9999
outp[bic==0,]$bic <- 9999
outp[which.min(outp$aic),,]
head(outp[order(aic),])
head(outp[order(bic),])

modeld <- Arima(minwage2$emp232,c(12,1,12),include.drift = TRUE)
summary(modeld)
AIC(modeld)

?forecast
forecast(modeld,h=60)
plot(forecast(modeld,h=60))
modele <- Arima(minwage2$emp232,
                c(3,1,3),
                seasonal=list(order=c(1,1,1),period=6))
modelf <- Arima(minwage2$emp232,
                c(5,1,5),
                seasonal=list(order=c(0,1,1),period=12),
                include.drift = TRUE)
modelg <- Arima(minwage2$wage232,
                c(4,1,4),
                seasonal = list(order = c(1,1,1),period = 6))
autoplot(forecast(modelg,h = 60))
modele
AIC(modele)
AIC(modelf)
AIC(modeld)
plot(forecast(modelf,h=60))

## prophet model

demp <- diff(minwage2$emp232)
dwage <- diff(minwage2$wage232)
TSA::periodogram(diff(minwage2$emp232))
TSA::periodogram(dwage)
### NEVER EVER SAY: library(TSA)
1/0.17
1/0.33
d
ydata <- cbind(demp,dwage)
BIC(vars::VAR(xdata,1,type="both"))
BIC(vars::VAR(xdata,2,type="both"))
BIC(vars::VAR(xdata,3,type="both"))
summary(vars::VAR(ydata,2,type="both"))  ## Changes in employment Granger-cause a change in the wage
