
french <- fread('french.csv')
summary(french)
french$date <- as.Date(french$date)
french$date <- paste(substr(french$date,1,4),substr(french$date,5,6),'01',sep = "-")
french$date
for(c in 2:50){
  series <- data.frame(french)[,c]
  kpss <- rep.kpss(series)
  print(c(names(series),kpss$trend,kpss$diffs))
}
french <- french[complete.cases(french)]
head(french)
panel <- french[,2:50]
pca <- prcomp(panel,scale=TRUE)
pca.select(pca)
pca$rotation[,1:3]

french$factor <- pca$x[,1]
french$factor <- scale(french$factor)
colnames(french)
pca$rotation[,1:3]

modely <- lm(Fin~factor,data=french)
summary(modely)
cor(french$Fin,french$factor)
modelx <- lm(Gold~factor,data=french)
summary(modelx)
cor(french$Gold,french$factor)

french <- fread('french.csv')
market <- fread('market.csv')
market$sp500 <- market$sp500*100
market$djia <- market$djia*100
market$nasdaq <- market$nasdaq*100
french <- merge(french,market,all=FALSE)
french$date <- as.Date(paste(substr(french$date,1,4),substr(french$date,5,6),'01',sep = "-"))
french[complete.cases(french)]
french <- french[complete.cases(french)]
french
panel <- french[,2:50]
pca <- prcomp(panel,scale=T)
pca.select(pca)
french$factor1 <- scale(prcomp(panel,scale=T)$x[,1])
##french$factor2 <- scale(prcomp(panel,scale=T)$x[,2])
french$factor1 <- -french$factor1

ggplot(french,aes(x=date)) + 
  geom_line(aes(y=scale(factor1)),color="black") +
  geom_line(aes(y=scale(sp500)),color="red") +
  geom_line(aes(y=scale(djia)),color="blue") +
  geom_line(aes(y=scale(nasdaq)),color="green") +
  scale_y_continuous('Standard deviations')
cor(french$factor1,french$nasdaq)
ggplot(french,aes(x=date)) + 
  geom_line(aes(y=scale(factor1)),color="black") +
  geom_line(aes(y=scale(mkt)),color="red") +
  geom_line(aes(y=scale(smb)),color="blue") +
  geom_line(aes(y=scale(hml)),color="green") +
  scale_y_continuous('Standard deviations')
cor(french$factor1,french$mkt)

summary(lm(Fin~factor1,data=french)) **
summary(lm(Fin~sp500,data=french))
summary(lm(Fin~djia,data=french))
summary(lm(Fin~nasdaq,data=french))
summary(lm(Fin~djia+sp500,data=french))
summary(lm(Fin~mkt+smb+hml,data=french))

pc.test(panel,french[,.(factor1)])
pc.test(panel,french$sp500)
pc.test(panel,french$djia)
pc.test(panel,french$nasdaq)
pc.test(panel,french[,.(djia,sp500,nasdaq)])
pc.test(panel,french[,.(mkt,smb,hml)])
pc.test(panel,french[,.(mkt,smb)])
pc.test(panel,french[,.(smb,hml)])
pc.test(panel,french[,.(mkt,hml)])
pc.test(panel,french$mkt)
pc.test(panel,french$smb)   ##parker n c test
summary(lm(factor1~sp500,data=french))
attach(french)
cor(mkt,sp500)
detach(french)
