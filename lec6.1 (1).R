data("Titanic")
titan <- data.table(Titanic)
rm(Titanic)

titan[c(32,32,32,32),]
repr <- rep(1:nrow(titan), titan$N)
titan <- titan[repr,]

titan$N <-NULL
titan$Survived <- as.factor(titan$Survived)
titan$Class <- as.factor(titan$Class)
titan$Sex <- as.factor(titan$Sex)
titan$Age <- as.factor(titan$Age)

glmodel <- glm(Survived~(Class+Sex+Age)^2,data=titan,family=binomial)
summary(glmodel)
pred1 <- predict(glmodel,type="response")
pred1 <- as.numeric(pred1 > 0.5)
mean(pred1 == as.numeric(titan$Survived=="Yes"))
table(pred1,titan$Survived) # Confusion matrix
accuracy <- (1470+270)/2201
accuracy
### Everybody dies model
accuracy <- 1490/2201
accuracy

library(partykit)
set.seed(75080)
tree <- ctree(Survived~Class+Sex+Age,data=titan)
plot(tree)
plot(nodeprune(tree,c(3,10)))
pred2 <- predict(tree)
mean(pred2 == titan$Survived)
table(pred2,pred1)
table(pred2,titan$Survived)
accuracy <- (1470+265)/2201
accuracy 

bwght <- wpull('bwght')
wage1 <- wpull('wage1')

bwght$smokes <- as.numeric(bwght$cigs>0)
bwght$smokes <- as.factor(bwght$smokes)
tree <- ctree(bwght~cigs+faminc+male+white,data=bwght)
plot(tree)
tree <- ctree(cigs~faminc+white,data=bwght)
plot(tree)
tree <- ctree(smokes~faminc+white,data=bwght)
plot(tree)

summary(glm(smokes~faminc+white,data=bwght,family=binomial()))
margins(glm(smokes~faminc+white,data=bwght,family=binomial()))

tree <- ctree(wage~educ+exper+tenure,data=wage1)
plot(tree)

glmtree(wage~educ+exper+tenure,data=wage1)
glmtree(wage~exper+tenure|educ,data=wage1)
glmtree(wage~tenure|educ,data=wage1)
glmtree(wage~educ+exper+tenure|nonwhite+female+married,data=wage1)
glmtree(wage~educ+exper+tenure|educ+exper+tenure,data=wage1)
