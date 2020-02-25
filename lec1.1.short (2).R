## Comments go in space after # symbol
## In green text

library(data.table)           ## Get us the data.table functions
churn <- fread('churn.csv')   ## Data.table package to read data into memory

## Primary data validation
head(churn)
summary(churn)

## Variable modification
churn$bill <- churn$day_charges+churn$eve_charges+churn$night_charges+churn$intl_charges
churn$churn <- as.numeric(churn$churn=="True.")
churn$intl_plan <- as.numeric(churn$intl_plan=="yes")
churn$vm_plan <- as.numeric(churn$vm_plan=="yes")
churn$state <- NULL
churn$phone <- NULL


## Plots
plot(churn$length,churn$cs_calls)
ggplot(churn,aes(x=length,y=cs_calls))+geom_point()+
  scale_x_continuous(name="Account length")+
  scale_y_continuous(name="Customer service calls")

## Group-by statements
##churn[row,column,by=??]
churn[order(cs_calls),c(.N,lapply(.SD,mean)),by=cs_calls]
churn[order(churn),c(.N,lapply(.SD,mean)),by=churn]

## Cutting data
churn$length_bins <- cut(churn$length,seq(0,250,25))
churn[order(length_bins),c(.N,lapply(.SD,mean)),by=length_bins]
churn$length_bins <- NULL


