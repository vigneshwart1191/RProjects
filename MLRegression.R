##Read data from table
winequality <- read.csv("C:/DataScience/MLDATA/winequality-red (1).csv")
attach(winequality)
winequality
##Explore the data
summary(winequality)
bx = boxplot(winequality)
hist(pH)
hist(sulphates)

##Scaling code to normalize entire data
scaling = function(x)
{
  x = (x-min(x))/(max(x)-min(x))
  return(x)
}

#scaling using log value
scale1 = function(y)
{
  y=log(y)
}
##Apply scaling for all data 
scalwine = apply(winequality,2,scaling)

##Apply scaling on log function
scalwine1 = apply(winequality,2,scale1)
###Elimination of Outliers
for(j in 1:ncol(scalwine)){
  v=summary(scalwine[,j])
  u=v[5]+1.5*IQR(v)
  l=v[2]-1.5*IQR(v)
  
  for(i in 1:nrow(scalwine)){
    if(scalwine[i,j]>u || scalwine[i,j]<l){
      scalwine[i,j]=mean(scalwine[,j])
    }
  }
}
scalwine=as.data.frame(scalwine)
class(scalwine)
attach(scalwine)
View(scalwine)
boxplot(scalwine)
summary(scalwine)
##Explore scaled data()
hist(scalwine$fixed.acidity)
hist(scalwine$volatile.acidity)
hist(scalwine$citric.acid) ##Not following Normal distribution
hist(scalwine$residual.sugar)
hist(scalwine$chlorides)
hist(scalwine$free.sulfur.dioxide)##Not following Normal distribution
hist(scalwine$total.sulfur.dioxide)##Not following Normal distribution
hist(scalwine$density)
hist(scalwine$pH)
hist(scalwine$sulphates)
hist(scalwine$alcohol)
hist(scalwine$quality)

##Find corelation between data
library(psych)
library(car)
#vif(scalwine)
cor(scalwine[c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")])
c1 = pairs(scalwine[c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")])
pairs.panels(scalwine[c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")])

##Partioning Train and Test data
ind = sample((1:nrow(scalwine)),round(0.70*nrow(scalwine)))
ind
train = scalwine[ind,]
train
test = scalwine[-ind,]
test

##Model generation
fis = lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,train)
fis
summary(fis)
plot(fis)
##Remove density because of p value 0.8
fis1 = lm(quality~fixed.acidity+volatile.acidity+pH+sulphates+alcohol,train)
fis1
summary(fis1)
plot(fis1)

##Remove free sulphur dioxide because of p value 0.8
fis2 = lm(quality~fixed.acidity+volatile.acidity+sulphates+alcohol,test)
fis2
summary(fis2)
p = predict(fis1,test)
p
