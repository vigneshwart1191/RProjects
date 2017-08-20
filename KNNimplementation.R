#explore the data set
winequality.red <- read.csv("C:/Users/vignesht/Downloads/winequality-red.csv")
View(winequality.red)
table(is.na(winequality.red))
#View(winequality.red)
#str(winequality.red)
#preproccesing the data


#scale the data
scaling = function(y)
{
  y = (y-min(y))/(min(y)-max(y))
}
#apply scaling for all data
wine1 = apply(winequality.red[,1:11],2,scaling)
#column bind
wine2 = cbind(wine1,winequality.red[,12])
#convert matrix to data frame
wine=as.data.frame(wine2)
#partioning the data
class3 = subset(wine,V12== 3)
class4 = subset(wine,V12== 4)
class5 = subset(wine,V12== 5)
class6 = subset(wine,V12== 6)
class7 = subset(wine,V12== 7)
class8 = subset(wine,V12== 8)

#partioning the data
ind3 = sample(1:nrow(class3),round(0.80*nrow(class3)))
ind4 = sample(1:nrow(class4),round(0.80*nrow(class4)))
ind5 = sample(1:nrow(class5),round(0.80*nrow(class5)))
ind6 = sample(1:nrow(class6),round(0.80*nrow(class6)))
ind7 = sample(1:nrow(class7),round(0.80*nrow(class7)))
ind8 = sample(1:nrow(class8),round(0.80*nrow(class8)))
train3=class3[ind3,]
train4=class4[ind4,]
train5=class5[ind5,]
train6=class6[ind6,]
train7=class7[ind7,]
train8=class8[ind8,]
test3=class3[-ind3,]
test4=class4[-ind4,]
test5=class5[-ind5,]
test6=class6[-ind6,]
test7=class7[-ind7,]
test8=class8[-ind8,]
train=rbind(train3,train4,train5,train6,train7,train8)
test=rbind(test3,test4,test5,test6,test7,test8)
#View(train)


#install a class package and build the knn algorithm
library(class)
#use KNN1 algoritm
model1 = knn1(train[,-12],test[,-12],as.factor(train[,12]))
summary(model1)
acc = length(which(model1 == as.factor(test[,12]),T))/length(test[,12])

#use cross valdiation to improve efficiency
cv1 =c()
for(i in 1:35)
{
  for(j in 1:35)
  {
  cvr = knn.cv(train[,-12],as.factor(train[,12]),k =i,l=j-1,prob=F,use.all = T)
  cv1=c(cv1,length(which(cvr == as.factor(train[,12]),T))/length(train[,12]))
}}
#use KNN algorithm
fit = knn(train = train[,-12],test = test[,-12],k =1,cl= as.factor(train[,12]),l=0,use.all = T,prob = F)
acc = length(which(fit == as.factor(test[,12]),T))/length(test[,12])
#print(fit)
print(acc)



fit
##Condense evaluation
keep=condense(train[,-12],as.factor(train[,12]))
fit1 = knn(train = train[keep,-12],test = test[,-12],k =1,cl= as.factor(train[keep,12]),l=0,use.all = T,prob = F)
acc = length(which(fit1 == as.factor(test[,12]),T))/length(test[,12])
#evaluation the model
library(gmodels)
CrossTable(x=as.factor(test[,12]),y = fit1,prop.chisq = F)

