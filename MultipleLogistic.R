#import the table
glass <- read.csv("C:/Users/vignesht/Downloads/glass.csv")
View(glass)
#exlpre thr data set
str(glass) ##all fetures are in numerical or integer values
summary(glass)
head(glass)
boxplot(glass)
#scale the data
scaling = function(x)
{
  x = (x-min(x))/(max(x)-min(x))
}
#apply scaling on your entire data set
glass1 = apply(glass[,1:9],2,scaling)
View(glass1)
#append glass 1 independent variables with glass dependent variables
glass3 = cbind(glass1,glass[,10])
View(glass3)
glass2 = as.data.frame(glass3)

#partioning the data
class1 = subset(glass2,V10 == 1)
class2 = subset(glass2,V10== 2)
class3 = subset(glass2,V10== 3)
class5 = subset(glass2,V10== 5)
class6 = subset(glass2,V10== 6)
class7 = subset(glass2,V10== 7)


#partioning the data
ind1 = sample(1:nrow(class1),round(0.80*nrow(class1)))
ind2 = sample(1:nrow(class2),round(0.80*nrow(class2)))
ind3 = sample(1:nrow(class3),round(0.80*nrow(class3)))
ind5 = sample(1:nrow(class5),round(0.80*nrow(class5)))
ind6 = sample(1:nrow(class6),round(0.80*nrow(class6)))
ind7 = sample(1:nrow(class7),round(0.80*nrow(class7)))

train1=class1[ind1,]
train2=class2[ind2,]
train3=class3[ind3,]
train5=class5[ind5,]
train6=class6[ind6,]
train7=class7[ind7,]

test1=class1[-ind1,]
test2=class2[-ind2,]
test3=class3[-ind3,]
test5=class5[-ind5,]
test6=class6[-ind6,]
test7=class7[-ind7,]

train=rbind(train1,train2,train3,train5,train6,train7)
test=rbind(test1,test2,test3,test5,test6,test7)
View(train)
View(test)

#conversion of binary values
train1  = train
train1$V10 = ifelse(train1$V10 != 1,0,1)
test1  = test
test1$V10 = ifelse(test1$V10 != 1,0,1)

#conversion of binary values
train2  = train
train2$V10 = ifelse(train2$V10 != 2,0,1)
test2  = test
test2$V10 = ifelse(test2$V10 != 2,0,1)
#conversion of binary values
train3  = train
train3$V10 = ifelse(train3$V10 != 3,0,1)
test3  = test
test3$V10 = ifelse(test3$V10 != 3,0,1)
#conversion of binary values
train5  = train
train5$V10 = ifelse(train5$V10 != 5,0,1)
test5 = test
test5$V10 = ifelse(test5$V10 != 5,0,1)
#conversion of binary values
train6  = train
train6$V10 = ifelse(train6$V10 != 6,0,1)
test6  = test
test6$V10 = ifelse(test6$V10 != 6,0,1)
#conversion of binary values
train7  = train
train7$V10 = ifelse(train7$V10 != 7,0,1)
test7  = test
test7$V10 = ifelse(test7$V10 != 7,0,1)
View(test)
#create logistic regression for all models
model1 = glm(V10~.,binomial(link = "logit"),train1)
summary(model1)
#create logistic regression for all models
model2 = glm(V10~.,binomial(link = "logit"),train2)
summary(model2)
#create logistic regression for all models
model3 = glm(V10~.,binomial(link = "logit"),train3)
summary(model3)
#create logistic regression for all models
model5 = glm(V10~.,binomial(link = "logit"),train5)
summary(model5)
#create logistic regression for all models
model6 = glm(V10~.,binomial(link = "logit"),train6)
summary(model6)
#create logistic regression for all models
model7 = glm(V10~.,binomial(link = "logit"),train7)
summary(model7)
#prediction
predict1 = predict(model1,type='response',test1)
predict1

#prediction
predict2 = predict(model2,type='response',test2)
predict2

#prediction
predict3 = predict(model3,type='response',test3)
predict3

#prediction
predict5 = predict(model5,type='response',test5)
predict5

#prediction
predict6 = predict(model6,type='response',test6)
predict6

#prediction
predict7 = predict(model7,type='response',test7)
predict7

#predicting class lables
p = cbind(predict1,predict2,predict3,c(rep(0,43)),predict5,predict6,predict7)
View(p)

##
cl1 = c()

for(i in 1:nrow(p))
{
  for(j in 1:ncol(p))
  {
    if(p[i,j] == max(p[i,]))
    {
      cl1 = c(cl1,j)
    }
  }
}

table(cl1,test$V10)
#accuracy
acc=length(which(cl1 ==test$V10))/nrow(p)






#find correlation between fetures
#corelation of data set
cor1 = cor(glass)
cor1
#corelation function
x=c()
y=c()
z=c()
for(i in 1:nrow(cor1))
{
  for(j in 1:ncol(cor1))
  {
    if(i == j)
    {
      next
    }else if (cor1[i,j]> 0.7 || cor1[i,j] <  -0.7){
      x=c(x,cor1[i,j])
      y=c(y,i)
      z=c(z,j)
    }
  }
}
x
y
z

