#Import the data set
scalchin <- read.csv("C:/Users/vignesht/Downloads/chin.csv")
View(chin)


##finding corelation between data
cor(chin)
library(psych)
library(car)
#partion the data
class1 = scalchin[1:25,]
class1
class0 = scalchin[26:114,]
class0
ind1 = sample((1:nrow(class1)),round(0.70*nrow(class1)))
ind1
ind0 = sample((1:nrow(class0)),round(0.70*nrow(class0)))
ind0
train1 =class1[ind1,]
train2=class0[ind0,]
train = rbind(train1,train2)
test1 = class1[-ind1,]
test2=class0[-ind0,]
test=rbind(test1,test2)

#model generation
fit = rpart((grade)~age+factor(ethnicity)+ER+PR+RT+CT+HT+N+tumorStage+tumorSize,train,method = "class",control = rpart.control(minsplit = 10))

#Model generation for entire data set
fit = glm(grade~age+ethnicity+ER+PR+RT+CT+HT+N+tumorStage+tumorSize,binomial(link = "logit"),chin)
fit
summary(fit)

#model generation for trian data set
fit1 = glm(grade~age+ethnicity+ER+PR+RT+CT+HT+N+tumorStage+tumorSize,binomial(link = "logit"),train)
fit1
summary(fit1)

#ethnicity removed due to less corelation with grade
fit1 = glm(grade~age+ER+PR+RT+CT+HT+N+tumorStage+tumorSize,binomial(link = "logit"),train)
fit1
summary(fit1)
plot(fit1)
predict = predict(fit1,type='response',test)
predict
length(predict)
##Accuracy of a model
totalpositive=0
totalnegative=0
for(i in 1:length(predict))
{
  if(predict[i]>0.5)
  {
    totalpositive=totalpositive+1
  }else{
    totalnegative=totalnegative+1
  }
}
cat("The total no of predicted person has cancer is ",totalpositive,"&those who dont have cancer is ",totalnegative)

#accuracy for this data set
truepositive=5
truenegative=28
falsepositive=2
falsenegative=0
accuracy=(truepositive+truenegative)/(totalpositive+totalnegative)
accuracy

