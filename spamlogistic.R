#importing the data
spamread <- read.csv("C:/Users/vignesht/Downloads/spam.csv", header=T)
View(spam)

#preproccesing the data
#Scaling function 
scaling = function(y)
{
  y = (y-min(y))/(max(y)-min(y))
}

##apply scaling for data set
scalespam = apply(spamread,2,scaling)
scalespam
View(scalespam)
scalespam1 = as.data.frame(scalespam)

#corelation of data set
cor1 = cor(scalespam1)
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
      #print(i)
      #print(j)
    }
  }
}
x
y
z
##which have fetures are 31,32,34,36,40 
#We can eliminate fetures of column 31,36,40

#data partioning
spam1 = subset(scalespam1,V58 == 1)
notspam1 = subset(scalespam1,V58 == 0)
View(spam1)

#assingn train and test data
ind1 = sample(1:nrow(spam1),round(0.70*nrow(spam1)))
ind0 = sample(1:nrow(notspam1),round(0.70*nrow(notspam1)))
train1 = spam1[ind1,]
train0 = notspam1[ind0,]
test1 = spam1[-ind1,]
test0 = notspam1[-ind0,]
test = rbind(test1,test0)
train = rbind(train1,train0)
str(train)
attach(train)
attach(test)
#train data on removing fetures
trainn = train[,c(-31,-36,-40)]
testt = test[,c(-31,-36,-40)]
length(testt)
View(testt)
#generate a model
fit = glm(trainn$V58 ~ ., binomial(link = "logit"),data=trainn)
fit
summary(fit)
plot(fit)
#predict with test data
library(ROCR)
predict1 = predict(fit,type='response', testt)
predict1
length(predict1)
accuracy <- table(predict1, testt[,"V58"])
sum(diag(accuracy))/sum(accuracy)
confusionMatrix(predict1, testt$V58)
