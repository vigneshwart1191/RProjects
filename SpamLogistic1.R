#importing the data
spam <- read.csv("C:/Users/vignesht/Downloads/spam.csv", header=T)
#View(spam)

#preproccesing the data
#str(spam)
dim(spam)
#str(spam)
summary(spam)
boxplot(spam)
#Scaling function 
scaling = function(y)
{
  y = (y-min(y))/(max(y)-min(y))
}

##apply scaling for data set
scalespam = apply(spam,2,scaling)
scalespam
#View(scalespam)
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
trainn = train[,c(-32,-36,-40)]
testt = test[,c(-32,-36,-40)]
View(testt)
#generate a intial model
fit = glm(trainn$V58 ~ ., binomial(link = "logit"),data=train)
fit
summary(fit)
plot(fit)
#generate a model after removing fetures which is highly corelated
fit = glm(trainn$V58 ~ ., binomial(link = "logit"),data=trainn)
fit
class(fit)
str(fit)
summary(fit)
plot(fit)

#predict with test data
prd_spam1 = predict(fit,type='response', testt)
prd_spam1


#confusion matrix
CM = table(testt$V58, prd_spam1 > 0.5)

#accuracy 
acc = (CM[2,2]+CM[1,1])/(CM[1,1]+CM[2,1]+CM[1,2]+CM[2,2])
acc

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(prd_spam1, testt$V58)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


#conversion of predict value into test value
for(i in 1:length(prd_spam1))
{
  if(prd_spam1[i]>0.5)
  {
    prd_spam1[i] = 1
  }else{
    prd_spam1[i] = 0
  }
}
length(prd_spam1)


#accuracy calculation
count=0
for (i in 1:nrow(testt)) {
  if(testt$V58[i] == prd_spam1[i])
  {
    count=count+1
  }
}
count

##
acc = count/length(prd_spam1)
acc


