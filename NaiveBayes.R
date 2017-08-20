#import the data set
flag.data <- read.csv("C:/Users/vignesht/Downloads/flag data.csv")
View(flag.data)
flag.data[,-c(4,5)]=lapply(flag.data[,-c(4,5)],factor)
#statistics of the data
summary(flag.data)
str(flag.data)
factor(flag.data$religion)

#convert as factor
flag.data[,-c(4,5)]=lapply(flag.data[,-c(4,5)],factor)
#subset the data
class0 = subset(flag.data,religion==0)
class1 = subset(flag.data,religion == 1)
class2 = subset(flag.data,religion == 2)
class3 = subset(flag.data,religion == 3)
class4 = subset(flag.data,religion == 4)
class5 = subset(flag.data,religion == 5)
class6 = subset(flag.data,religion == 6)
class7 = subset(flag.data,religion == 7)

#partion train and test samples
#partioning the data
ind0 = sample(1:nrow(class0),round(0.80*nrow(class0)))
ind1 = sample(1:nrow(class1),round(0.80*nrow(class1)))
ind2 = sample(1:nrow(class2),round(0.80*nrow(class2)))
ind3 = sample(1:nrow(class3),round(0.80*nrow(class3)))
ind4 = sample(1:nrow(class4),round(0.80*nrow(class4)))
ind5 = sample(1:nrow(class5),round(0.80*nrow(class5)))
ind6 = sample(1:nrow(class6),round(0.80*nrow(class6)))
ind7 = sample(1:nrow(class7),round(0.80*nrow(class7)))

#train data
train0=class0[ind0,]
train1=class1[ind1,]
train2=class2[ind2,]
train3=class3[ind3,]
train4=class4[ind4,]
train5=class5[ind5,]
train6=class6[ind6,]
train7=class7[ind7,]

#test data
test0=class0[-ind1,]
test1=class1[-ind1,]
test2=class2[-ind2,]
test3=class3[-ind3,]
test4=class4[-ind4,]
test5=class5[-ind5,]
test6=class6[-ind6,]
test7=class7[-ind7,]

#prepared train and test data
train=rbind(train0,train1,train2,train3,train4,train5,train6,train7)
test=rbind(test0,test1,test2,test3,test4,test5,test6,test7)
anyNA(train)
anyNA(test)
View(train)
View(test)
nrow(train)
nrow(test)
length(class0)

#apply naivebayes algorithm
library(e1071)
fit = naiveBayes(as.factor(religion)~.,data = train,laplace = .001)
summary(fit)
#prediction of data

predict1 = predict(fit,newdata = test)
acc = length(which(predict1 == test$religion))/length(test$religion)
table(predict1,test$religion)
predict1
table(test$religion)


#
hist(flag.data$area)
