#1 Car seats problem

library(ISLR)
attach(Carseats)
View(Carseats)


#explore the data
anyNA(Carseats)  #no NA
boxplot(Carseats)
summary(Carseats)
str(Carseats)
nrow(Carseats)


#generate a model
library(rpart)
model = rpart(US~.,data = Carseats,method = "class",minsplit = 1)
summary(model)

#2 Boston data frame
library(MASS)
attach(Boston)

#explore the data
anyNA(Boston)
boxplot(Boston)
summary(Boston)
str(Boston)
View(Boston)


#generate a model
model1 = rpart(medv~.,data = Boston,method = "anova",minsplit = 1)
