#import the data
Telco <- read.csv("C:/Users/vignesht/Downloads/Telco-Customer-Churn.csv")
View(Telco)

#explore the data
anyNA(Telco)
summary(Telco)
summary(Telco$TotalCharges)
str(Telco)


#replace Na value with particular colum nean
Telco[is.na(Telco[,19]),19] = mean(Telco[,19],na.rm = T)
#check once any na in data set


#check any outliers is there in numerical data set
boxplot(Telco$MonthlyCharges)
boxplot(Telco$TotalCharges)

#senior citizen is int data but it has values as 0 and 1 so convert it as categorical
Telco$SeniorCitizen = as.factor(Telco$SeniorCitizen)
class(Telco$SeniorCitizen)

#exclude first column alone to pass data into model
Telco1 = Telco[-1]

#partition train and test data
YesData = subset(Telco1,Telco1$Churn == "Yes")
NoData = subset(Telco1,Telco1$Churn == "No")


#train data
ind1 = sample((1:nrow(YesData)),round(.80*nrow(YesData)))
ind2 = sample((1:nrow(NoData)),round(0.80*nrow(NoData)))
train1 = YesData[ind1,]
train2 = NoData[ind2,]
test1 = YesData[-ind1,]
test2 = NoData[-ind2,]

#combine train and test data
train = rbind(train1,train2)
test = rbind(test1,test2)
nrow(train)
nrow(test)
#Now almost all the data is categorical and has only two fetures are numerical
#apply tree regression support vector for this problem
#model using gini index
library(rpart)
lossmatrix = matrix(c(0,0.6,0.4,0),nrow = 2,byrow = T)
fit = rpart(Churn ~ .,data = train,method = "class",control = rpart.control(minsplit = 10,xval = 20),parms = list(loss = lossmatrix,split = "gini") )
summary(fit)
summary(fit$splits)
plot(fit$splits)
plot(fit)
#using homogenity of data
fit1 = rpart(Churn ~ ., data = train,method = "class",control = rpart.control(minsplit = 10))
fit1
library(rattle)
library(rpart.plot)
fancyRpartPlot(fit)
fancyRpartplot(fit)
#purning the tree
fit1 = prune(fit,cp = 0.01)

#predict the output
Churn1 = predict(fit,test,type = "class")
table(Churn1,test$Churn)

#plot roc curve
#ROCR Curve
#ROCR Curve
library(ROCR)
#prediction(predictions = as.vector(Churn1),labels = as.factor(test$Churn))
ROCRpred = prediction(Churn1,test$Churn)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
