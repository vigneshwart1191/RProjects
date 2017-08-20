################Training data
#export the data
loanData <- read.csv("C:/DataScience/Analytics vidya/LoanPrediction/Data/train_u6lujuX_CVtuZ9i.csv")
View(loanData)

#explore the data
nrow(loanData) #small dataset
anyNA(loanData) #Need to replace NA
summary(loanData)
str(loanData)
hist(loanData$Loan_Amount_Term)
hist(loanData$Credit_History) #need to modify as factors

#####preporcessing steps to be followed before applying model

#########################################

#1.convert credit history as factorial data
loanData$Credit_History = as.factor(loanData$Credit_History)

#2.convert loan amount term also as factor
#loanData$Loan_Amount_Term = as.factor(loanData$Loan_Amount_Term)

#3.replace na value of Loan amont with mean amount
loanData[is.na(loanData[,9]),9] = mean(loanData[,9],na.rm = T)
anyNA(loanData$LoanAmount)

#4.replace loan amount term with mode to check the accuracy
loanData[is.na(loanData[,10]),10] = 360

#5.convert Na value of credit history to mode
loanData[is.na(loanData[,11]),11] = 1

#remove loan id
loanData1 = loanData[,-1]
View(loanData1)
#########################################

###############Test data
#import the data
testLoan <- read.csv("C:/DataScience/Analytics vidya/LoanPrediction/Data/test_Y3wMUE5_7gLdaTN.csv")
View(testLoan)

#explore the data
nrow(testLoan) #small dataset
anyNA(testLoan) #Need to replace NA
summary(testLoan)
str(testLoan)
hist(testLoan$Loan_Amount_Term)
hist(testLoan$Credit_History) #need to modify as factors

#####preporcessing steps to be followed before applying model

#########################################

#1.convert credit history as factorial data
testLoan$Credit_History = as.factor(testLoan$Credit_History)

#2.convert loan amount term also as factor
#testLoan$Loan_Amount_Term = as.factor(testLoan$Loan_Amount_Term)

#3.replace na value of Loan amont with mean amount
testLoan[is.na(testLoan[,9]),9] = mean(testLoan[,9],na.rm = T)
anyNA(testLoan$LoanAmount)

#4.replace loan amount term with mode to check the accuracy
testLoan[is.na(testLoan[,10]),10] = 360

#5.convert Na value of credit history to mode
testLoan[is.na(testLoan[,11]),11] = 1
length(loanData)

#
testLoan1 = testLoan[,-1]
View(testLoan1)
#########################################
#apply decision tree on this data set and check the prediction value
library(rpart)
fit5  = rpart(loanData1$Loan_Status~.,data = loanData1,method = "class",control = rpart.control(minsplit = 10,cp=0.015))
summary(fit5)

#########################################
#predict the output
library(caret)
LoanPrediction = predict(fit5,testLoan,type ="class")
nrow(LoanPrediction)
table(LoanPrediction)
p1 = as.data.frame(LoanPrediction)
View(p1)
nrow(p1)

#bind the data based on submission file

c1 = cbind(as.vector(testLoan$Loan_ID),p1$LoanPrediction)
dimnames(c1)=list(c(),c("Loan_ID","LoanStatus"))
LoanPredict = as.data.frame(c1)
class(LoanPredict)
View(LoanPredict)
write.csv(LoanPredict,"C:/DataScience/Analytics vidya/LoanPrediction/Data/loanprediction.csv")

