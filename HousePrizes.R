##########House prize problem
##########Problem statment:To predict the sale prize of the property


#Train data
train<- read.csv("C:/DataScience/Analytics vidya/HousePrice/train (1).csv", header=T)
View(`train`)


#Test data
test <- read.csv("C:/DataScience/Analytics vidya/HousePrice/test.csv")
View(test)


######Exploring the data
nrow(train) #1460 entries are there in train
anyNA(train) #there are lot of NA values so preprocessing is required for the NA
length(train) #80 independent variables
summary(train)
str(train)


#####Preporcessing 
#1.MSSubClass convert into factor
as.factor(train[,2])

#1.Lot Frontage(Linear)
str