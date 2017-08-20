#import the data
bigMart <- read.csv("C:/DataScience/Analytics vidya/BigMartSales/DataSets/Train_UWu5bXk.csv")
View(bigMart)

#visualize the data
str(bigMart)
str(bigMart$Item_Fat_Content)
nrow(is.na(bigMart[,9]))
(summary(bigMart))
which(is.na(bigMart))
hist(bigMart$Item_Weight)
summary(bigMart$Outlet_Size)

##prepocessing need to be done for train data
#replace NA with mean for item weight
bigMart[is.na(bigMart[,2]), 2] <- median(bigMart[,2], na.rm = TRUE)
#convert year into factor
bigMart$Outlet_Establishment_Year = as.factor(bigMart$Outlet_Establishment_Year)
class(bigMart$Outlet_Establishment_Year)

#convert fat values into 2 factor values
library(plyr)
bigMart$Item_Fat_Content <- revalue(bigMart$Item_Fat_Content,
                                    c("LF" = "Low Fat", "low fat" = "Low Fat", "reg" = "Regular"))

#convert factor data into numerical data
##1.Item_Fat_Content
bigMart$Item_Fat_Content = as.numeric(bigMart$Item_Fat_Content)
##2.Item_Type
bigMart$Item_Type = as.numeric(bigMart$Item_Type)
##3.Outlet_Identifier
bigMart$Outlet_Identifier = as.numeric(bigMart$Outlet_Identifier)
##4.outlet size
bigMart$Outlet_Size = as.numeric(bigMart$Outlet_Size)
##5.outlet location
bigMart$Outlet_Location_Type = as.numeric(bigMart$Outlet_Location_Type)
##6.Outlet type
bigMart$Outlet_Type= as.numeric(bigMart$Outlet_Type)
##7.
#scaling
scaling = function(x)
{
  x = (x-min(x))/(max(x)-min(x))
  return(x)
}

##Apply scaling for all data 
bigMart[,c(2,4,6)] = apply(bigMart[,c(2,4,6)],2,scaling)
View(bigMart)

#apply preprocessing steps on test data
TestbigMart = read.csv("C:/Users/vignesht/Downloads/Test_u94Q5KV.csv")
TbigMart <- read.csv("C:/DataScience/Analytics vidya/BigMartSales/DataSets/Test_u94Q5KV.csv")
View(TbigMart)
##prepocessing need to be done for train data
#replace NA with mean for item weight
TbigMart[is.na(TbigMart[,2]), 2] <- mean(TbigMart[,2], na.rm = TRUE)
#convert year into factor
TbigMart$Outlet_Establishment_Year = as.factor(TbigMart$Outlet_Establishment_Year)
class(TbigMart$Outlet_Establishment_Year)

#convert factor data into numerical data
##1.Item_Fat_Content
TbigMart$Item_Fat_Content = as.numeric(TbigMart$Item_Fat_Content)
##2.Item_Type
TbigMart$Item_Type = as.numeric(TbigMart$Item_Type)
##3.Outlet_Identifier
TbigMart$Outlet_Identifier = as.numeric(TbigMart$Outlet_Identifier)
##4.outlet size
TbigMart$Outlet_Size = as.numeric(TbigMart$Outlet_Size)
##5.outlet location
TbigMart$Outlet_Location_Type = as.numeric(TbigMart$Outlet_Location_Type)
##6.Outlet type
TbigMart$Outlet_Type= as.numeric(TbigMart$Outlet_Type)
##7.
#scaling
scaling = function(x)
{
  x = (x-min(x))/(max(x)-min(x))
  return(x)
}

##Apply scaling for all data 
TbigMart[,c(2,4,6)] = apply(TbigMart[,c(2,4,6)],2,scaling)
View(TbigMart)
#write.csv(TbigMart,"C:/Users/vignesht/Downloads/TbigMart.csv")

#apply random forest on data set
library(randomForest)
fit = randomForest(bigMart$Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type,bigMart,ntree=1000)
summary(fit)
which.min(fit$err.rate[,1])
#predict the output
Item_Outlet_Sales= predict(fit,TbigMart)
p1 = as.data.frame(Item_Outlet_Sales)
View(p1)

#bind the data based on submission file
TbigMart
c1 = cbind(as.vector(TbigMart$Item_Identifier),as.vector(TestbigMart$Outlet_Identifier),p1$Item_Outlet_Sales)
dimnames(c1)=list(c(),c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales"))
bigMartPredict = as.data.frame(c1)
class(bigMartPredict)
View(bigMartPredict)
write.csv(bigMartPredict,"C:/DataScience/Analytics vidya/BigMartSales/DataSets/bigMartPredict2.csv")


