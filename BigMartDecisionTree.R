#import the data
bigMart <- read.csv("C:/Users/vignesht/Downloads/Train_UWu5bXk.csv")
View(bigMart)


#graph of data
hist(bigMart$Item_Outlet_Sales)


#visualize the data
str(bigMart)
str(bigMart$Item_Fat_Content)
nrow(is.na(bigMart[,9]))
summary(bigMart)
which(is.na(bigMart))
hist(bigMart$Item_Weight)
summary(bigMart$Outlet_Size)


# clearly, assigning a fat content to non-food items, i.e. the
# categories "Health and Hygiene", "Household" and "Others"
# makes no sense.
# We'll introduce a fat level "None" for them

levels(bigMart$Item_Fat_Content) <- c(levels(bigMart$Item_Fat_Content), "None")

bigMart[ which(bigMart$Item_Type == "Health and Hygiene") ,]$Item_Fat_Content <- "None"
bigMart[ which(bigMart$Item_Type == "Household") ,]$Item_Fat_Content <- "None"
bigMart[ which(bigMart$Item_Type == "Others") ,]$Item_Fat_Content <- "None"

bigMart$Item_Fat_Content <- factor(bigMart$Item_Fat_Content)
#check how many levels in fat content
levels(bigMart$Item_Fat_Content)


##prepocessing need to be done for train data
#replace NA with mean for item weight
bigMart[is.na(bigMart[,2]), 2] <- mean(bigMart[,2], na.rm = TRUE)
#convert year into factor
bigMart$Outlet_Establishment_Year = as.factor(bigMart$Outlet_Establishment_Year)
class(bigMart$Outlet_Establishment_Year)

#convert fat values into 2 factor values
library(plyr)
bigMart$Item_Fat_Content <- revalue(bigMart$Item_Fat_Content,
                                    c("LF" = "Low Fat", "low fat" = "Low Fat", "reg" = "Regular"))
levels(bigMart$Item_Fat_Content)

#apply preprocessing steps on test data
TestbigMart = read.csv("C:/Users/vignesht/Downloads/Test_u94Q5KV.csv")
TbigMart <- read.csv("C:/Users/vignesht/Downloads/Test_u94Q5KV.csv")
View(TbigMart)
##prepocessing need to be done for train data
#replace NA with mean for item weight
TbigMart[is.na(TbigMart[,2]), 2] <- mean(TbigMart[,2], na.rm = TRUE)
#convert year into factor
TbigMart$Outlet_Establishment_Year = as.factor(TbigMart$Outlet_Establishment_Year)
class(TbigMart$Outlet_Establishment_Year)

# We'll introduce a fat level "None" for them

levels(TbigMart$Item_Fat_Content) <- c(levels(TbigMart$Item_Fat_Content), "None")

TbigMart[ which(TbigMart$Item_Type == "Health and Hygiene") ,]$Item_Fat_Content <- "None"
TbigMart[ which(TbigMart$Item_Type == "Household") ,]$Item_Fat_Content <- "None"
TbigMart[ which(TbigMart$Item_Type == "Others") ,]$Item_Fat_Content <- "None"

TbigMart$Item_Fat_Content <- factor(TbigMart$Item_Fat_Content)


#replace new 3 levels
TbigMart$Item_Fat_Content <- revalue(TbigMart$Item_Fat_Content,
                                     c("LF" = "Low Fat", "low fat" = "Low Fat", "reg" = "Regular"))


#apply tree regression support vector for this problem
library(rpart)
fit = rpart(bigMart$Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Identifier+Outlet_Size+Outlet_Location_Type+Outlet_Type,bigMart,method = "anova",control = rpart.control(minsplit = 1),parms = list(split = "gini"))
summary(fit)

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
write.csv(bigMartPredict,"C:/Users/vignesht/Downloads/bigMartPredict5.csv")


