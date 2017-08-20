#apply preprocessing steps on test data
TbigMart <- read.csv("C:/Users/vignesht/Downloads/Test_u94Q5KV.csv")
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
write.csv("C:/Users/vignesht/Downloads/TbigMart.csv")
