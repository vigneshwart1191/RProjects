#Importing the data sets
#1.BankMaster
BankMaster <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/BankMaster.csv")
summary(BankMaster)
#2.CategoryMaster
CategoryMaster <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/CategoryMaster.csv")
summary(CategoryMaster)
#3.CustomerMaster
CustomerMaster <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/CustomerMaster.csv")
summary(CustomerMaster)
#4.MerchantMaster
MerchantMaster <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/MerchantMaster.csv")
summary(MerchantMaster)
#5.OfferCategorization
OfferCategorization <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/OfferCategorization.csv")
summary(OfferCategorization)
#6.OfferLocation
OfferLocation <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/OfferLocation.csv")
summary(OfferLocation)
#7.OfferMaster
OfferMaster <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/OfferMaster.csv", header=T)
summary(OfferMaster)
#8.Payment
Payment <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/Payment.csv")
summary(Payment)
#9.Seller
Seller <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/Seller.csv")
summary(Seller)
#10.Shop
Shop <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/Shop.csv")
View(Shop)
summary(Shop)
#11.Events_Data
Events_Data <- read.csv("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/Events_Data.txt", header=T)
View(Events_Data)
#merging data of OfferLocation and shopID
offerLocShp = merge(OfferLocation,Shop,by="ShopID",all.y = T)
View(offerLocShp)

#merging data of OfferCategorizatipn and CategoryMaster
offerCatCat = merge(OfferCategorization,CategoryMaster,by="CategoryID")
View(offerCatCat)

#merge offerMaster with offLocShp
View(OfferMaster)
offMasLoc=merge(OfferMaster,offerLocShp,by="OfferID",all.x = T)
View(offMasLoc)
#merge data of offMasLoc and offerCatCat
offerMasCat=merge(offMasLoc,offerCatCat,by="OfferID",all.x = T)
View(offerMasCat)

#merging data of Merchant master and merchantId
offerMerMas = merge(MerchantMaster,Seller,by="MerchantID",all.x = T)
View(offerMerMas)
View(Payment)
#merging payment and mermas
offPayMer = merge(offerMerMas,Payment)
View(offPayMer)

#merge offpaymer and offerloccat
offMas = merge(offPayMer,offerMasCat,by = "OfferID")
View(offMas)

#Unstructured to Structured file
#the given json input file is need to be added with comma after every record.
library(dplyr)
library(rjson)
library(jsonlite)
library(RJSONIO)
json_file <- fromJSON("C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/Events_Data1.json", simplifyDataFrame = FALSE)
####
json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

class(json_file)
Customer_Offer <- do.call("rbind", json_file)##it is convrting into data frame
Customer_Offer <- as.data.frame(Customer_Offer)
View(Customer_Offer)
CustOffer <-  select(Customer_Offer,attributes.Merchants,attributes.Locations)
CustOfferResult <- filter(CustOffer, nchar( as.character( attributes.Merchants)) == 5 &  nchar( as.character( attributes.Locations)) <= 3 )
CustOfferResult <- rename(CustOfferResult, OfferID = attributes.Merchants , CustomerID = attributes.Locations )
View(CustOfferResult)
##CustOfferResult <- CustOfferResult[ ! duplicated( CustOfferResult[ c("OfferID" , "CustomerID") ] ) , ]
#CustOfferResult <- CustOfferResult[order(CustOfferResult$CustomerID, CustOfferResult$OfferID),]
#CustOfferResult <- CustOfferResult[order(CustOfferResult$CustomerID),] 
#CustOfferResult <- CustOfferResult[with(CustOfferResult, order(CustomerID, OfferID)),]
write.csv(CustOfferResult, "cust1.csv", na ="0")