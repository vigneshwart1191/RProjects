library(dplyr)
library(rjson)
library(jsonlite)
library(RJSONIO)
library(magrittr)
library(recommenderlab)
#Importing the data sets
#1.BankMaster
BankMaster <- read.csv("BankMaster.csv")
summary(BankMaster)
head(BankMaster)

#2.CategoryMaster
CategoryMaster <- read.csv("CategoryMaster.csv")
summary(CategoryMaster)
head(CategoryMaster)

#3.CustomerMaster
CustomerMaster <- read.csv("CustomerMaster.csv")
summary(CustomerMaster)
head(CustomerMaster)

#4.MerchantMaster
MerchantMaster <- read.csv("MerchantMaster.csv")
summary(MerchantMaster)
head(MerchantMaster)

#5.OfferCategorization
OfferCategorization <- read.csv("OfferCategorization.csv")
summary(OfferCategorization)
head(OfferCategorization)

#6.OfferLocation
OfferLocation <- read.csv("OfferLocation.csv")
summary(OfferLocation)
head(OfferLocation)

#7.OfferMaster
OfferMaster <- read.csv("OfferMaster.csv", header=T)
summary(OfferMaster)
head(OfferMaster)

#8.Payment
Payment <- read.csv("Payment.csv")
summary(Payment)
head(Payment)

#9.Seller
Seller <- read.csv("Seller.csv")
summary(Seller)
head(Seller)

#10.Shop
Shop <- read.csv("Shop.csv")
View(Shop)
summary(Shop)
head(Shop)

#11.Events_Data
Events_Data <- read.csv("Events_Data.txt", header=T)
View(Events_Data)
head(Events_Data)


#merging data of OfferLocation and shopID
offerLocShp = merge(OfferLocation,Shop,by="ShopID",all.y = T)
View(offerLocShp)
head(offerLocShp)

#merging data of OfferCategorizatipn and CategoryMaster
offerCatCat = merge(OfferCategorization,CategoryMaster,by="CategoryID")
View(offerCatCat)
head(offerCatCat)

#merge offerMaster with offLocShp
View(OfferMaster)
offMasLoc=merge(OfferMaster,offerLocShp,by="OfferID",all.x = T)
View(offMasLoc)
head(offMasLoc)

#merge data of offMasLoc and offerCatCat
offerMasCat=merge(offMasLoc,offerCatCat,by="OfferID",all.x = T)
View(offerMasCat)
head(offerMasCat)


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

#converting json data into data frame
jsonEventsData =readLines(file = "C:/Users/vignesht/Downloads/Analytics Assignment (1)/Analytics Assignment/Events_Data1.json")

#converting json into data frame


##########

#Unstructured to Structured file
#the given json input file is need to be added with comma after every record.
json_file <- fromJSON("C:/Users/vignesht/Downloads/Events_Data.json", simplifyDataFrame = FALSE)
####
json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

Customer_Offer <- do.call("rbind", json_file)##it is convrting into data frame
Customer_Offer <- as.data.frame(Customer_Offer)
View(Customer_Offer)
CustOffer <-  select(Customer_Offer,attributes.Merchants,attributes.Locations)
CustOfferResult <- filter(CustOffer, nchar( as.character( attributes.Merchants)) == 5 &  nchar( as.character( attributes.Locations)) <= 3 )
CustOfferResult <- rename(CustOfferResult, OfferID = attributes.Merchants , CustomerID = attributes.Locations )
write.csv(CustOfferResult, "cust1.csv", na ="0")



