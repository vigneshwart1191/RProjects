######Problem Statement
######Find the right segmentation for the customers
######Business problem:This problem is that to form cluster based on the customers pattern

######1.Import the datset 
library(readr)
Demo <- read_csv("C:/Users/vignesht/Downloads/Demo.csv")
#View(Demo)

######2.Explore the data 
str(Demo) #Three continous data
summary(Demo)
anyNA(Demo) #Some NA values are there in this data
which(anyNA(Demo)) #We have NA values in Customer Unique ID and no need to treat this Na because all values are unique
#Draw a histogram of the data and check how data is distributed
hist(Demo$BillAmount)
hist(Demo$NoofProducts)

######3.Data Preprocessing
#CustomerID and Transaction Id are unique values so we can remove both fetures
#THis won't help us to form segmentation
DemoActual=Demo[,3:5]
#View(DemoActual)

#3.1.Normalising the data
#While doing clustering and segmentation normalizing the data is very important
#sacle the entire data to do normalization
scaling = function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

#apply scaling for entire data
DemoActual = apply(DemoActual,2,scaling)

#convert matrix to data frame
DemoActual = as.data.frame(DemoActual)
#View(DemoActual)

#Find number of rows to fix range of k values in for loop
a = nrow(DemoActual)

#apply kmeans with multiple kvales to find optimised k value
#will be using elbow method to find optmised k value
wtss = c()
btss = c()
for(i in 2:10)
{
  fit = kmeans(DemoActual,i,nstart = 20)
  wtss = c(wtss,fit$tot.withinss)
  btss = c(btss,fit$betweenss)
}
plot(wtss)
plot(btss)

#After plotting Inter and Intra values of multiple k-values 5 segmentation will be good for this data
#Its a trail and error method k value should be choosed based on business knowledge

#Aplly 5 in k 
fit1=kmeans(DemoActual,5,nstart=20)

DemoActual$cluster = fit$cluster
View(DemoActual$cluster)
View(DemoActual)
summary(fit1)
fit1$withinss
fit1$betweenss
fit1$tot.withinss
#propotion between inter and intra on 5 clustering is good
library(fpc)
library(cluster)
#plot the clustering data
plotcluster(DemoActual,fit1$cluster)



