#read the data
snsdata <- read.csv("C:/Users/vignesht/Downloads/snsdata.csv")
View(snsdata)
str(snsdata$age)
#explore the data
str(snsdata)
summary(snsdata)
anyNA(snsdata)

#preprocessing the data
#partion the data based onthe grad year
factor(snsdata$gradyear)
year1 = subset(snsdata,gradyear == 2006)
nrow(year1)
year2 = subset(snsdata,gradyear == 2007)
nrow(year2)
year3 = subset(snsdata,gradyear == 2008)
nrow(year3)
year4 = subset(snsdata,gradyear == 2009)
nrow(year4)


#replace gender Na with Female
year1[is.na(year1[,2]), 2] <- "F"
year2[is.na(year2[,2]), 2] <- "F"
anyNA(year2$gender)
year3[is.na(year3[,2]), 2] <- "F"
year4[is.na(year4[,2]), 2] <- "F"
anyNA(year1$gender)
#structure of years datamean(bigMart[,2], na.rm = TRUE)
year1[is.na(year1[,3]), 3] <- mean(year1[,3],na.rm = T)
year2[is.na(year2[,3]), 3] <- mean(year2[,3],na.rm = T)
year3[is.na(year3[,3]), 3] <- mean(year3[,3],na.rm = T)
year4[is.na(year4[,3]), 3] <- mean(year4[,3],na.rm = T)
anyNA(year2$age)
#removing outliers in age based on years
summary(year1$age)
range(year1$age)
range(year2$age)
range(year3$age)
range(year4$age)
mean(year1$age)
#remove outliers based on year
s1 = summary(year1$age)
s2 = summary(year2$age)
s3 = summary(year3$age)
s4 = summary(year4$age)
#outliers code
outliers = function(x)
{
  y<-summary(x$age)
  for(i in 1:nrow(x))
  {
    if((x[i,3] < y[2]) | (x[i,3] > y[5]))
    {
      x[i,3] = y[4]
    }
  }
  return(x)
}
#apply outliers for all data set
year1 = outliers(year1)
year2 = outliers(year2)
year3 = outliers(year3)
year4 = outliers(year4)

#bind the data together
SocData = rbind(year1,year2,year3,year4)
#convert factor female data into numeric
SocData[,2] = as.numeric(SocData$gender)

#sacle the entire data to do normalization
scaling = function(x)
{
  return((x-min(x))/(max(x)-min(x)))
#  x = x-min(x)/(max(x)-min(x))
 # return(x)
}
#apply scaling for entire data
SocData = apply(SocData,2,scaling)
#convert matrix to data frame
SocData = as.data.frame(SocData)
#check sructure of data
str(SocData)
summary(SocData)
anyNA(SocData)
View(SocData)
#apply kmeans in social of all data
fit1 = kmeans(SocData,5,10,20)
fit1
summary(fit1)
fit1$withinss
fit1$betweenss
fit1$tot.withinss
library(fpc)
library(cluster)
#plot the clustering data
#plot(SocData,col = (fit1$cluster+1))
plotcluster(SocData,fit1$cluster)
