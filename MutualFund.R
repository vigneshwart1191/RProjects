#intially get the data from the webpage
start_date='01-jan-2016'
paste(start_date)->x
End_date='31-jan-2016'
paste(End_date)->y

url=paste('http://portal.amfiindia.com/DownloadNAVHistoryReport_Po.aspx?mf=53&frmdt=',paste(x),'&todt=',paste(y))
url
data1=read.csv2(url)
View(data1)


#Explore the data with help of ggplot
library(ggplot2)
#plotting ggplot
graph = ggplot(data,aes(Date,Net.Asset.Value))+geom_dotplot()+ xlab("Date") + ylab("Net Value")
graph1 = ggplot()+geom_line(aes(x = (data$Date),y =data$Net.Asset.Value),color = 'red')

##
plans = function(x)
{
  NAVdata6 = subset(data,data$Scheme.Name == x)
  
  return(NAVdata6)
}

b5 = plans("Axis Income Fund - Quarterly Dividend Option")
View(b5)
a1 = subset(data,data$Scheme.Name == "Axis Income Fund - Quarterly Dividend Option")
a1$Net.Asset.Value = as.numeric(a1$Net.Asset.Value)
a1[length(a1),]$Net.Asset.Value-a1[1,]$Net.Asset.Value
min(a1$Net.Asset.Value)-max(a1$Net.Asset.Value)
##gain function
plans = function(x)
{
  NAV = subset(data1,data1$Scheme.Code == x)
  NAV$Net.Asset.Value = as.numeric(NAV$Net.Asset.Value)
  gain = NAV[length(NAV),]$Net.Asset.Value-NAV[1,]$Net.Asset.Value
  return(gain)
}
#do scheme.code till 248
#get unique data for entire data set
b1 = levels(factor(data1$Scheme.Code))
b1
length(b1)
class(b1)
str(b1)

#apply c1 on function plans
v1= c()
for(i in 1:247)
{
v1 = c(v1,plans(b1[i]))
}
#different scheme code
b1 = levels(factor(data1$Scheme.Code))
b1
b1[-249:-259]

#bind v1 v2 and and induvual code
a1 = cbind(b1,v1)
View(a1)
write.csv(a1,'C:/DataScience/Analytics vidya/rank.csv')
a1 = as.data.frame(a1)
class(a1)
odr = a1[with(a1,order(v1)),]
xx4[with(xx4,order(xx2)),]
order = a1[,sort(v1,decreasing = T)]
#order the data
library(plyr)
rank = arrange(a1,desc(v1))
View(rank)
#order function and ranked according to SchemaID
rank <- read.csv("C:/DataScience/Analytics vidya/rank.csv")
View(rank)

