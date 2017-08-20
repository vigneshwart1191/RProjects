#intially get the data from the webpage
start_date='01-jan-2016'
paste(start_date)->x
End_date=Sys.Date()
paste(End_date)->y

url=paste('http://portal.amfiindia.com/DownloadNAVHistoryReport_Po.aspx?mf=53&frmdt=',paste(x),'&todt=',paste(y))
url
data1=read.csv2(url)

View(data1)


#Explore the data with help of ggplot
library(ggplot2)
#plotting ggplot
graph = ggplot(data,aes(Date,Net.Asset.Value))+geom_dotplot()+ xlab("Date") + ylab("Net Value")
graph1 = ggplot()+geom_line(aes(x = (data$Date),y =data$Net.Asset.Value),col=data$Netscheme)


##Calculate gain so function has been written to calculate gain for all data
###based on first and last date

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
#bind v1 v2 and and induvual code
a1 = cbind(b1,v1)
View(a1)
#sort the gain in a decending order
s1 = a1[order(v1,decreasing = TRUE),]
View(s1)
nrow(s1)

rank = c(1:247)
#bind ranking with entire data
df = cbind(s1,rank)
View(df)


###weighted average ranking
averageFunction = function(x)
{
  NAV = subset(data1,data1$Scheme.Code == x)
  NAV$Net.Asset.Value = as.numeric(NAV$Net.Asset.Value)
  gain1 = mean(NAV$Net.Asset.Value)
  return(gain1)
}

#do scheme.code till 248
#get unique data for entire data set
b1 = levels(factor(data1$Scheme.Code))
b1
b2 = levels(factor(data1$Scheme.Name))
b2
length(b1)
class(b1)
str(b1)

#apply c1 on function plans
g2= c()
for(i in 1:247)
{
  g2 = c(g2,averageFunction(b1[i]))
}

#bind v1 v2 and and induvual code
gainCal = cbind(b1,b2,g2)
View(gainCal)
#sort the gain in a decending order
rankData = gainCal[order(g2,decreasing = TRUE),]
View(rankData)
nrow(rankData)
nrow(s1)

rank = c(1:247)
#bind ranking with entire data
df1 = cbind(rankData,rank)
View(df1)


NAV = subset(data1,data1$Scheme.Code == 117447)
NAV$Net.Asset.Value = as.numeric(NAV$Net.Asset.Value)
gain = NAV[nrow(NAV),]$Net.Asset.Value-NAV[1,]$Net.Asset.Value
return(gain)

