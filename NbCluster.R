#importing the data
spamread <- read.csv("C:/Users/vignesht/Downloads/spam.csv", header=T)
View(spamread)

#preproccesing the data
#Scaling function 
scaling = function(y)
{
  y = (y-min(y))/(max(y)-min(y))
}

##apply scaling for data set
scalespam = apply(spamread,2,scaling)
scalespam
View(scalespam)
scalespam1 = as.data.frame(scalespam)

#remove class label
spam2 = scalespam1[,-58]
class(spam2)
str(spam2)
library(NbClust)
#Nbclust
fis = NbClust(spam2,diss = NULL,"euclidean",2,7,"kmeans","all")
summary(fis)
fis$All.CriticalValues
#explore the Nbclust
fis$All.index
fis$Best.nc
fis$Best.partition

a1 = fis$Best.partition
table(spamread$V58,a1)


#explore with kmeans algorithm
wtss = c()
btss = c()
for(i in 2:10)
{
  spamcluster = kmeans(spam2,i,nstart = 20)
  wtss = c(wtss,spamcluster$tot.withinss)
  btss = c(btss,spamcluster$betweenss)
}
wtss
btss
plot(wtss)
plot(btss)

sclus1 = kmeans(spam2,3,nstart = 20)
a2 = sclus1$cluster
a3 = as.data.frame(a2)
table(spamread$V58,a2)
library(amap)
#amap method
wtss1 = c()
for(i in 2:10){
  fis4 = Kmeans(spam2,i,iter.max = 100,nstart=1,method="euclidean")

for(j in 1:i)
{
  wt = (mean(sum((spam2[fis4$cluster == j,]- fis4$centers[j,])^2)))
}
    wtss1 = c(wtss1,sum(wt))
}
wtss1
plot(wtss1)
fis4
fis4$cluster

#calculate wtss
#calculate sum
summ = c()
for(i in 1:ncol(spam2))
{
  summ = c(summ,(sum(spam2[,i])))
}
summ
#calculate center using mean
centers = c()
for(j in 1:ncol(spam2))
{
  centers = c(centers,(mean(spam2[,i])))
}
centers
#calculate Wtss
wtss = c()
for(i in 1:57)
{
  wtss = c(wtss,(summ[i]-centers[i]))
}
wtss
