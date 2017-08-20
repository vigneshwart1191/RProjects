#import the data set
glass <- read.csv("C:/Users/vignesht/Downloads/glass.csv")
View(glass)
#preprocess the data
str(glass)
glass1 = glass[1:9]
str(glass1)
summary(glass1)
#scale the data
scaling = function(y)
{
  y = (y-min(y))/(max(y)-min(y))
}
#apply scaling for entire data set
glass2 = apply(glass1,2,scaling)
glass2=as.data.frame(glass2)
summary(glass2)
#apply kmeans
wtss = c()
btss = c()
for(i in 2:10)
{
  glasscluster = kmeans(glass2,i,nstart = 20)
  wtss = c(wtss,glasscluster$tot.withinss)
  btss = c(btss,glasscluster$betweenss)
}
plot(wtss)
plot(btss)

#kmeans
glassclu  = kmeans(glass2[,1:9],13,nstart = 20)
glassclu
table(glassclu$cluster,glass$Type)
glassclu$cluster = as.factor(glassclu$cluster)
library(ggplot2)
ggplot(glass2,aes(RI,Na,Mg,Al,SI,K,Ca,Ba,Fe,color = glassclu$cluster))+geom_point()
