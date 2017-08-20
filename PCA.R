#import the data
library(MASS)
data(Boston,package = 'MASS')
mydata = Boston
View(mydata)

#Check the summary
summary(mydata)

#Check correlation between data
cor(mydata)


#principle compnent analysis
pcadata = prcomp(mydata,scores = T,cor = T)
summary(pcadata)



#check loadings
loadings(pcadata)

#screenshot of eigen values which will give stdeviation value
screeplot(pcadata,type = 'line',main = 'screenplot')

#biplot
biplot(pcadata)

#scores of the planet
pcadata$scores[1:10,]
