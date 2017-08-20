
#read the data from local system
cpu <- read.csv("C:/Users/vignesht/Downloads/data set.csv", header=FALSE)
View(cpu)
#Explore the data
summary(cpu)
boxplot(cpu$V3)
boxplot(cpu$V5)
boxplot(cpu$V10)
hist(cpu$V8)
#Exploring corelation between predictors and response variable
cor(cpu[,c(3:10)])
#cor(cpu,use = "everything")
#scale the data
#scaling = function(y)
#{
#  y = (y-min(y))/(max(y)-min(y))
#}
#scale using log function
scaling = function(y)
{
  y=log(y)
}
#apply scaling for entire data set
scalcpu=apply(cpu[,c(3:10)],2,scaling)
View(scalcpu)
#conversion of matrix to data frame
scalcpu=as.data.frame(scalcpu)

#remove infinity
scalcpu$V6[is.infinite(scalcpu$V6)] = 0 #mean(scalcpu$V6)
scalcpu$V7[is.infinite(scalcpu$V7)] = 0 #mean(scalcpu$V7)
scalcpu$V8[is.infinite(scalcpu$V8)] = 0 #mean(scalcpu$V8)
#removing outliers for scaled data

for(i in 1:ncol(scalcpu[,c(3:8)]))
{
  s = summary(scalcpu[,i])
  ub=s[5]+1.5*IQR(s)
  lb=s[2]-1.5*IQR(s)
  for(j in 1:nrow(scalcpu[,c(3:8)]))
  {
    if((scalcpu[j,i] > ub || scalcpu[j,i] < lb))
    {
      scalcpu[j,i]=mean(scalcpu[,i])
    }
  }
}
#conversion of matrix to data frame
scalcpu=as.data.frame(scalcpu)


#Checking outliers after elimation of outliers
boxplot(scalcpu)

#checking correlation between features
cor(scalcpu,use = "everything")
#there is no features which is higly corelated so need to use entire data

#partioning train and test data
ind=sample((1:nrow(scalcpu)),round(0.70*nrow(scalcpu)))
train=scalcpu[ind,]
train
test=scalcpu[-ind,]
test


#model generation
##1 for orginal data set without scaling
fit = lm(V10 ~ V3+V4+V5+V6+V7+V8,cpu)
summary(fit)
plot(fit)

##1 for scaled data set
fit1=lm(V10 ~ V3+V4+V5+V6+V7+V8,data = train)
fit1
summary(fit1)
plot(fit1)

##2 remove v3 which is less corelated
fit2=lm(V10 ~ V3+V4+V5+V6+V8,data = train)
fit2
summary(fit2)
plot(fit2)
p = predict(fit2,test)
p

