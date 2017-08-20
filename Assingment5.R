#1.Create a matrix of given values
a1 = c(15,5,5,0,0,5,6,1,3,0,3,3,4,4,4,2)
mat1 = matrix(a1,ncol=4,nrow=4,byrow = T)
mat1

#2.Diagonal values of matrices
a2 = c()
for(i in 1:4)
{
  for(j in 1:4)
  {
    if(i == j)
    {
      a2 = c(a2,mat1[i,j])
    }
    }
}
a2

#3.Create a matrix containg first two rows
head(mat1,2)

#4.Create data subset of Iris contains only verisColor
a3 =iris
subset(a3,Species == "versicolor",select = c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species))

#5.Sort iris according to Sepal.length
l = order(iris$Sepal.Length,decreasing = T)
l
iris[l,]

#6.Benchmark statistics of the Three Varaibles of ethanol data set
library(SemiPar)
#attach(ethanol)
data(ethanol)
pairs(ethanol)

c1 = c(ethanol$NOx)
c2 = c(ethanol$C)
c3 = c(ethanol$E)

summary(c1)
hist(c1)
summary(c2)
hist(c2)
summary(c3)
hist(c3)

#7.Calculate the quartiles of three variables,using apply and quartile function
quantile(c1)
quantile(c2)
quantile(c3)

#8.Create a matrix which values are lesser than 6
mat2 = c()
for(i in 1:4)
{
  for(j in 1:4)
  {
    if(mat1[i,j] < 6)
    {
      mat2 = c(mat2,mat1[i,j])
    }
  }
}
mat2
mat3= matrix(mat2,nrow=4,ncol=4)
mat3


#create values which are less than 6
mat1[c(-1,-2),]

#9Create a matrix for rows which dont have zero
mat4= c()
for(i in 1:4)
{
  for(j in 1:4)
  {
    if(mat1[i,j] >= 1)
    {
      mat4= c(mat4,mat1[i,j])
    }
  }
}
mat4
mat5= matrix(mat4,nrow=4,ncol = 4)
mat5
#Create a matrix which rows dont have zero
mat1[,3]

#10.Dataset1
dataset1 <- cbind(observationA = 16:8, observationB = c(20:19, 6:12))
dataset1 

#11.using Apply function find row means
apply(dataset1,1,mean)

#12	Using apply(), find the column sums of dataset1
apply(dataset1,2,mean)

#13	Use apply() to sort the columns of dataset1
apply(dataset1,2,sort)

#14	Find the length of the dataset1 columns using apply
apply(dataset1,2,length)

#15 Create a List
list1 <- list(observationA = c(1:5, 7:3), observationB=matrix(1:6, nrow=2))
list1

#16.	Using lapply(), find the length of list1‘s observations
lapply(list1,length)

#17.	Using lapply(), find the sums of list1‘s observations
lapply(list1,sum)

#18. Observe the output
X <- c (22,3,7,NA,NA,67)
length(X)

#19.Choose the Correct Code
X1 = c(NA,3,14,NA,33,17,NA,41) 
X1[!is.na(X1)]

#20.Choose the Correct Code
Y = c(1,3,12,NA,33,7,NA,21) 
Y[is.na(Y)]= 11
Y

#21.Choose the Correct Code
X2 = c(34,33,65,37,89,NA,43,NA,11,NA,23,NA) 
sum(is.na(X))

#22.Write the code for missing values
c1 <- c(1,2,3,NA) 
c2 <- c(2,4,6,89) 
c3 <- c(45,NA,66,101) 
X <- rbind (c1,c2,c3, deparse.level=1) 
X
#23.Observe the output
(x4 = scan("http://www.ats.ucla.edu/stat/data/scan.txt", what = list(age = 0,name = "")))

#24.Observe the output
 
#<- scan("http://www.ats.ucla.edu/stat/data/scan.txt", what = list(NULL, name = character()))

c=list(a1 = c(1,2,3,4),b1= c(5,6,7,8))
c$a1
a2 = matrix(c(1:12),3,4)
a3 = c(1:4)
a4 = c(5:8)
a5 = c(9:12)
a6=data.frame(a3,a4,a5)
a6 = dimnames(c("A","B","C"))
            