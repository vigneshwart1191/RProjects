my
myDataFrame
myDataFrame[,c(1,2)]
h=c("a","b","c","d")

##QUE NO2
myDataFrame
myMatrices=matrix(1:12,3,4)
myMatrices
dimnames(myMatrices)=list(letters[1:3],letters[1:4])
myMatrices
myMatrices2=matrix(13:24,3,4)
myMatrices2
dimnames(myMatrices2)=list(letters[4:6],letters[5:8])
myMatrices2
mat1andmat2=cbind(myMatrices,myMatrices2)
mat1andmat2
mat1andmat22=rbind(myMatrices,myMatrices2)
mat1andmat22
#xxx=rbind(mat1andmat2,mat1andmat22)

##QUE 3
x=(1:10)
names(x)<- letters[x]
x
x[1:3]
x[c(1,10)]
x[c(-1,-2)]
x[x>5]
x[c("a","d")]
x[]
jj1=matrix(1:100,ncol=10)
jj1
jj1[1:5,]


##QUe 4
x.lis=list(a=1:10,b=letters[1:3],b=matrix(1:10,ncol=2))
x.lis
x.lis$a
x.lis[[2]]
x.lis[[3]][,1]
y=matrix(1:10,ncol=2)
y
y+1
y+y
y%*%t(y)
t(y)
