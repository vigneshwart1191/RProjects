#Creating sample vectors
sampVec1 = c(1,2,3,4,5)
sampVec1
sampVec2=c("Ab","BC","CA","ED")
sampVec2
sampVec3=c(1000,2000,3000,4000)
sampVec3
#creating List= List is a collection of values or vectors which will taken into count one after the other
#can have different data type and size of vector values can also differ
sampList = list(sampVec1,sampVec2,sampVec3)
sampList
#Creating Matrix=Matrix is a 2D values and can only have same data type
sampMat=matrix(sampVec3,nrow=2,byrow=T)
sampMat
#Creating Arrayy=Array is similar to matrix but we can have multi dimensions and data type should be same
sampArray=array(sampVec3,dim=c(2,2,4))
sampArray
#Factors=It will label what are the values in vector 
sampFact=factor(c(1,1,1,3,4,5,4,34,54,6,5,2,5,4))
sampFact
#DataFrame can have different data types but the number of values in vectors should be same.
#sampData=data.frame(sampVec1,sampVec2,sampVec3)
#through an errror because vector value is different
sampData=data.frame(c(1,2,3,4),sampVec2,sampVec3)
sampData