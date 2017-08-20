sampList2 = list(c(1:5) , c("Green","Blue","Red") , matrix(c(1:9),nrow=3) ,list(1,2,3))
#sampList2 = names(c("Numbers","Colours,Matrices","InnerList"))
names(sampList2) = c("Numbers","Colours","Matrices","InnerList")
print(sampList2[3])
print(sampList2$InnerList)
sampList2[] = "new Inserted List"
sampList2
row=c("Row1","Row2","Row3")
column =c("C1","C2","C3")
mat = c("Mat1","Mat2")
gender = c(rep("Male",20),rep("Female",30))
gender = factor(gender,levels=c("Male","Female"))
gender
#simple IF statment example and else and curly brases should be in same line
age=34
if(age>18)
{
  print("Elegible to vote")
}else{
  print("Not Elegible to vote")
}

x = 1:15
if(sample(x,1,replace=TRUE) <= 10)
{
  print("X is greater than 10")
  }else{
    print("X is not greater than 10")

     }
j=c(1,54,654,6545)
for(i in 1:4)
{
  print(j[i])
        
}
cd = seq(1,10,2)
cd
sampMat=matrix(c(1:9),nrow=3,ncol=3,byrow=T)
for(i in 1:3)
{
  for(j in 1:3)
  {
    print(sam[i][j])
  }
  print("Space")
}

