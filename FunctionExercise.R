a= c(1,2,3,4,5,6,7,8,9)
oddEven = function(a)
{
  for(i in 1:9)
  {
  if(a[i] %% 2 ==0)
  {
    print("Even Number")
  }else{
    print("Odd Number")
  }}
}
oddEven(a)

#printing odd and even values in vectors
a= c(1,2,3,4,5,6,7,8,9)
oddEven = function(a)
{
  odd = c()
  even = c()
  for(i in 1:length(a))
  {
    if(a[i] %% 2 ==0)
    {
      even = c(even,a[i])
    }else{
      odd = c(odd,a[i])
    }}
  mylist = list(odd,even)
  print(mylist)
}
oddEven(a)

#summation of two vectors using for loop
fis=c(1,2,3,4,5,6,7,8,9)
sec=c(1,2,3,4,5,6,7,8,9)
addition = function(fis,sec)
{
  thir = c()
  for(i in 1:length(fis))
  {
    thir = c(fis[i]+sec[i])
    print(thir)
  }
}
addition(fis,sec)

#Summation of two matrices using for loop
mat1 = matrix(fis,nrow=3)
mat2 = matrix(fis,nrow=3)
matAddition = function(mat1,mat2)
{
  mat3 = matrix(0,nrow(mat1),ncol(mat1))
  for(i in 1:nrow(mat1))
  {
    for(j in 1:ncol(mat2))
    {
      mat3[i,j] = mat1[i,j]+mat2[i,j]
      #print(mat3[i,j])
    }
  }
  print(mat3)  
}
matAddition(mat1,mat2)

#simple matrix addition
m1= matrix(c(1:9),nrow = 3)
m2= matrix(c(1:9),nrow =3)
m3 = m1+m2
m3

