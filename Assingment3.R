#Print Numbrs using repeat
x=0
repeat
{
  x=x+1
  print(x)
  if(x == 5)
  {
    break
  }
  }
#check a number is positive or negative
a = 13
if(a %% 2 ==0)
{
  print("Number is even")
}else{
  print("Number is odd")
}

#print numbers using while loop
i=1
while(i <= 10)
{
  print(i)
  i=i+1
}

#create a vecot and find the sum of the vector using for loop
a1=c(1,2,3,4,5,6,7,8,9)
s=0
for(i in 1:length(a1))
{
  s= s+a1[i]
}
print(s)

#create a vecot and find the sum of the vector using while loop
a1=c(1,2,3,4,5,6,7,8,9) 
s=0
i = 1
#for(i in 1:length(a1))
while(i <= length(a1))
{
  s= s+a1[i]
  i=i+1
}
print(s)

#count number of even elements in the vector
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
  print(length(even))
}
oddEven(a)

#Program to find factors using for loop
sampFact = function(f)
{
  for (i in 1:f)
  {
    if (f %%i == 0)
    {
      print(i)
    }
  }
}
sampFact(16)

#Prime Number or not
m=0
flag = 0
n=17
m = n/2
#for(i in 2:m)
i=2
while(i<=m)
{
  if(n%%i == 0){
    print("Number is not prime")
    flag = 1
    break
  }
  i=i+1
}
if(flag == 0)
{
  print("Number is prime")
}

#Factorial using while loop
fact=1
sampleFact = function(x)
{
  #for (i in 1:x)
  i=1
  while(i<=x)
  {
    fact = fact*i
    i=i+1
  }
  print(fact)
}
sampleFact(5)