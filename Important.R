#sorting
v1 = c(32,21,3,2,43,54,1,6)
temp1=0
for(i in 1:length(v1))
{
  for(j in 2:length(v1)-1)
  {
    if(v1[i]<v1[j])
    {
      temp1=v1[j]
      v1[j]=v1[i]
      v1[i]=temp1
    }
  }
}
#mean
sum=0
for(i in 1:length(v1))
{
  sum=sum+v1[i]
}
print(sum)
#meam
mean1 = sum/length(v1)

#sd
sum1=0
sd1=c()
for(i in 1:length(v1))
{
  s1=(v1[i]-(mean1))*(v1[i]-(mean1))
  sd1=c(sd1,s1)
}
sd1 

s2=0
for(i in 1:length(sd1))
{
  s2=s2+sd1[i]
}
print(s2)


#
sd =(s2/length(v1))^0.5
sd(v1)
print(v1)
length(v1)/2
7%/%2


#
#sd
sum1=0
sd1=0
for(i in 1:length(v1))
{
  #1=v1[i]-(mean1)
  sd1=sd1+v1[i]-(mean1)
}
sd1 

#reverse a number
sum=0
n=432
while(n>0){    
  r=n%%10  
  sum=(sum*10)+r   
  n=n%/%10    
}    
print(sum)
