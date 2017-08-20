#1)
a1 = -98:100
a2 = sample(a1,100)
a2
m1 = matrix(a2,nrow = 10,ncol = 10)
m1
p1 = apply(m1,1,prod)
p1
s1 = apply(m1,2,sum)
s1
m2 = m1%%10
m2

#2
v = c(60,49,40,61,64,60,59,54,62,69,70,42,56,61,61,61,58,51,48,65,49,49,41,48,52,46,59,46,58,43)
d1 = data.frame(v)
d1
mean1 = apply(d1,1,mean)
mean1

#3
m3 = matrix(1:30,nrow = 5,ncol = 6)
m3
s = apply(m3,2,sum)
s

#4
x = c(151,174,138,186,128,136,179,163,152,131)
y = c(63,81,56,91,47,57,76,72,62,48)
lm(y~x)
y = 0.6746*x + -38.4551
y
plot(x,y,"l")
plot(x,y,main = "X vS Y",xlab = "x axis",ylab = "y axis",col = "red")
cor(x,y)
summary(x,y)
#5
call = c(23,28,39,48,64,75,88,96,97,109,118,149,150,156,165)
profit = c(1,2,3,3,4,4,5,6,6,7,8,8,9,10,10)
lm(profit~call)  #you can use abline(lm(profit~call)) and then no need of y = mx+c
plot(call,profit)
profit = 0.05988*call + 0.12496
points(call,profit,"l")
call_5 = (5-0.12496)/0.05988
call_5
###
#6
insurance <- read.csv("~/insurance.csv")
insurance
#model generation
fit = lm(insurance$expenses ~ insurance$age+insurance$bmi+insurance$smoker+insurance$children)
fit
summary(fit)
plot(fit)
#scatter.smooth(fit)
#fit1 = lm(expenses ~ age+bmi+smoker+children)
#fit1
#scatter.smooth(fit1)
#error = predict(fit,test~test["mpg"]
#sqrt
