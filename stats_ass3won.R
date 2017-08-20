#1
x1 = 75
n1 = 1500
x2 = 80
n2 = 2000
p1 = x1/n1
p2 = x2/n2
alpha = 0.90
error = (qnorm(.95)*sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2)))
l_limit  = (p1-p2)-error
u_limit = (p1-p2)+error
#2
n1=32
n2=30
m1=85
m2=81
s1=5
s2=4
alpha = 0.95/2
#different variance
cal = (qnorm(.025)*sqrt((s1^2/n1)+(s2^2/n2)))
low = (m1-m2) + cal
high = (m1-m2) - cal
#same variance
sspool = (((n1-1)*s1^2)+((n2-1)*s2^2))/(n1+n2-2)
spool = sqrt(sspool)
error=(qnorm(.975)*spol*sqrt((1/n1)+(1/n2)))
low = (m1-m2) + error
high = (m1-m2) - error
#3
m1=85
m2=81
n1=12
n2=10
#t1 = qt(.025,20)
t2 = qt(.975,20) #pooling tdistribution value n1+n2-2
sspool = (((n1-1)*s1^2)+((n2-1)*s2^2))/(n1+n2-2)
spool = sqrt(sspool)
error=(t2*spool*sqrt((1/n1)+(1/n2)))
low = (m1-m2)-error
upp = (m1-m2)+error

#2
n1=15
n2=15
m1=1.20
m2=1.35
s1=.45
s2=.54
alpha = 0.95/2
dof = (((s1^2/n1)+(s2^2/n2))^2)/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
error = (qt(.975,dof)*sqrt((s1^2/n1)+(s2^2/n2)))
low = (m1-m2) + error
high = (m1-m2) - error

#3
n1=50
n2=50
m1=78.3
m2=87.2
s1=5.6
s2=6.3
aplha=.975
sspool = (((n1-1)*s1^2)+((n2-1)*s2^2))/(n1+n2-2)
spool = sqrt(sspool)
error = qnorm(.975)*spool*sqrt((1/n1)+(1/n2))
l_limit = (m1-m2)-error
u_limit = (m1-m2)+error

#4
v1 = c(20,31,18,23,23,28,23,26,27,26,12,17,25)
v2 = c(19,30,32,28,15,26,35,18,25,27,35,35)
n1=length(v1)
n2=length(v2)
m1=mean(v1)
m2=mean(v2)
s1=sd(v1)
s2=sd(v2)
alpha=0.975
#pooled one
sspool = (((n1-1)*s1^2)+((n2-1)*s2^2))/(n1+n2-2)
spool = sqrt(sspool)
#dof = (((s1^2/n1)+(s2^2/n2))^2)/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
error = qt(alpha,23)*spool*sqrt((1/n1)+(1/n2))
l_limit = (m1-m2)-error
u_limit = (m1-m2)+error
#unpooled
error = qt(alpha,dof)*sqrt((s1^2/n1)+(s2^2/n2))
l_limit = (m1-m2)-error
u_limit = (m1-m2)+error

                  
#5
v1 = c(.98,.83,.99,.86,.90,.81,.94,.92,.87)
v2 = c(1.20,1.18,1.33,1.21,1.20,1.07,1.13,1.12)
n1=length(v1)
n2=length(v2)
m1=mean(v1)
m2=mean(v2)
s1=sd(v1)
s2=sd(v2)
alpha=0.975
#pooled one
sspool = (((n1-1)*s1^2)+((n2-1)*s2^2))/(n1+n2-2)
spool = sqrt(sspool)
#dof = (((s1^2/n1)+(s2^2/n2))^2)/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
error = qt(alpha,15)*spool*sqrt((1/n1)+(1/n2))
l_limit = (m1-m2)-error
u_limit = (m1-m2)+error
#unpooled
error = qt(alpha,dof)*sqrt((s1^2/n1)+(s2^2/n2))
l_limit = (m1-m2)-error
u_limit = (m1-m2)+error

#6
n1=200
n2=100
m1=10.9
m2=10.5
s1=2.0
s2=3.0
aplha=.975
sspool = (((n1-1)*s1^2)+((n2-1)*s2^2))/(n1+n2-2)
spool = sqrt(sspool)
error = qnorm(.975)*spool*sqrt((1/n1)+(1/n2))
l_limit = (m1-m2)-error
u_limit = (m1-m2)+error

#7
v1 = c(45,32,58,59,60,44,47,51,42,38)
v2 = c(47,34,60,57,63,38,49,53,46,41)
n1=length(v1)
n2=length(v2)
m1=mean(v1)
m2=mean(v2)
s1=sd(v1)
s2=sd(v2)
dof = (((s1^2/n1)+(s2^2/n2))^2)/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
error = (qt(.995,dof)*sqrt((s1^2/n1)+(s2^2/n2)))
u_limit = (m1-m2) + error
l_limit = (m1-m2) - error

#8
v1=c(65,67,67,76,70,69,62)
v2=c(66,60,64,68,65,66,60)
n1=length(v1)
n2=length(v2)
m1=mean(v1)
m2=mean(v2)
s1=sd(v1)
s2=sd(v2)
alpha = 0.95/2
dof = (((s1^2/n1)+(s2^2/n2))^2)/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
error = (qt(.975,dof)*sqrt((s1^2/n1)+(s2^2/n2)))
u_limit = (m1-m2) + error
l_limit = (m1-m2) - error

#9
v1=c(2750,2360,2950,2830,2250)
v2=c(2850,2380,2930,2860,2320)
n1=length(v1)
n2=length(v2)
m1=mean(v1)
m2=mean(v2)
s1=sd(v1)
s2=sd(v2)
alpha = 0.95/2
dof = (((s1^2/n1)+(s2^2/n2))^2)/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
error = (qt(.975,dof)*sqrt((s1^2/n1)+(s2^2/n2)))
u_limit = (m1-m2) + error
l_limit = (m1-m2) - error
#10
x1 = 250
n1 = 1000
x2 = 275
n2 = 1000
p1 = x1/n1
p2 = x2/n2
alpha = 0.90
error = (qnorm(.975)*sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2)))
l_limit  = (p1-p2)-error
u_limit = (p1-p2)+error

#11
n1=16
n2=11
m1=4.653
m2=4.274
s1=0.012
s2=0.02
r1=n1-1
r2=n2-1
error1 = qf(0.01,120,60)
error2 = qf(0.99,r1,r2)
l_limit = (s2^2/s1^2)*error1
u_limit = (s2^2/s1^2)*error2

#11
n1=10
n2=11
m1=82
m2=78
s1=8
s2=7
r1=n1-1
r2=n2-1
error1 = qf(0.05,r1,r2)
error2 = qf(0.95,r1,r2)
l_limit = (s2^2/s1^2)*(error1)
u_limit = (s2^2/s1^2)*error2

#12
error=0.03
ci=0.99
pcap=0.69
n=(pcap*(1-pcap)*qnorm((1-ci)/2))/(error^2)


