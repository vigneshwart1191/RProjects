#3.23 Catalyst Example
c1_n1=12
c2_n2=10
c1_mu1=85
c2_mu2=81
c1_s1=4
c2_s2=5
#due to Variance is equal so we will use Spooled Variance Formula
c1_sp=(((c1_n1-1)*(c1_s1^2))+((c2_n2-1)*(c2_s2^2)))/(c1_n1+c2_n2-2)
c1_sq<-round(x = sqrt(c1_sp),digits = 2)
#Cofidence Interval of 90%
c1_a=1-0.9
al<-c1_a/2
ar<-0.9+al
c1_l<-(-(round(x = qt(p = al,df = 20),digits = 2)))
c_cil<-round(x = (c1_mu1-c2_mu2)-(c1_l*c1_sq*sqrt(((1/c1_n1)+(1/c2_n2)))),digits = 2)
c_cir<-round(x = (c1_mu1-c2_mu2)+(c1_l*c1_sq*sqrt(((1/c1_n1)+(1/c2_n2)))),digits = 2)
cat(c_cil,'< mu1-mu2 <',c_cir)
#3.24 
r_mu1=1.20
r_mu2=1.35
r_s1=0.45
r_s2=0.54
r_a=1-0.95  #Confidence Interval of 95%
r_n12=15
#Calculating Degree of Freedom 
r_u=(((r_s1)^2/r_n12)+((r_s2)^2/r_n12))^2
r_dl=(((r_s1)^2/r_n12)^2)/(r_n12-1)
r_dr=(((r_s2)^2/r_n12)^2)/(r_n12-1)
r_d=r_dl+r_dr
r_dof=ceiling(x = r_u/r_d)
r_al=r_a/2
r_tl<-(-(round(x=qt(p = r_al,df = r_dof),digits = 2)))
r_l<-round(x = (r_mu1-r_mu2)-(r_tl*sqrt(((r_s1)^2/r_n12)+((r_s2)^2/r_n12))),digits = 2)
r_r<-round(x = (r_mu1-r_mu2)+(r_tl*sqrt(((r_s2)^2/r_n12)+((r_s2)^2/r_n12))),digits = 2)
cat(r_l,'< mu1-mu2 <',r_r)

#3.25 

p_n12<-50

p_mu1<-78.3

p_mu2<-87.2

p_s1<-5.6

p_s2<-6.3



#Equal Variance

p_s2<-(((p_n12-1)*p_s1^2)+((p_n12-1)*p_s2^2))/(p_n12+p_n12-2)

p_s<-round(x = sqrt(p_s2),digits = 2)



#95% of Confidence Interval

p_a<-1-0.95 #0.05

a_l<-p_a/2

p_l<-(-(round(x = qt(a_l,df = 98),digits = 2)))



p_cl<-round(x = (p_mu1-p_mu2)-(p_l*p_s*sqrt((1/p_n12)+(1/p_n12))),digits = 2)

p_cr<-round(x = (p_mu1-p_mu2)+(p_l*p_s*sqrt((1/p_n12)+(1/p_n12))),digits = 2)

cat(p_cl,'< mu1 - mu2 < ',p_cr)
## -11.26 < mu1 - mu2 <  -6.54

#3.26
fine<-c(20,31,18,23,23,28,23,26,27,26,12,17,25)
course<-c(19,30,32,28,15,26,35,18,25,27,35,35)
s_mu1<-mean(fine)
s_mu2<-round(x = mean(course),digits = 2)
s_n1<-length(fine)
s_n2<-length(course)
s_sd1<-round(x = sd(fine),digits = 2)
s_sd2<-round(x = sd(course),digits = 2)
#1.By assuming Equal Variance
s_spooled<-round(x = (((s_n1-1)*s_sd1^2)+((s_n2-1)*s_sd2))/(s_n1+s_n2-2),digits = 2)
s_sq<-round(x = sqrt(s_spooled),digits = 2)
#Confidence Interval of 95%
sa<-1-0.95
a<-sa/2
sa_l<-(-(round(x = qt(p = a,df = 23),digits = 2))) #DOF is 13+12-2
s_l<-round(x = (s_mu1-s_mu2)-(sa_l*s_sq*sqrt((1/s_n1)+(1/s_n2))),digits = 2)
s_r<-round(x = (s_mu1-s_mu2)+(sa_l*s_sq*sqrt((1/s_n1)+(1/s_n2))),digits = 2)
cat(s_l,'< mu1-mu2 <',s_r)
#2. By assuming unequal variablity
s_d<-round(x = (((s_sd1)^2/s_n1)+((s_sd2)^2/s_n2))^2,digits = 2)
s_u<-((((((s_sd1)^2/s_n1)^2)/(s_n1-1))+(((s_sd2)^2/s_n2)/(s_n2-1))))
s_dof<-ceiling(s_d/s_u)
s_l<-(-(round(x = qt(p = a,df = s_dof),digits = 2)))
sl<-round(x = (s_mu1-s_mu2)-(s_l*sqrt((s_sd1^2/s_n1)+(s_sd2^2/s_n2))),digits = 2)
sr<-round(x = (s_mu1-s_mu2)+(s_l*sqrt((s_sd1^2/s_n1)+(s_sd2^2/s_n2))),digits = 2)
cat(sl,'< mu1-mu2 <',sr)
#3.27

rt_37<-c(0.98,0.83,0.99,0.86,0.90,0.81,0.94,0.92,0.87)

rt_18<-c(1.20,1.18,1.33,1.21,1.20,1.07,1.13,1.12)

rt_mu1<-round(x = mean(rt_37),digits = 2)

rt_mu2<-round(x = mean(rt_18),digits = 2)

rt_sd1<-round(x = sd(rt_37),digits = 2)

rt_sd2<-round(x = sd(rt_18),digits = 2)

rt_n1<-length(rt_37)

rt_n2<-length(rt_18)



#By assuming Equal Variance

rt_spooled<-round(x = (((rt_n1-1)*rt_sd1^2)+((rt_n2-1)*rt_sd2^2))/(rt_n1+rt_n2-2),digits = 4)

rt_sq<-sqrt(rt_spooled)  



#Confidence Interval of 95% --> alpha is 0.05

rt_l<-(-(round(x = qt(p = 0.025,df = 15),digits = 2))) #9+8-2 is DOF 



rt_cl<-round(x = (rt_mu1-rt_mu2)-(rt_l*rt_sq*sqrt((1/rt_n1)+(1/rt_n2))),digits = 2)

rt_cr<-round(x = (rt_mu1-rt_mu2)+(rt_l*rt_sq*sqrt((1/rt_n1)+(1/rt_n2))),digits = 2)

cat(rt_cl,'< mu1-mu2< ',rt_cr)
#By assuming Unequal Variance

rt_u<-((rt_sd1^2/rt_n1)+(rt_sd1^2/rt_n2))^2

rt_d<-((((rt_sd1^2/rt_n1)^2)/(rt_n1-1))+(((rt_sd2^2/rt_n2)^2)/(rt_n2-1)))

rt_dof<-ceiling(x = rt_u/rt_d)



rt_ql<-(-(round(x = qt(p = 0.025,df = rt_dof))))



rtl<-round(x = (rt_mu1-rt_mu2)-(rt_ql*sqrt((rt_sd1^2/rt_n1)+(rt_sd2^2/rt_n2))),digits = 2)

rtr<-round(x = (rt_mu1-rt_mu2)+(rt_ql*sqrt((rt_sd1^2/rt_n1)+(rt_sd2^2/rt_n2))),digits = 2)

cat(rtl,'<mu1-mu2<',rtr)
## -0.35 <mu1-mu2< -0.21
#Comment



#3.28

#Assuming Equal Variablity

sd_n1<-200

sd_n2<-100

sd_mu1<-10.9

sd_mu2<-10.5

sd_sd1<-2

sd_sd2<-3

#Assuming COnfidenc interval of 95%

sd_spooled<-round(x = (((sd_n1-1)*sd_sd1^2)+((sd_n2-1)*sd_sd2^2))/(sd_n1+sd_n2-2),digits = 2)

sd_sq<-round(x = sqrt(sd_spooled),digits = 2)



sd_l<-(-(round(x = round(x = qt(p = 0.025,df = 298)),digits = 2)))#DOF is 200+100-2



sdl<-round(x = (sd_mu1-sd_mu2)-(sd_l*sd_sq*sqrt((1/sd_n1+1/sd_n2))),digits = 2)

sdr<-round(x = (sd_mu1-sd_mu2)+(sd_l*sd_sq*sqrt((1/sd_n1+1/sd_n2))),digits = 2)

cat(sdl,'< mu1 - mu2 <',sdr)

#3.29

d_s1<-c(45,32,58,59,60,44,47,51,42,38)

d_s2<-c(47,34,60,57,63,38,49,53,46,41)

d_mu1<-round(x = mean(d_s1),digits = 2)

d_mu2<-round(x = mean(d_s2),digits = 2)

d_n12<-length(d_s1)

d_sd1<-round(x = sd(d_s1),digits = 2)

d_sd2<-round(x = sd(d_s2),digits = 2)

#Calculating Degree of Freedom 

d_u<-((d_sd1^2/d_n12)+(d_sd2^2/d_n12))^2

d_d<-((((d_sd1^2/d_n12)^2)/(d_n12-1))+(((d_sd2^2/d_n12)^2)/(d_n12-1)))

d_dof<-ceiling(d_u/d_d)



#Confidenc Interval of 99% so Alpha is 0.1

d_l<-(-(round(x = qt(p = 0.005,df = d_dof),digits = 2)))



dl<-round(x = (d_mu1-d_mu2)-(d_l*sqrt((d_sd1^2*d_n12)+(d_sd2^2*d_n12))),digits = 2)

dr<-round(x = (d_mu1-d_mu2)+(d_l*sqrt((d_sd1^2*d_n12)+(d_sd2^2*d_n12))),digits = 2)  

cat(dl,'< mu1 - mu2 <',dr)

#3.30

w_s1<-c(65,67,67,76,70,69,62)

w_s2<-c(66,60,64,68,65,66,60)

w_mu1<-round(x = mean(w_s1),digits = 2)

w_mu2<-round(x = mean(w_s2),digits = 2)

w_sd1<-round(x = sd(w_s1),digits = 2)

w_sd2<-round(x = sd(w_s2),digits = 2)

w_n12<-length(w_s2)

#Calculating Degree of Freedom

w_u<-((w_sd1^2/w_n12)+(w_sd2^2/w_n12))^2

w_l<-((((w_sd1^2/w_n12)^2)/(w_n12-1))+(((w_sd2^2/w_n12)^2)/(w_n12-1)))

w_dof<-ceiling(w_u/w_l)



#Confidence Interval of 95% So alpha is 0.05

w_l<-(-(round(x = qt(p = 0.025,df = w_dof),digits = 2)))



wl<-round(x = (w_mu1-w_mu2)-(w_l*sqrt((w_sd1^2/w_n12)+(w_sd2^2/w_n12))),digits = 2)

wr<-round(x = (w_mu1-w_mu2)+(w_l*sqrt((w_sd1^2/w_n12)+(w_sd2^2/w_n12))),digits = 2)

cat(wl,'< mu1 - mu2 < ',wr)

#3.31

b_s1<-c(2750,2360,2950,2830,2250)

b_s2<-c(2850,2380,2930,2860,2320)

b_mu1<-round(x = mean(b_s1),digits = 2)

b_mu2<-round(x = mean(b_s2),digits = 2)

b_sd1<-round(x = sd(b_s1),digits = 2)

b_sd2<-round(x = sd(b_s2),digits = 2)

b_n12<-length(b_s2)

#Calcualting Degree of Freedom 

b_u<-((b_sd1^2/b_n12)+(b_sd2^2/b_n12))^2

b_d<-(((b_sd1^2/b_n12)^2)/(b_n12-1)+(((b_sd2^2/b_n12)^2)/(b_n12-1)))

b_dof<-ceiling(b_u/b_d)



#Confidence Interval is 95% & Alpha is 0.05

b_l<-(-(round(x = qt(p = 0.025,df = b_dof),digits = 2)))



bl<-round(x = (b_mu1-b_mu2)-(b_l*sqrt((b_sd1^2/b_n12)+(b_sd2^2/b_n12))),digits = 2)

br<-round(x = (b_mu1-b_mu2)+(b_l*sqrt((b_sd1^2/b_n12)+(b_sd2^2/b_n12))),digits = 2)

cat(bl,'< mu1 - mu2 <', br)

#3.32

m_x1<-75

m_x2<-80

m_n1<-1500

m_n2<-2000

m_phat1<-m_x1/m_n1

m_phat2<-m_x2/m_n2

#Confidence Interval is 95 % for(p1-p2) Alpha is 0.05 

ma<-(-(round(x = qnorm(p = 0.025),digits = 2)))



ml<-round(x = (m_phat1-m_phat2)-(ma*sqrt(((m_phat1*(1-m_phat1))/m_n1)+(m_phat2*(1-m_phat2))/m_n2)),digits = 4)

mr<-round(x = (m_phat1-m_phat2)+(ma*sqrt(((m_phat1*(1-m_phat1))/m_n1)+(m_phat2*(1-m_phat2))/m_n2)),digits = 4)  

cat(ml,'< p1 - p2 <',mr)

#3.33

g_x1=250

g_x2<-275

g_n12<-1000

g_phat1<-g_x1/g_n12

g_phat2<-g_x2/g_n12

#Confidence Interval is 95% for (p1-p2) Alphsa is 0.05

ga<-(-(round(x = qnorm(p = 0.025),digits = 2)))



gl<-round(x = ((g_phat1-g_phat2)-(ga*sqrt(((g_phat1*(1-g_phat1))/g_n12)+((g_phat2*(1-g_phat2))/g_n12)))),digits = 4)

gr<-round(x = ((g_phat1-g_phat2)+(ga*sqrt(((g_phat1*(1-g_phat1))/g_n12)+((g_phat2*(1-g_phat2))/g_n12)))),digits = 4)



cat(gl,'< p1 - p2 <',gr)

#3.34

f_n1=16

f_n2=11

f_mu1=4.653

f_mu2=4.274

f_sd1=0.012

f_sd2=0.02



#Calculating Degree of Freedom

f_u=((f_sd1^2/f_n1)+(f_sd2^2/f_n2))^2

f_l=((((f_sd1^2/f_n1)^2)/(f_n1-1))+(((f_sd2^2/f_n2)^2)/(f_n2-1)))    

f_dof=ceiling(f_u/f_l)



#Confidence Interval is 98% so alpha is 0.02

fa<-(-(round(x = qt(p = 0.01,df = f_dof),digits = 2)))



fl<-round(x = (f_mu1-f_mu2)-(fa*sqrt((f_sd1^2/f_n1)+(f_sd2^2/f_n2))),digits = 2)

fr<-round(x = (f_mu1-f_mu2)+(fa*sqrt((f_sd1^2/f_n1)+(f_sd2^2/f_n2))),digits = 2)

cat(fl,'f_mu1 - f_mu2 <',fr)
## 0.36 f_mu1 - f_mu2 < 0.4
#3.35

e_n1=10

e_n2=11

sd1=8

sd2=7

e_ssq1=sd1^2

e_ssq2=sd2^2

e_r1=e_n1-1

e_r2=e_n2-1



#Confidence Interval is 0.90 & alpha is 0.05

cl<-qf(p = 0.05,df1 = 10,df2 = 9)

cr<-qf(p = 0.95,df1 = 9,df2 = 10)

el<-round(x = (e_ssq2/e_ssq1)*cl,digits = 3)

er<-round(x = (e_ssq2/e_ssq1)*cr,digits = 3)

cat(el,'< Ratio <',er)

#3.36

err<-0.03

ci<-0.99  #Confidence LEvel of 99%



#a) Finding Sample Size if Estimation is 0.69

pcap<-0.69

ap<-(1-ci)/2

z<-(-(round(x = qnorm(p = ap),digits = 2)))

n<-ceiling((pcap*(1-pcap)*z^2)/err^2)

cat("Sample size is : ",n)

#c)

#Confidence INterval is 90%

cic=0.90

apc<-(1-0.90)/2

zc<-(-(round(x = qnorm(p = apc),digits = 2)))

nc<-ceiling((pcap*(1-pcap)*zc^2)/err^2)

cat("Sample Size is : ",nc)