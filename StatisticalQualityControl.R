library(qcc)
#make 2 plots in 1 figure
par(mfrow=c(1,1))

#points have base value of 10 w/ normally distributed error
lugnuts <- rep(10, 100) + rnorm(100, mean=0, sd=0.5)
qcc(lugnuts, type="xbar.one", center=10, add.stats=T,
    title="1st Batch", xlab="i-th lugnut produced")

#first 90 points have base value of 10 w/ normally distributed error,
#last 10 points have base value of 11 w/ normally distributed error
lugnuts <- c(rep(10, 90), rep(11, 10)) + rnorm(100, mean=0, sd=0.5)
qcc(lugnuts, type="xbar.one", center=mean(lugnuts), add.stats=T,
    title="2nd Batch", xlab="i-th lugnut produced")

#example using holdout/test sets
lugnuts <- rep(10, 100) + rnorm(100, mean=0, sd=0.5)
qcc(lugnuts, newdata=rep(11, 10) + rnorm(10, mean=0, sd=0.5),
    type="xbar.one", center=10, add.stats=FALSE, title="2nd Batch", xlab="i-th lugnut produced")

#example
(diameters <- as.data.frame(replicate(4, rnorm(10,mean=1.31,sd=0.05))))
(q <- qcc(diameters, type="R", nsigmas=3))
(q <- qcc(diameters, type="xbar", nsigmas=3))

#np chart
np1 <- read.csv("C:/Users/vignesht/Downloads/np1.csv", sep="")
View(np1)
attach(np1)
np1
nrow(np1)
qcc(np1$def,type="np",sizes=50)


#np2 chart
np2 <- read.csv("C:/Users/vignesht/Downloads/np2.csv", sep="")
View(np2)
qcc(np2$def,type="np",sizes=50)
#c chart
c1 <- read.csv("C:/Users/vignesht/Downloads/c1.csv", sep="")
View(c1)
attach(c1)
c1
qcc(c1$defects,type="c",sizes=25)
