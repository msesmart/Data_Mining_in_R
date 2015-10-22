library(ISLR)
library(splines)
library(rms)
setwd("C:/Dropbox/DataMining/FinalWork")
trainSet<-read.csv("SYS6018-FinalQ4_train.csv",header=T)
testSet<-read.csv("SYS6018-FinalQ4_test.csv",header=T)
summary(trainSet)

xlims=range(trainSet$x)
x.grid=seq(from=xlims[1],to=xlims[2],by=(xlims[2]-xlims[1])/100)


#fit=lm(y~bs(x,knots=c(-0.6,0.0017,0.6166)),data=trainSet)
fit=lm(y~bs(x,df=9),data=trainSet)
pred=predict(fit,newdata=list(x=x.grid),se=T)
plot(trainSet$x,trainSet$y,col="red",xlab="X",ylab="Y",lwd=2)
points(testSet$x,testSet$y,col="green",lwd=2)
lines(x.grid,pred$fit,lwd=3)
lines(x.grid,pred$fit+2*pred$se,lty="dashed")
lines(x.grid,pred$fit-2*pred$se,lty="dashed")

# MSE vs. number of knots in cubic Splines using trainSet
k=20
mean.knots=c(rep(0,k))
ndf=seq(2,1+k)
nfreedom=nrow(trainSet)
for(i in 1:k)
{
  fit=lm(y~bs(x,df=3+ndf[i]),data=trainSet)
  x.pred=predict(fit,newdata=list(x=trainSet$x),se=T)
  #mean.knots[i]=mean((x.pred$fit-trainSet$y)^2)
  mean.knots[i]=sum((x.pred$fit-trainSet$y)^2)/(nfreedom-4-ndf[i])
}
plot(ndf,mean.knots,xlab="Number of knots",ylab="MSE",type="b",col="blue",lwd=2)

# MSE vs. number of knots in cubic Splines using testSet 
k=20
mean.knots=c(rep(0,k))
ndf=seq(2,1+k)
nfreedom=nrow(testSet)
for(i in 1:k)
{
  fit=lm(y~bs(x,df=3+ndf[i]),data=trainSet)
  x.pred=predict(fit,newdata=list(x=testSet$x),se=T)
  mean.knots[i]=sum((x.pred$fit-testSet$y)^2)/(nfreedom-4-ndf[i])
  #mean.knots[i]=mean((x.pred$fit-testSet$y)^2)
}
plot(ndf,mean.knots,xlab="Number of knots",ylab="MSE",type="b",col="blue",lwd=2)

# The training & test RSS of 6-Knots cubic spline
fit=lm(y~bs(x,df=3+6),data=trainSet)
pred=predict(fit,list(x=trainSet$x),se=T)
sum((pred$fit-trainSet$y)^2)
pred=predict(fit,list(x=testSet$x),se=T)
sum((pred$fit-testSet$y)^2)

attr(bs(trainSet$x,df=3+6),"knots")

# 5-order polynomial fit
fit=lm(y~bs(x,df=3+6),data=trainSet)
fit.poly5=lm(y~poly(x,5),data=trainSet)
anova(fit.poly5,fit)
pred=predict(fit,newdata=list(x=x.grid),se=T)
poly5.pred=predict(fit.poly5,newdata=data.frame(x=x.grid))
plot(trainSet$x,trainSet$y,col="red",xlab="X",ylab="Y",lwd=2)
lines(x.grid,pred$fit,lwd=3)
lines(x.grid,poly5.pred,lty="dashed",col="blue",lwd=3)

# 6-degree smoothing spline
fit.smooth=smooth.spline(trainSet$x,trainSet$y,df=6)
smooth.pred=predict(fit.smooth,trainSet$x,se=T)
sum((smooth.pred$y-trainSet$y)^2)
smooth.pred=predict(fit.smooth,x.grid,se=T)
plot(trainSet$x,trainSet$y,col="red",xlab="X",ylab="Y",lwd=2)
lines(x.grid,pred$fit,lwd=3)
lines(x.grid,smooth.pred$y,lty="dashed",col="blue",lwd=3)

# local regression with span=0.2
fit.local=loess(y~x,span=0.2,data=trainSet)
local.pred=predict(fit.local,data.frame(x=trainSet$x))
sum((local.pred-trainSet$y)^2)
local.pred=predict(fit.local,data.frame(x=testSet$x))
sum((local.pred-testSet$y)^2)

local.pred=predict(fit.local,data.frame(x=x.grid))
plot(trainSet$x,trainSet$y,col="red",xlab="X",ylab="Y",lwd=2)
lines(x.grid,pred$fit,lwd=3)
lines(x.grid,local.pred,lty="dashed",col="blue",lwd=3)

attr(terms(fit),"predvars")
attr(terms(fit),"names")


plot(trainSet$x,trainSet$y,col="red")
points(trainSet$x,x.pred$fit,lwd=1)


