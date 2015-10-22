library(ISLR)
setwd("C:/Dropbox/DataMining/FinalWork")
Q2.train<-read.csv("SYS6018-FinalQ2_train.csv",header=T)
Q2.test<-read.csv("SYS6018-FinalQ2_test.csv",header=T)
names(Q2.train)

pairs(Q2.train)
cor(Q2.train)
attach(Q2.train)
plot(as.factor(x2),y,data=Q2.train,xlab="x2",ylab="y",col="red")

# linear fit with four variables
linear.fit=lm(y~x1+x2+x3+x4,data=Q2.train)
summary(linear.fit)
# remove x4
linear.fit=lm(y~x1+x2+x3,data=Q2.train)
summary(linear.fit)
# remove x2
linear.fit=lm(y~x1+x3,data=Q2.train)
summary(linear.fit)

# mean RSS R^2 for train data
#data.x1x3=data.frame(Q2.train$x1,Q2.train$x3)
#fit.pred=predict(linear.fit,data.x1x3)
fit.pred=predict(linear.fit,Q2.train)
fit.RSS=sum((fit.pred-Q2.train$y)^2)
fit.meanRSS=mean((fit.pred-Q2.train$y)^2)
y.mean=mean(Q2.train$y)
fit.TSS=sum((Q2.train$y-y.mean)^2)
fit.R2=1.0-fit.RSS/fit.TSS

# mean RSS R^2 for test data
fit.pred=predict(linear.fit,Q2.test)
fit.RSS=sum((fit.pred-Q2.test$y)^2)
fit.meanRSS=mean((fit.pred-Q2.test$y)^2)
y.mean=mean(Q2.test$y)
fit.TSS=sum((Q2.test$y-y.mean)^2)
fit.R2=1.0-fit.RSS/fit.TSS

# start from y~x1+x2+x3+x4+x5+x6
linear.fit=lm(y~x1+x2+x3+x4+x5+x6,data=Q2.train)
summary(linear.fit)
# remove x3
linear.fit=lm(y~x1+x2+x4+x5+x6,data=Q2.train)
summary(linear.fit)
# remove x4
linear.fit=lm(y~x1+x2+x5+x6,data=Q2.train)
summary(linear.fit)
# remove x1
linear.fit=lm(y~x2+x5+x6,data=Q2.train)
summary(linear.fit)
# remove x2
linear.fit=lm(y~x5+x6,data=Q2.train)
summary(linear.fit)

# mean RSS R^2 for train data
fit.pred=predict(linear.fit,Q2.train)
fit.RSS=sum((fit.pred-Q2.train$y)^2)
fit.meanRSS=mean((fit.pred-Q2.train$y)^2)
y.mean=mean(Q2.train$y)
fit.TSS=sum((Q2.train$y-y.mean)^2)
fit.R2=1.0-fit.RSS/fit.TSS

# mean RSS R^2 for test data
fit.pred=predict(linear.fit,Q2.test)
fit.RSS=sum((fit.pred-Q2.test$y)^2)
fit.meanRSS=mean((fit.pred-Q2.test$y)^2)
y.mean=mean(Q2.test$y)
fit.TSS=sum((Q2.test$y-y.mean)^2)
fit.R2=1.0-fit.RSS/fit.TSS


linear.fit=lm(y~x6,data=Q2.train)
summary(linear.fit)
linear.fit=lm(y~x6+x6:x2,data=Q2.train)
summary(linear.fit)
linear.fit=lm(y~x6*x2,data=Q2.train)
summary(linear.fit)


library(rgl)
linear.fit=lm(y~x5+x6,data=Q2.train)
summary(linear.fit)
fit.pred=predict(linear.fit,data=Q2.train)


range(Q2.train$x5)
range(Q2.train$x6)

k=11
x5x6data=matrix(0,nrow=k*k,ncol=2)
colnames(x5x6data)=c("x5","x6")
dx5=15000/(k-1)
dx6=44/(k-1)
for(i in 1:k)
{
  for(j in 1:k)
  {
    x5x6data[(i-1)*k+j,1]=(i-1.0)*dx5-1000.0
    x5x6data[(i-1)*k+j,2]=(j-1.0)*dx6-4.0
  } 
}

y.pred=predict(linear.fit,data.frame(x5x6data))
y.matrix.x5x6=matrix(0,nrow=k,ncol=k)
for(i in 1:k)
{
  for(j in 1:k)
  {
    y.matrix.x5x6[i,j]=y.pred[(i-1)*k+j]
  } 
}
x5.array=seq(-1000,14000,15000/(k-1))
x6.array=seq(-4,40,44/(k-1))

x2true=1
for(i in 2:nrow(Q2.train))
{
  if(Q2.train[i,3]==1)x2true=c(x2true,i)
}

plot3d(Q2.train$x5[x2true],Q2.train$x6[x2true],Q2.train$y[x2true],col="red")
points3d(Q2.train$x5[-x2true],Q2.train$x6[-x2true],Q2.train$y[-x2true],col="blue")
surface3d(x5.array,x6.array,y.matrix.x5x6,col="gray")


# add an interaction of x2 & x6 into y~x5+x6
linear.fit=lm(y~x5+x6+x2:x6,data=Q2.train)
summary(linear.fit)
# consider x2=1
x2.array=c(rep(1,k*k))
x2x5x6data=data.frame(x2.array,x5x6data)
colnames(x2x5x6data)=c("x2","x5","x6")
y.pred.x2T=predict(linear.fit,data.frame(x2x5x6data))
y.matrix.x2Tx5x6=matrix(0,nrow=k,ncol=k)
for(i in 1:k)
{
  for(j in 1:k)
  {
    y.matrix.x2Tx5x6[i,j]=y.pred.x2T[(i-1)*k+j]
  } 
}

# consider x2=0
x2.array=c(rep(0,k*k))
x2x5x6data=data.frame(x2.array,x5x6data)
colnames(x2x5x6data)=c("x2","x5","x6")
y.pred.x2F=predict(linear.fit,data.frame(x2x5x6data))
y.matrix.x2Fx5x6=matrix(0,nrow=k,ncol=k)
for(i in 1:k)
{
  for(j in 1:k)
  {
    y.matrix.x2Fx5x6[i,j]=y.pred.x2F[(i-1)*k+j]
  } 
}

plot3d(Q2.train$x5[x2true],Q2.train$x6[x2true],Q2.train$y[x2true],col="red",xlab="x5",ylab="x6",zlab="y")
points3d(Q2.train$x5[-x2true],Q2.train$x6[-x2true],Q2.train$y[-x2true],col="blue")
#surface3d(x5.array,x6.array,y.matrix.x5x6,col="green",front="lines",back="lines")
surface3d(x5.array,x6.array,y.matrix.x2Tx5x6,col="red",front="lines",back="lines")
surface3d(x5.array,x6.array,y.matrix.x2Fx5x6,col="blue",front="lines",back="lines")


# d)iii. mean RSS and R^2
linear.fit=lm(y~x5+x6+x2:x6,data=Q2.train)
# mean RSS R^2 for train data
fit.pred=predict(linear.fit,Q2.train)
fit.RSS=sum((fit.pred-Q2.train$y)^2)
fit.meanRSS=mean((fit.pred-Q2.train$y)^2)
y.mean=mean(Q2.train$y)
fit.TSS=sum((Q2.train$y-y.mean)^2)
fit.R2=1.0-fit.RSS/fit.TSS

# mean RSS R^2 for test data
fit.pred=predict(linear.fit,Q2.test)
fit.RSS=sum((fit.pred-Q2.test$y)^2)
fit.meanRSS=mean((fit.pred-Q2.test$y)^2)
y.mean=mean(Q2.test$y)
fit.TSS=sum((Q2.test$y-y.mean)^2)
fit.R2=1.0-fit.RSS/fit.TSS
