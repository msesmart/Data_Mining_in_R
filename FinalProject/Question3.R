library(glmnet)
library(ISLR)
library(MASS)
setwd("C:/Dropbox/DataMining/FinalWork")
trainSet<-read.csv("SYS6018-FinalQ2_train.csv",header=T)
testSet<-read.csv("SYS6018-FinalQ2_test.csv",header=T)

numLambda<-300
x<-model.matrix(y~.,trainSet)[,-1]
test.x=model.matrix(y~.,testSet)[,-1]
#grid=10^seq(-2,6,length=numLambda)
grid=10^seq(-2,3.5,length=numLambda)
ridge.mod<-glmnet(x,trainSet$y,alpha=0,lambda=grid)
#ridge.mod$df

set.seed (1)
#cv.out =cv.glmnet(x,trainSet$y,alpha =0)

plot(cv.out,ylab="Cross-Validation MSE")
bestlam=cv.out$lambda.min
cv.out$lambda.1se

ridge.pred<-predict(ridge.mod,s=bestlam,newx=x)
coef.ridge.mod=predict(ridge.mod,type="coefficients",s=bestlam)
coef.ridge.mod
meanRSS=mean((ridge.pred-trainSet$y)^2)
y.mean=mean(trainSet$y)
fit.RSS=sum((ridge.pred-trainSet$y)^2)
fit.TSS=sum((trainSet$y-y.mean)^2)
fit.R2=1.0-fit.RSS/fit.TSS


# mean RSS of train data
y.train<-trainSet[,1]
meanRSS.ridge<-c(rep(0,numLambda))
for(i in 1:numLambda)
{
  ridge.pred<-predict(ridge.mod,s=grid[i],newx=x)
  meanRSS.ridge[i]=mean((ridge.pred-y.train)^2)
}
plot(grid,meanRSS.ridge)
meanRSS.ridge=data.frame(grid,meanRSS.ridge)
write.csv(meanRSS.ridge,file="meanRSS.ridge.train.csv")

# mean RSS of test data
#x<-model.matrix(y~.,testSet)[,-1]
select(lm.ridge(trainSet$y~x1+x2+x3+x4+x5+x6,data=trainSet[,-1],lambda=grid))
       
grid=10^seq(-2,1.5,length=numLambda)
y.test<-testSet[,1]
meanRSS.ridge<-c(rep(0,numLambda))
for(i in 1:numLambda)
{
  #ridge.mod<-glmnet(x,trainSet$y,alpha=0,lambda=grid[i])
  ridge.mod<-lm.ridge(trainSet$y~x1+x2+x3+x4+x5+x6,data=trainSet[,-1],lambda=grid[i])
  ridge.pred<-scale(testSet[,2:7],center=T,scale=ridge.mod$scales)%*% ridge.mod$coef+ridge.mod$ym
  #ridge.pred<-predict(ridge.mod,test.x)
  #meanRSS.ridge[i]=mean((ridge.pred-y.test)^2)
  meanRSS.ridge[i]=ridge.mod$GCV
}
plot(grid,meanRSS.ridge,ylab="GCV",xlab="lambda",type="l",lwd=3,col="red")
meanRSS.ridge=data.frame(grid,meanRSS.ridge)
write.csv(meanRSS.ridge,file="meanRSS.ridge.test.csv")

coef.ridge.mod=coef(ridge.mod)
plot(ridge.mod$lambda,coef.ridge.mod[2,],col="black")
library(MASS)

Tcoef.ridge.mod=t(coef.ridge.mod)
dim(Tcoef.ridge.mod)
write.matrix(Tcoef.ridge.mod,file="Tcoef.ridge.mod.csv",sep=",")
write.csv(ridge.mod$lambda,file="ridge.mod_lambda.csv")

meanRSS_grid<-data.frame(grid,meanRSS.ridge)
write.csv(meanRSS_grid,file="meanRSS_grid.csv")

cv.ridge=cv.glmnet(x,trainSet$y,alpha=0)
plot(cv.ridge)
bestlam=cv.ridge$lambda.min

x1<-data.frame(ridge.mod$lambda,ridge.mod$beta[6,])
write.csv(x1,file="ridge.x6.csv")
plot(coef(ridge.mod))
coef(ridge.mod)[,50]
plot(ridge.mod$lambda,ridge.mod$beta[6,])
write.csv(a1,file="ridge.a1.dat")

allSet<-rbind(trainSet,testSet)
