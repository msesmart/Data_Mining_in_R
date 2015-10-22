# Code for Final Exam

# Problem 2
dat2 <- read.csv("SYS6018-FinalQ2_train.csv")
dat2test <- read.csv("SYS6018-FinalQ2_test.csv")
attach(dat2)
# a)
pairs(dat2)
cor(dat2)
str(dat2)
apply(dat2,2,mean)
apply(dat2,2,sd)
plot(as.factor(x2),y,data=dat2,outline=F,xlab="x2",ylab="y",main="boxplot for y and x2")
plot(y,x3)
plot(y,x5)

# b)

pairs(dat2)
str(dat2)
lm.fit2 <- lm(y~x1+x2+x3+x4,data=dat2)
summary(lm.fit2)
# the p-value for x4 is largest, so drop x4
lm.fit21 <- lm(y~x1+x2+x3,data=dat2)
summary(lm.fit21)
# the p-value for x2 is largest, so drop x2
lm.fit22 <- lm(y~x1+x3,data=dat2)
summary(lm.fit22)
2.655anova(lm.fit22)
test.pred <- predict(lm.fit22,dat2test)
test.y <- dat2test$y
ssr <- sum((test.y-test.pred)^2)
sst <- sum((test.y-mean(test.y))^2)
mse <- ssr/(nrow(dat2test)-2)
r2 <- 1-(ssr/sst)
ssr
sst
mse
r2
# c)
lm.fit23 <- lm(y~.,data=dat2)
summary(lm.fit23)
anova(lm.fit23)
lm.fit24 <- lm(y~x1+x2+x3+x5+x6,data=dat2)
summary(lm.fit24)
lm.fit24 <- lm(y~x1+x2+x5+x6,data=dat2)
summary(lm.fit24)
lm.fit25 <- lm(y~x2+x5+x6,data=dat2)
summary(lm.fit25)
lm.fit26 <- lm(y~x5+x6,data=dat2)
summary(lm.fit26)
anova(lm.fit26)
test.pred2 <- predict(lm.fit26,data=dat2test)
test.y <- dat2test$y
ssr2<- sum((test.y-test.pred2)^2)
mse <- ssr2/(nrow(dat2test)-2)
head(test.y-test.pred2)
head(test.y-test.pred)
sst2 <- sum((test.y-mean(test.y))^2)
mean(test.pred2)
r22 <- 1-(ssr2/sst2)
ssr2
sst2
r22
mse

# d)
lm.fit3 <- lm(y~x5+x6+x2*x6,data=dat2)
summary(lm.fit3)

#### Problem 3#####
library(glmnet)
grid<-10^seq(-2,10,length=100)
ridge.fit<-glmnet(as.matrix(dat2[,-1]),as.vector(dat2[,1]),alpha=0,lambda=grid)
names(ridge.fit)
dim(coef(ridge.fit))
coeffi <- as.matrix(coef(ridge.fit))
par(mfrow=c(2,3))
apply(coeffi[2:7,],1,plot,type="l")
ridge.predt <- predict(ridge.fit,s=grid,newx=as.matrix(dat2[,-1]))
dim(ridge.predt)
head(ridge.predt)
msetr <- apply(ridge.predt,2,function(x) mean((dat2$y-x)^2))
par(mfrow=c(1,1))
plot(msetr,type="l",xlab="lambda 10^i",ylab="Mean of RSS for Training")
ridge.predte <- predict(ridge.fit,s=grid,newx=as.matrix(dat2test[,-1]))
msete <- apply(ridge.predte,2,function(x) mean((dat2test$y-x)^2))
plot(msete,type="l",xlab="lambda 10^i",ylab="Mean of RSS for Testing")

###### Problem 6######
dat6 <- read.table("dat6.txt",header=F)
names(dat6) <- c("participant","price","software","aesthetics","brand","os")
library(pls)
pca <- pcr(os~.,data=dat6,scale=T,validation="CV")
summary(pca)
validationplot(pca,val.type="MSEP")
pr.out <- princomp(dat6[,-c(1,6)],cor=T)
names(pr.out)
pr.out$loadings
pr.out$scores
biplot(pr.out,scale=0)
pr.out$call
# we don't need to multiply the data by principal component loading vector
# in order to obtain principal component score vectors. The kth column for 
# score is the kth principal component score vector
score <- data.frame(pr.out$scores)
newdat <- data.frame(score[,c(1,2)],dat[,6])
names(newdat)[3] <- "os"
library(MASS)
qda.fit <- qda(os~.,data=newdat)
qda.fit$
qda.class <- predict(qda.fit,newdat)

#####Problem 7#####
test7 <- read.table("SYS6018.FinalOEA_test",header=T)
train7 <- read.table("SYS6018.FinalOEA_train",header=T)
library(dplyr)
train7$cla <- ifelse(train7$quality<=5,"below",ifelse(train7$quality<8,"above","exceptional"))
train7$quality <- as.factor(train7$quality)
str(train7)
train7tr <- sample_frac(train7,0.7)
train7te <- setdiff(train7,train7tr)
pairs(train7tr)
cor(train7tr)
str(train7tr)
plot(train7$quality,train7$fixed.acidity,outline=F)
plot(train7$quality,train7$volatile.acidity,outline=F)
plot(train7$quality,train7$citric.acid,outline=F)
plot(train7$quality,train7$residual.sugar,outline=F)
plot(train7$quality,train7$chlorides,outline=F)
plot(train7$quality,train7$free.sulfur.dioxide,outline=F)
plot(train7$quality,train7$total.sulfur.dioxide,outline=F)
plot(train7$quality,train7$density,outline=F)

par(mfrow=c(2,2))
for (i in 2:12){
  plot(train7$quality,train7[,i],outline=F,ylab=paste(names(train7)[i],"column=",i,sep=" "))
}
par(mfrow=c(1,1))
apply(train7[,2:12],2,function (x) plot(train7$quality,x,outline=F,ylab=))

# Fit a regression model for quality
lm.fit <- lm(as.numeric(quality)~.-id,data=train7tr)
summary(lm.fit)
lm.pred <- round(predict(lm.fit,train7te))
table(lm.pred,train7te$quality)
mean(lm.pred!=train7te$quality)
lm.pred
lm.fit.poly <- lm(as.numeric(quality)~-id+poly(volatile.acidity,2)+poly(residual.sugar,2)
             +poly(citric.acid,2)+poly(chlorides,2)+poly(free.sulfur.dioxide)
             +poly(total.sulfur.dioxide,2)+poly(density,2)+poly(pH,2)+poly(alcohol),data=train7tr)
summary(lm.fit.poly)
lm.poly.pred <- round(predict(lm.fit.poly,train7te))
mean(lm.poly.pred!=train7te$quality)
# 
install.packages("VGAM")
library(VGAM)
vglm.fit <- vglm(as.factor(quality)~.-id,data=train7tr,family=cumulative)


# 1) try decision tree
library(tree)
head(train7tr)
tree.fit <- tree(as.factor(cla)~.,train7tr[,-c(1,13)])
summary(tree.fit)
plot(tree.fit)
text(tree.fit,pretty=0)
tree.pred <- predict(tree.fit,train7te,type="class")
table(tree.pred,train7te$cla)
mean(tree.pred!=train7te$cla)
# The error rate for decision tree is 25.6%

## Then I try pruned tree
set.seed(1)
cv.fit <- cv.tree(tree.fit,FUN=prune.misclass)
cv.fit
plot(cv.fit$size,cv.fit$dev,type="b")

# Then I try bagging
library(randomForest)
bag.fit <- randomForest(as.factor(cla)~.,train7tr[,-c(1,13)],mtry=11)
bag.fit
yhat.bag <- predict(bag.fit,newdata=train7te)
table(yhat.bag,train7te$cla)
mean(yhat.bag!=train7te$cla)
# the test error for randomforest is 17.17%

## Then I try random forest
rf.fit <- randomForest(as.factor(cla)~.,data=train7tr[,-c(1,13)],mtry=4)
rf.fit.qua <- randomForest(as.factor(quality)~.,data=train7tr[,-c(1,14)],mtry=4,type=prob)
prob.rf <- predict(rf.fit,newdata=train7te,type="prob")
cla.rf <- predict(rf.fit,newdata=train7te,type="response")
head(prob.rf)
head(cla.rf)
table(cla.rf,train7te$cla)
mean(cla.rf!=train7te$cla)
names(yhat.rf)
names(rf.fit)
rf.fit$predicted
length(rf.fit$oob.times)
dim(rf.fit$votes)
yhat.rf.qua <- predict(rf.fit.qua,newdata=train7te)

table(yhat.rf.qua,train7te$quality)
mean(yhat.rf.qua!=train7te$quality)
# the test error for random forest is 17.08%
?predict.randomForest

# try cumulative logit regression
N=nrow(d.train)
train<-sample(1:N,ceiling(0.7*N))
test<-setdiff(1:N,train)
library(VGAM)
clogit<-vglm(as.factor(cla)~.-id,data=train7tr,family=cumulative(link = logit, parallel = TRUE))
clogit
pred<-predict(clogit,train7te,type="response")
as.factor(train7$cla)
pred.cat<-apply(pred,1,which.max)
pred.cat2<-ifelse(pred.cat==1,"above",ifelse(pred.cat==2,"below","exception"))

mean(pred.cat2==train7te$cla)
## The I try SVM
library(e1071)
svm.fit <- svm(as.factor(cla)~.,train7tr[,-c(1,13)],kernel="radial",gamma=1,cost=1)
tune.out <- tune(svm,as.factor(cla)~.,data=train7tr[,-c(1,13)],kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
svm.pred <- predict(tune.out$best.model,newdata=train7te)
table(svm.pred,train7te$cla)
mean(svm.pred!=train7te$cla)
# the test error for SVM is 22.75%