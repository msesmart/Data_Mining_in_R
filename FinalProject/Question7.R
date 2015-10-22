library(ISLR)
library(MASS)
library(VGAM)

setwd("C:/Dropbox/DataMining/FinalWork")
OEA.train<-read.csv("SYS6018-FinalOEA_train.csv",header=T)
OEA.test<-read.csv("SYS6018-FinalOEA_test.csv",header=T)
train=sample(nrow(OEA.train),0.7*nrow(OEA.train))
names(OEA.train)


OEA.vglm=vglm(quality~.,data=OEA.train[train,-1],family=cumulative(link="logit",parallel=TRUE))
#OEA.pred=predictvglm(OEA.vglm,newdata=OEA.train[,-1],type="response")
OEA.pred=predict(OEA.vglm,newdata=OEA.train[-train,-1],type="response")
OEA.pred.quality=apply(OEA.pred,1,which.max)+2
test.quality.pred=predict(OEA.vglm,newdata=OEA.test[,-1],type="response")
test.OEA.pred.quality=apply(test.quality.pred,1,which.max)+2
write.csv(test.OEA.pred.quality,file="test.OEA.pred.quality.vglm.csv")

table(OEA.pred.quality,OEA.train$quality[-train])

# random forest classification
library(randomForest)
OEA.train$type=ifelse(OEA.train$quality<=5,"below",ifelse(OEA.train$quality<8,"above","exceptional"))
# random forest for type
OEA.type.rf=randomForest(as.factor(type)~.,data=OEA.train[train,-c(1,13)],mtry=4)
type.pred=predict(OEA.type.rf,newdata=OEA.train[-train,-c(1,13)],type="response")
table(type.pred,OEA.train[-train,14])
# random forest for quality
OEA.quality.rf=randomForest(as.factor(quality)~.,data=OEA.train[,-c(1,14)],mtry=4)
quality.pred=predict(OEA.quality.rf,newdata=OEA.train[-train,-c(1,14)],type="response")
table(quality.pred,OEA.train[-train,13])

# fit test set
test.quality.pred=predict(OEA.quality.rf,newdata=OEA.test[,-1],type="response")
test.quality.pred=predict(OEA.quality.rf,newdata=OEA.test[,-1],type="prob")
write.csv(test.quality.pred,file="test.quality.pred.prob.rf.csv")
unique(test.quality.pred)
table(test.quality.pred)

