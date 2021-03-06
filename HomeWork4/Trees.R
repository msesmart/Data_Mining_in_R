install.packages("rpart")
library(tree)
library(rpart)
library(ISLR)
setwd("C:/Dropbox/DataMining/HW4")
SP500<-read.csv("SP500_trainData.csv")
attach(SP500)
#tree.SP500<-rpart(UpDown~R_tm1+R_tm2,method="class",data=SP500,control=rpart.control(minsplit=11))
tree.SP500<-tree(factor(UpDown)~R_tm1+R_tm2,data=SP500,control=tree.control(nobs=nrow(SP500),mincut=6))
plot(tree.SP500, uniform=T,margin = 0.1, minbranch = 0.05)
text(tree.SP500, use.n=TRUE, all=TRUE, cex=.8, col="red")

prune.SP500<-prune.misclass(tree.SP500,best=5)
plot(prune.SP500, uniform=T,margin = 0.1, minbranch = 0.05)
text(prune.SP500, all=T,cex=.75, col="red")

plot(SP500$R_tm1, SP500$R_tm2,  pch=20, col=as.numeric(SP500$UpDown)+10,xlab="R_tm1",ylab="R_tm2")
partition.tree(prune.SP500,ordvars=c("R_tm1","R_tm2"),add=TRUE)
