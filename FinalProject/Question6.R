library(ISLR)
setwd("C:/Dropbox/DataMining/FinalWork")
OSdata<-read.csv("SYS6018-FinalQ6.csv",header=T)
names(OSdata)

# normalize the data variables to have zero mean and standard deviations
PC.OSdata=princomp(OSdata[,-5],scale=T)
PC.OSdata$loadings
PC.OSdata$scores
PC.OSdata$center

# use components 1&2 for QDA
library(MASS)
OSdata.PC12=PC.OSdata$scores[,1:2]
OSdata.PC12=data.frame(OSdata.PC12,OSdata$OS)
names(OSdata.PC12)
qda.fit=qda(OSdata.OS~Comp.1+Comp.2,data=OSdata.PC12)
qda.pred=predict(qda.fit,OSdata.PC12[,1:2])$class
table(qda.pred,OSdata.PC12$OSdata.OS)

# use components 1&2&3 for QDA
OSdata.PC12=PC.OSdata$scores[,1:3]
OSdata.PC12=data.frame(OSdata.PC12,OSdata$OS)
names(OSdata.PC12)
qda.fit=qda(OSdata.OS~Comp.1+Comp.2,data=OSdata.PC12)
qda.pred=predict(qda.fit,OSdata.PC12[,1:3])$class
table(qda.pred,OSdata.PC12$OSdata.OS)

# use original data for QDA
qda.fit=qda(OS~.,data=OSdata)
qda.pred=predict(qda.fit,OSdata[,1:4])$class
table(qda.pred,OSdata$OS)

# debug codes
PC.OSdata=prcomp(OSdata[,-5],scale=T)
PC.OSdata$rotation
PC.OSdata$center
PC.OSdata$x

sum=0
j=2
for(i in 1:4)
{
  sum=sum+(OSdata[j,i]-PC.OSdata$center[i])*PC.OSdata$rotation[i,1]/PC.OSdata$scale[i]
}
sum