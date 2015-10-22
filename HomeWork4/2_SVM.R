library(lubridate)
library(e1071)
require(quantmod)
SP500_Px = data.frame(read.csv('SP500.csv'))
SP500_Px$Date = strptime(SP500_Px$Date, "%m/%d/%Y")
MaxDate = as.POSIXlt(max(SP500_Px$Date))
MinDate = as.POSIXlt(min(SP500_Px$Date))

# read and massage data
SP500_Px = data.frame(read.csv('SP500.csv'))
SP500_Px$Date = strptime(SP500_Px$Date, "%m/%d/%Y")
SP500_Hist = data.frame(SP500_Px[order(SP500_Px$Date), ])

#calc return series
SP500_Hist$Ret = rep(0,length(SP500_Hist$SP500))
SP500_Hist$Ret[-1] = diff(SP500_Hist$SP500)/SP500_Hist$SP500[-length(SP500_Hist$SP500)]


#specify classification
SP500_Hist$UpDown = rep(-1,length(SP500_Hist$SP500))
SP500_Hist$UpDown[SP500_Hist$Ret>0] = 1

#select/extract features 
#Return from period t-1
SP500_Hist$R_tm1 = rep(0,length(SP500_Hist$SP500))
SP500_Hist$R_tm1[-1] = SP500_Hist$Ret[-length(SP500_Hist$SP500)]

#Return from period t-2
SP500_Hist$R_tm2 = rep(0,length(SP500_Hist$SP500))
SP500_Hist$R_tm2[-1] = SP500_Hist$R_tm1[-length(SP500_Hist$SP500)]

FirstPeriodwithData = 4

# Prepare for Loop
SP500_Hist$Position = rep(0,length(SP500_Hist$SP500))
SP500_Hist$Wealth = rep(1,length(SP500_Hist$SP500))


MaxDate = as.POSIXlt(max(SP500_Hist$Date))
MinDate = as.POSIXlt(min(SP500_Hist$Date))
TrainWindow = 126
StartDate = as.POSIXlt(SP500_Px[TrainWindow,]$Date)


#build loop

for (t in ((length(SP500_Hist$SP500)-(FirstPeriodwithData + TrainWindow)) ):(length(SP500_Hist$SP500)-(FirstPeriodwithData + TrainWindow)) ){
  #for (t in 0:(length(SP500_Hist$SP500)-(FirstPeriodwithData + TrainWindow)) ){
  print(SP500_Hist$Date[t+FirstPeriodwithData + TrainWindow])
  today = t+FirstPeriodwithData + TrainWindow
  index_slice = c((today - TrainWindow):(today - 1))
  
  #slice train data
  #trainData = data.frame(SP500_Hist[t+FirstPeriodwithData:t+FirstPeriodwithData + TrainWindow, c("UpDown","R_tm1","R_tm2")]
  mu_R_tm1 = mean(SP500_Hist$R_tm1[index_slice])
  mu_R_tm2 = mean(SP500_Hist$R_tm2[index_slice])
  sd_R_tm1 = sd(SP500_Hist$R_tm1[index_slice])
  sd_R_tm2 = sd(SP500_Hist$R_tm2[index_slice])
  
  trainData=data.frame(R_tm1=(SP500_Hist$R_tm1[index_slice]-mu_R_tm1)/sd_R_tm1, R_tm2=(SP500_Hist$R_tm2[index_slice]-mu_R_tm2)/sd_R_tm2, UpDown=as.factor(SP500_Hist$UpDown[index_slice]))
  
  #fit model
  svmfit =svm(UpDown~., data=trainData , kernel ="linear", cost =0.01,scale =FALSE )
  
  #tune
  
  #make prediction  
  testData = data.frame(R_tm1=(SP500_Hist$R_tm1[today]-mu_R_tm1)/sd_R_tm1, R_tm2=(SP500_Hist$R_tm2[today]-mu_R_tm2)/sd_R_tm2, UpDown=as.factor(SP500_Hist$UpDown[today]))
  
  Rpred = predict (svmfit ,testData )
  
  SP500_Hist$Position[today]=as.numeric(levels(Rpred))[Rpred]
  print(SP500_Hist$Position[today])
  SP500_Hist$Wealth[today]=SP500_Hist$Wealth[today-1]*(1+(SP500_Hist$Ret[today]*SP500_Hist$Position[today]))
  
  
  #calculate result
  #calculate cumulative performance
  
}


# (b) ### plot 
x=matrix (c(trainData$R_tm1,trainData$R_tm2) , ncol =2)
y=trainData$UpDown
a<-as.numeric(y)
plot(x, col =(3-a))

#
my.data <- data.frame(x1=trainData$R_tm1, x2=trainData$R_tm2, type=as.factor(y))
w<- t(svmfit$coefs) %*% svmfit$SV
b <- -svmfit$rho
p <- svmfit$SV
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=1)
abline(a=--b/p[1,2], b=-w[1,1]/w[1,2], col="orange", lty=2)
abline(a=--b/p[3,2], b=-w[1,1]/w[1,2], col="blue", lty=2)
#################### (b) plot new
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
library('kernlab')
svp <- ksvm(x,y,type="C-svc")
plot(svp,data=x)
alpha(svp)  # support vectors whose indices may be 
# found with alphaindex(svp)
b(svp)      
plot(x[,2],x[,1], col=as.numeric(y)+2, pch=as.numeric(y)+2, xlab="x.2", ylab="x.1")
w <- colSums(coef(svp)[[1]] * x[unlist(alphaindex(svp)),])
b <- b(svp)
abline(b/w[1],-w[2]/w[1])
abline((b+1)/w[1],-w[2]/w[1],lty=2)
abline((b-1)/w[1],-w[2]/w[1],lty=2)




# svm plot
dat=data.frame(x=x, y=as.factor (y))
svmfit =svm(y~., data=dat , kernel ="linear", cost =10,scale =FALSE )
plot(svmfit , dat)
#
plot(svmfit, dat, svSymbol = 2, dataSymbol = 1)
svmfit
#########################
# svm plot
dat=data.frame(x=x, y=as.factor (y))
svmfit =svm(y~., data=dat , kernel ="linear", cost =10,scale =FALSE )
plot(svmfit , dat)

##(c)
##(d)
SP500_Hist$class<-rep(0,length(SP500_Hist$Ret))
SP500_Hist$class[SP500_Hist$Ret>0.001]<-1
SP500_Hist$class[SP500_Hist$Ret<  -0.001]<-  -1
table(SP500_Hist$class)

trainDataNew=data.frame(R_tm1=(SP500_Hist$R_tm1[index_slice]-mu_R_tm1)/sd_R_tm1, R_tm2=(SP500_Hist$R_tm2[index_slice]-mu_R_tm2)/sd_R_tm2, UpDown=as.factor(SP500_Hist$UpDown[index_slice]),Ret=SP500_Hist$Ret[index_slice])
trainDataNew$class<-rep(0,length(trainDataNew$UpDown))
trainDataNew$class[trainDataNew$Ret>0.001]<-1
trainDataNew$class[trainDataNew$Ret<-0.001]<- -1
table(trainDataNew$class)
# OVO
# class of 0 and 1
trainDataNew$class<-as.factor(trainDataNew$class)
data1<-trainDataNew[trainDataNew$class!=-1,c(1,2,5)]
svmfit1 =svm(class~., data=data1 , kernel ="linear", cost =10,scale =FALSE )
w1<- t(svmfit1$coefs) %*% svmfit1$SV
pred1<-as.matrix(testData[,-3])%*%t(as.matrix(w1))
Rpred1 <-predict (svmfit1 ,testData,type="class" )
print(as.numeric(levels(Rpred1))[Rpred1])
###### class of -1 and 1
data2<-trainDataNew[trainDataNew$class!=0,c(1,2,5)]
svmfit2 =svm(class~., data=data2 , kernel ="linear", cost =10,scale =FALSE )
w1<- t(svmfit2$coefs) %*% svmfit2$SV
pred2<-as.matrix(testData[,-3])%*%t(as.matrix(w1))
Rpred2 = predict (svmfit2 ,testData,type="class" )
Rpred2 = predict (svmfit2 ,testData,type="class" )
print(as.numeric(levels(Rpred2))[Rpred2])
####### class of 0 and -1
data3<-trainDataNew[trainDataNew$class!=1,c(1,2,5)]
svmfit3 =svm(class~., data=data3 , kernel ="linear", cost =10,scale =FALSE )
w1<- t(svmfit3$coefs) %*% svmfit3$SV
pred3<-as.matrix(testData[,-3])%*%t(as.matrix(w1))
Rpred3 = predict (svmfit3 ,testData,type="class" )
Rpred3 = predict (svmfit3 ,testData,type="class" )
print(as.numeric(levels(Rpred3))[Rpred3])
###### OVA
trainData.1=data.frame(R_tm1=(SP500_Hist$R_tm1[index_slice]-mu_R_tm1)/sd_R_tm1, R_tm2=(SP500_Hist$R_tm2[index_slice]-mu_R_tm2)/sd_R_tm2, UpDown=as.factor(SP500_Hist$UpDown[index_slice]),Ret=SP500_Hist$Ret[index_slice])
trainData.1$class<-rep(0,length(trainData.1$UpDown))
trainData.1$class[trainData.1$Ret>0.001]<-1
trainData.1$class[trainData.1$Ret<  -0.001]<-  -1
trainData.1$class<-as.factor(trainData.1$class)
#### -1 vs the rest (code the rest as 1)
data.1<-trainData.1[,c(1,2,5)]
data.1$class<-as.factor(data.1$class)
data.1$class<-ifelse(data.1$class!=-1,1,-1)
svmfit.1 =svm(class~., data=data.1 , kernel ="linear", cost =10,scale =FALSE )
w.1<- t(svmfit.1$coefs) %*% svmfit.1$SV
pred.1<-as.matrix(testData[,-3])%*%t(as.matrix(w.1))     # The output f(x)hat is -7.5813
Rpred.1 = predict (svmfit.1 ,testData,type="class" )
print(as.numeric(levels(Rpred.1))[Rpred.1])
##The answer is 1
#### 0 vs the rest (code the rest as 1)
data.2<-trainData.1[,c(1,2,5)]
data.2$class<-as.factor(data.2$class)
data.2$class<-ifelse(data.2$class!=0,1,0)
svmfit.2 =svm(class~., data=data.2, kernel ="linear", cost =10,scale =FALSE )
w.2<- t(svmfit.2$coefs) %*% svmfit.2$SV
pred.2<-as.matrix(testData[,-3])%*%t(as.matrix(w.2))     # The output f(x)hat is 1.723
Rpred.2 = predict (svmfit.2 ,testData,type="class" )
print(as.numeric(levels(Rpred.2))[Rpred.2])
##The answer is 1
#### 1 vs the rest (code the rest as 0)
data.3<-trainData.1[,c(1,2,5)]
data.3$class<-as.factor(data.3$class)
data.3$class<-ifelse(data.3$class!=1,0,1)
svmfit.3 =svm(class~., data=data.3, kernel ="linear", cost =10,scale =FALSE )
w.3<- t(svmfit.3$coefs) %*% svmfit.3$SV
pred.3<-as.matrix(testData[,-3])%*%t(as.matrix(w.3))     # The output f(x)hat is -5.5715
Rpred.3 = predict (svmfit.3 ,testData,type="class" )
print(as.numeric(levels(Rpred.3))[Rpred.3])
##The answer is 1
# Comparing the three result above the answer should be 1
################################## QSN 3
library('tree')
tree.SP500 =tree(UpDown ~.,trainData)
plot(tree.SP500)
text(tree.SP500 ,pretty =0)
tree.SP500
Rpred = predict (tree.SP500 ,testData,type='class' )
# prune tree
prune.SP500 =prune.misclass (tree.SP500 ,best =5)
plot(prune.SP500)
text(prune.SP500 ,pretty =0)
# plot

plot(trainData$R_tm1,trainData$R_tm2)
plot(trainData$R_tm1,trainData$R_tm2,pch=20,xlab="R_tm1",ylab="R_tm2")
partition.tree(prune.SP500,ordvars=c("R_tm1","R_tm2"),add=TRUE,cex=0.3)
