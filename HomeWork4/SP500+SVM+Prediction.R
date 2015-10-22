library(lubridate)
library(e1071)
install.packages("quantmod")
require(quantmod)
setwd("C:/Dropbox/DataMining/HW4")


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

for (t in ((length(SP500_Hist$SP500)-(FirstPeriodwithData + TrainWindow)) -5):(length(SP500_Hist$SP500)-(FirstPeriodwithData + TrainWindow)) ){
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
  