library(lubridate)

SP500_Px = data.frame(read.csv('SP500.csv'))

SP500_Px$Date = strptime(SP500_Px$Date, "%m/%d/%Y")

MaxDate = as.POSIXlt(max(SP500_Px$Date))
MinDate = as.POSIXlt(min(SP500_Px$Date))

Window_Train = 252

#calc return series
#specify classification
#select/extract features

#build loop
    
    #slice train data
    #fit model
    #make prediction
    #calculate result
    #calculate cumulative performance
  