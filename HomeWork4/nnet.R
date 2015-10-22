library(nnet)
library(neuralnet)
library(RSNNS)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

setwd("C:/Dropbox/DataMining/HW4")
seeds<-read.table('seeds_dataset_noHead.txt',sep="\t")
names(seeds)<-c("A","P","C","LK","WK","AC","LKG","Type")

train<-sample(nrow(seeds),0.7*nrow(seeds))
ideal<-class.ind(seeds$Type)



seedsNnet<-nnet(seeds[train,-8],ideal[train,],size=nnSize,softmax=TRUE)
seedsPred<-predict(seedsNnet,seeds[-train,-8],type="class")
table(seedsPred,seeds[-train,]$Type)

#Perform cv_K fold cross validation
cv_K=20
parts<-c(rep(1.0/cv_K,cv_K))
foldIndexes<-vector(mode = "list", length = length(parts))
totrows<-nrow(seeds)
rownos<-seq(totrows)
for(i in 1:cv_K)
{
  foldIndexes[[i]]<-sample(x = rownos, size = parts[i]*totrows)
  rownos <- setdiff(rownos, foldIndexes[[i]])
}

nnSize=6
errorRate<-0
errorRateArray<-c(1:cv_K)
for(i in 1:cv_K){
  #Segement your data by fold using the which() function 
  testIndexes <- foldIndexes[[i]]
  #Use the test and train data partitions for test Error
  seedsNnet<-nnet(seeds[-testIndexes,-8],ideal[-testIndexes,],size=nnSize,softmax=TRUE)
  seedsPred<-predict(seedsNnet,seeds[testIndexes,-8],type="class")
  table(seedsPred,seeds[testIndexes,]$Type)
  class.pred<-table(seedsPred,seeds[testIndexes,]$Type)
  errorRateArray[i]<- 1-sum(diag(class.pred))/sum(class.pred)
  errorRate<-errorRate+errorRateArray[i]
}
errorRate<-errorRate/cv_K
class.pred
errorRateArray
plot.nnet(seedsNnet)

#use neuralnet
attach(seeds.new)
#seedsNnet<-neuralnet(Type~A+P+C+LK+WK+AC+LKG,seeds[train,],hidden=c(5,5),rep=10,linear.output=FALSE)
seedsNnet<-neuralnet(Type~A+P+C+LK+WK+AC+LKG,seeds.new[train,],hidden=c(6,5),threshold=0.01,stepmax=1e6)
plot.nn(seedsNnet)
#seedsPred<-prediction(seedsNnet,seeds[-train,-8])
cbind(seeds[-train,8],compute(seedsNnet,seeds.new[-train,-8])$net.result)[1:30,]
#seedsPred<-compute(seedsNnet,seeds[-train,-8])
#seedsPred$net.result

#use RSNNS
seedsNnet<-mlp(seeds[train,-8],seeds[train,8], size=c(12,6),linOut=T)
seedsPred<-predict(seedsNnet,seeds[-train,-8])



table(seedsPred,seeds[-train,]$Type)

seedsNnet<-nnet(seeds[train,-8],ideal[train,],size=10,softmax=TRUE)
seedsPred<-predict(seedsNnet,seeds[-train,-8],type="class")
table(seedsPred,seeds[-train,]$Type)
plot.nnet(seedsNnet)

# Normalize data
attach(seeds)
seeds.new <- apply(seeds[,1:7],2,scale)
seeds.new <- data.frame(seeds.new,seeds[,8])

seedsNnet<-nnet(seeds.new[train,-8],ideal[train,],size=10,softmax=TRUE)
seedsPred<-predict(seedsNnet,seeds.new[-train,-8],type="class")
table(seedsPred,seeds[-train,]$Type)

# change size & hidden
seedsNnet<-neuralnet(seeds[train,-8],ideal[train,],hidden=c(5,6))
seedsPred<-predict(seedsNnet,seeds[-train,-8],type="class")
table(seedsPred,seeds[-train,]$Type)


source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(seedsNnet)
