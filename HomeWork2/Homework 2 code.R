magic <- read.csv("~/Documents/Spring 15/Data Mining/magic04.data.txt", header=FALSE)
dim(magic)
names(magic) <- c("length","width","size","conc","concl","asym","m3long","m3trans","alpha","dist","class")
head(magic)
attach(magic)
pairs(magic)
cor(magic[,-11])
set.seed(1)
a<-sample(19020,5000)
test<-magic[a,]
train<-magic[-a,]

## To do logistic regression
# Without model selection procedure
glm.fit0 <- glm(class~.,data=train,family=binomial)
summary(glm.fit0)
glm.prob0 <- predict(glm.fit0,test,type="response")
glm.pred0<-rep("g",5000)
glm.pred0[glm.prob0>0.5] <- "h"
class.test<-test$class
table(glm.pred0,class.test)
# This is our testing error rate for Logistic regression without
# model selection: 0.201
mean(glm.pred0!=class.test)

# I found some variables' p-values are greater than 0.05, so I try backward
# model selection
glm.fit <- glm(class~.,data=train,family=binomial)
summary(glm.fit)
names(train)
# Since conc has the largest p-value, delete it
glm.fit <- glm(class~.,data=train[,-4],family=binomial)
summary(glm.fit)
# Since m3trans has the largest p-value, delete it
glm.fit <- glm(class~.,data=train[,-c(4,8)],family=binomial)
summary(glm.fit)
# Since asym has the largest p-value, delete it
glm.fit <- glm(class~.,data=train[,-c(4,6,8)],family=binomial)
summary(glm.fit)
# Since dist has the largest p-value and greater than 0.05, delete it
glm.fit <- glm(class~.,data=train[,-c(4,6,8,10)],family=binomial)
summary(glm.fit)
# Since width has the largest p-value, delete it
glm.fit <- glm(class~.,data=train[,-c(2,4,6,8,10)],family=binomial)
summary(glm.fit)
contrasts(class)
glm.prob <- predict(glm.fit,test,type="response")
glm.pred<-rep("g",5000)
glm.pred[glm.prob>0.5] <- "h"
class.test<-test$class
table(glm.pred,class.test)

# This is our testing error rate for Logistic regression: 0.2016
mean(glm.pred!=class.test)


## To apply LDA to the data set
library(MASS)
lda.fit <- lda(class~.,data=train)
lda.fit
lda.pred <- predict(lda.fit,test)
names(lda.pred)
head(lda.pred$class)
table(lda.pred$class,class.test)
# This our testing error rate for lda: 0.2102
mean(lda.pred$class!=class.test)

## To apply QDA to the data set
qda.fit <- qda(class~.,data=train)
qda.fit
qda.pred <- predict(qda.fit,test)
names(qda.pred)
head(qda.pred$class)
table(qda.pred$class,class.test)
# This our testing error rate for qda: 0.2086
mean(qda.pred$class!=class.test)

## Then I generize a new testing and training data set and repeat the 
# above steps
a<-sample(19020,2500)
test<-magic[a,]
train<-magic[-a,]

## To do logistic regression
glm.fit <- glm(class~.,data=train,family=binomial)
summary(glm.fit)
names(train)
class.test<-test$class
contrasts(class)
glm.prob <- predict(glm.fit,test,type="response")
glm.pred<-rep("g",2500)
glm.pred[glm.prob>0.5] <- "h"
class.test<-test$class
table(glm.pred,class.test)
table(class.test)
# This is our testing error rate for Logistic regression: 0.2052
mean(glm.pred!=class.test)

## To apply LDA to the data set
library(MASS)
lda.fit <- lda(class~.,data=train)
lda.fit
lda.pred <- predict(lda.fit,test)
names(lda.pred)
head(lda.pred$class)
table(lda.pred$class,class.test)
# This our testing error rate for lda: 0.2092
mean(lda.pred$class!=class.test)

## To apply QDA to the data set
qda.fit <- qda(class~.,data=train)
qda.fit
qda.pred <- predict(qda.fit,test)
names(qda.pred)
head(qda.pred$class)
table(qda.pred$class,class.test)
# This our testing error rate for qda: 0.212
mean(qda.pred$class!=class.test)