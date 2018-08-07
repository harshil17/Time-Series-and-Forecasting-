
library(forecast)
library(gtrendsR)
library(zoo)
library(bindrcpp)

getwd()
setwd("E:/book/Pridictive_analysis")
mydata <- read.csv("mydata.csv", sep = ",")
head(mydata)
str(mydata)
summary(mydata)
library(dplyr)
mydata_train<-filter(mydata, date<=201612)
mydata_train
mydata_test<-filter(mydata, date>201612)
mydata_test
train_split<- split(mydata, list(mydata$daypart, mydata$network), drop=1)
train_split 
test_split<-split(mydata, list(mydata$daypart, mydata$network), drop = 1)
test_split

train_lapply<- lapply(train_split, function(x) x[c(4)])
train_lapply
train_ts <- lapply(train_lapply, function(x) ts(x, start = c(2014,1), frequency = 12 ))
train_ts
test_lapply <- lapply(test_split, function(x) x[c(4)])
test_lapply
test_ts<- lapply(test_lapply, function(x) ts(x, start = c(2017,1), frequency = 12 ))
test_ts

decomposition<- lapply(train_ts, function(x) decompose(x))
stl_decomposition<- lapply(decomposition, function(x)plot(x, col= "red"))
ACF_decomposition<- lapply(train_ts, function(x)acf(x, ylim = c(-0.1,2), col="red", main="ACF Decomposition"))
Pacf_decompostion<- lapply(train_ts, function(x)pacf(x, ylim = c(-0.1,2), col="red", main= "PACF Decomposition"))

# Exponential Smoothing using Holt-Winter 
fit <- lapply(train_ts, function(x)HoltWinters(x, alpha = 0.02, beta = 0.1, gamma = 0.1))
fit

#Test prediction

pred_test <- lapply(fit, function(x)predict(x, n.ahead=12))
pred_test
class(pred_test)

#Changing class of time series to evaluate MAE
pred_num <- lapply(pred_test, function(x)as.numeric(x[,1]))
pred_num
actual_test <- lapply(test_ts, function(x)as.numeric(x[,1])) 
actual_test

#MAE Calculation

MAE <- mapply(function(x,y)mean(abs(x-y)), x=pred_num, y=test_ts)
MAE

#Making predictions by using comparision of fit with Anonymous parameter 
mod<-lapply(train_ts, function(x) HoltWinters(x))
mod
pred_acc<-lapply(mod, function(x) predict(x, n.ahead=12))
pred_acc
prediction_acc<-lapply(pred_acc, function(x) as.numeric(x[,1]))
prediction_acc

#Measuring MAE with anonamous model
MAE_acc<-mapply(function(x,y) mean(abs(x-y)), x=prediction_acc, y=actual_test)
MAE_acc

length(pred_num)
length(test)
length(plt)
pred_num
test
