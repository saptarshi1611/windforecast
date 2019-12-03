#install.packages("forecast")
#install.packages("chron")
#install.packages("tseries")
#install.packages("lubridate")
#install.packages("zoo")
#install.packages("lattice")
#install.packages("ggplot2")
#install.packages('prophet', type="source")
#install.packages("nnfor")
#install.packages("Metrics")
library(nnfor)
library(prophet)
library(forecast)
library(chron)
library(tseries)
library(lubridate)
library(zoo)
library(lattice)
library(ggplot2)
library(xlsx)
library(tidyr)
library(dplyr)
library(Metrics)
setwd("C:/Users/HP/Downloads/Research Project/Data")


#LOAD DATA
TN <- read.xlsx("TN_Chennai_Alandur.xlsx", 1)
TN = separate(TN, col = To.Date, into = c("Date", "Time"), sep = 10, remove=T)
TN$Time<-NULL
TN$Date=strptime(TN$Date, format= "%d-%m-%Y")
TN$WS = as.character(TN$WS)
TN$WS = as.numeric(TN$WS)

#TN1718 <- read.xlsx("TN_WS_1718.xlsx", 1)
#TN19 <- read.xlsx("TN_WS_2019.xlsx", 1)
#TN16 <- read.xlsx("TN_WS_2016.xlsx", 1)
#DROP UNNECESSARY COLUMN
#TN1718$From.Date<-NULL
#SEPARATE DATE AND TIME
#TN_split = separate(TN1718, col = To.Date, into = c("Date", "Time"), sep = 10, remove=T)
#TN_split$Time<-NULL

#Converting the date column from character to required time format
#TN_split$Date=strptime(TN_split$Date, format= "%d-%m-%Y")
#combining all 3 datasets into 1
#TN<-rbind(TN16, TN_split, TN19)

#TN$WS = as.character(TN$WS)
#TN$WS = as.numeric(TN$WS)


#TN$Date=as.POSIXct(strptime(TN$Date, format= "%Y-%m-%d"))

#converting the 5 value columns from factor to numeric
#TN$WS = as.character(TN$WS)
#TN$WS = as.numeric(TN$WS)

#TN_split$WD = as.character(TN_split$WD)
#TN_split$WD = as.numeric(TN_split$WD)
#TN_split$Temp = as.character(TN_split$Temp)
#TN_split$Temp = as.numeric(TN_split$Temp)
#TN_split$BP<-NULL
#TN_split$WD<-NULL
#TN_split$Temp<-NULL
#TN_split$RH<-NULL
#TN_split$BP = as.character(TN_split$BP)
#TN_split$BP = as.numeric(TN_split$BP)
#TN_split$RH = as.character(TN_split$RH)
#TN_split$RH = as.numeric(TN_split$RH)
#converting the dataframe into a zoo object
temp= zoo(TN %>% select(2), order.by = TN$Date)
#replacing the outliers and null values with locally smoothed values
temp$WS <- tsclean(temp$WS)
#temp$WD <- tsclean(temp$WD)
#temp$Temp <- tsclean(temp$Temp)
#temp$BP <- tsclean(temp$BP)
#temp$RH <- tsclean(temp$RH)
#agregating the hourly data to daily format in oder to make mid-term predictions successfully
TN_ag= aggregate(temp, as.Date, mean)
TN_ag[,1]=round(TN_ag[,1],digits = 2)

TN.ts <- ts(TN_ag, start= c(2016, 01, 01), frequency=365)
TN.df <- as.data.frame(TN_ag)
write.table(TN.df, "C:/Users/HP/Downloads/Research Project/Data/TN_all.csv", sep=" ,", row.names=FALSE)
autoplot(decompose(TN.ts))

#plot(TN_aggregate)
#TN_aggregate<- as.ts(TN_aggr)

#Dickey-Fuller test for Stationarity
apply(TN.ts, 2, adf.test)

#As Temp, BP, RH are found to be non-stationary, so they are differenced
#install.packages("MTS")
#library(MTS)
#TN_aggregate_st= diffM(TN_aggregate)

#Retest for stationarity
#apply(TN_aggregate_st, 2, adf.test)

#autoplot(decompose(TN_aggregate))

#Ljung-Box Test for white noise
Box.test(TN.ts, lag = 24, fitdf = 0, type = "Ljung")


# The pvalue very less than 0.05 suggests that the data is not white noise

ggAcf(TN.ts)
#The ACF plot confirms the same

#Storing the lambda value for boxcox transformation
TNlambda<-BoxCox.lambda(TN.ts)

############################ ARIMA ######################################

TNarima <-auto.arima(ts(TN.ts,frequency=365),D=0,lambda=TNlambda)
TNarima.forecast <-forecast(TNarima,h=180)
autoplot(TNarima.forecast)
#write.table(forecastedvalue, "C:/Users/nain/Documents/ARIMApredictedvalue.csv", sep=" ,", row.names=FALSE)
TNarima.acc<-accuracy(TNarima.forecast)

checkresiduals(TNarima)

as.numeric(TNarima.forecast$mean)
TNarima.res=TNarima$res
squared.res.TNarima=TNarima.res^2
par(mfcol=c(3,1))
plot(squared.res.TNarima,main='Squared Residuals')
acf.squared212=acf(squared.res.TNarima,main='ACF Squared
                   Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared212=pacf(squared.res.TNarima,main='PACF Squared
                     Residuals',lag.max=100,ylim=c(-0.5,1))

#################### Recurrent Neural Network ########################
#install.packages("rnn")
#library(rnn)
#library(quantmod)

#TN.temporal <- read.csv("TN_all.csv")


#x=TN.temporal[,1]

#y=TN.temporal[,2]

#X=matrix(x, nrow=15)
#Y=matrix(y, nrow=15)

#X=na.omit(X)
#Y=na.omit(Y)
#standardize in the interval 0 - 1
#Yscaled= (Y - min(Y))/(max(Y) - min(Y))
#Y=t(Yscaled) # taking the transpose of Y
#train= 1:871
#test= 871:1245

#TN.rnn= trainr(temp,learningrate = 0.05,numepochs = 1000)


####################### Neural Net ########################

set.seed(18127355)
TNnn=nnetar(TN.ts, size = 20, repeats=30, lambda = TNlambda, scale.inputs=TRUE)

TNnn.forecast=forecast(TNnn, h=180)
TNnn.acc<-accuracy(TNnn.forecast)

autoplot(TNnn.forecast)


################### Simple Exponential Smoothening ####################

TNses <- ses(TN.ts, h = 180, lambda = TNlambda)

# checking the error rates to evaluate the model
#summary(TNses.forecast)
TNses.acc<-accuracy(TNses)

# Add the one-step forecasts for the training data to the plot
autoplot(TNses) + autolayer(fitted(TNses))

########################## Prophet Model #############

TN.temporal <- read.csv("TN_all.csv")
#TN.temporal$ds=as.POSIXct(strptime(TN.temporal$ds, format= "%Y-%m-%d"))
TN.prophet<-prophet(TN.temporal[1:1203,], growth = 'linear', seasonality.mode = 'additive', daily.seasonality=F)
TN.prophetforecast <- predict(TN.prophet, TN.temporal[1204:1416,], type="response")

#TN.prophet<-prophet(TN.temporal, growth = 'linear', seasonality.mode = 'additive')
#TN.prophetforecast <- predict(TN.prophet, TN.temporal, type="response")

####### Errors
TNactual<- TN.temporal[1204:1416,2]


rmse(TNactual, TN.prophetforecast$yhat)
mase(TNactual, TN.prophetforecast$yhat)
mae(TNactual, TN.prophetforecast$yhat)

summary(TN.prophet) # from here it can be seen that there are 25 times where there were sudden changes in the contineuty of the data
#TN.future <- make_future_dataframe(TN.prophet, periods = 90)
#TN.prophetforecast <- predict(TN.prophet, TN.future)
#plot(TN.prophet, TN.prophetforecast)

#TN.prophetcv<- cross_validation(TN.prophet,horizon = 90, units = 'days')
#tail(TN.prophetcv)
#TN.prophetaccuracy<- performance_metrics(TN.prophetcv)
#tail(TN.prophetaccuracy)
#mean(TN.prophetaccuracy$rmse)
#mean(TN.prophetaccuracy$mape)




############## MLP #################


TNmlp<-mlp(TN.ts, hd=20, sel.lag=T,retrain = T, reps=30)
TNmlp.forecast<-forecast(TNmlp,h=180)
autoplot(TNmlp.forecast)
TNmlp.acc<-accuracy(TNmlp.forecast)



####################Dynamic Harmonic Regression###################

TNdhr<- auto.arima(TN.ts, xreg= fourier(TN.ts, K=3), seasonal = FALSE, lambda = 0)
summary(TNdhr) # checking the AICc value from summary of the fit model, the value of K is fixed with the minimum AICc value
TNdhr.forecast<-forecast(TNdhr, xreg= fourier(TN.ts, K=3, h=90))
TNdhr.acc<-accuracy(TNdhr.forecast)


############################################################################################################
############### Accuracy Table ######################

TamilNadu <- data.frame("Models" = c("ARIMA","NN", "SES", "PROPHET", "MLP"), "RMSE" =0, "MAE" =0, "MASE"=0)
TamilNadu[1,2:3] <- TNarima.acc[1,2:4]

TNarima.acc<-accuracy(TNarima.forecast)

######################################################################################################################
###################################################################################################################################
#############################################################################################################################
##########################################################################################################################
###########################################   Maharashtra     ##############################################################################
#################################################################################################################################
#########################################################################################################################
###############################################################################################################################
######################################################################################################################


MH <- read.xlsx("Maharashtra_Pune_Karve.xlsx", 1)
MH = separate(MH, col = To.Date, into = c("Date", "Time"), sep = 10, remove=T)
MH$Time<-NULL
MH$Date=strptime(MH$Date, format= "%d-%m-%Y")
MH$WS = as.character(MH$WS)
MH$WS = as.numeric(MH$WS)

temp1= zoo(MH %>% select(2), order.by = MH$Date)
#replacing the outliers and null values with locally smoothed values
temp1$WS <- tsclean(temp1$WS)
#agregating the hourly data to daily format in oder to make mid-term predictions successfully
MH_ag= aggregate(temp1, as.Date, mean)
MH_ag[,1]=round(MH_ag[,1],digits = 2)

MH.ts <- ts(MH_ag, start= c(2016, 01, 01),  frequency=365)

#MH.df <- as.data.frame(MH_ag)
#write.table(MH.df, "C:/Users/HP/Downloads/Research Project/Data/MH_all.csv", sep=" ,", row.names=FALSE)
autoplot(decompose(MH.ts))
#Dickey-Fuller test for Stationarity
apply(MH.ts, 2, adf.test)


#From the pvalue, it can be seen that the data is not stationary
#install.packages("MTS")
#library(MTS)
MH.ts= diff(MH.ts,2)


#differencing the time series data 2 times to make the data stationary
#Retest for stationarity
apply(MH.ts, 2, adf.test)

#autoplot(decompose(TN_aggregate))

#Ljung-Box Test for white noise
Box.test(MH.ts, lag = 24, fitdf = 0, type = "Ljung")


# The pvalue very less than 0.05 suggests that the data is not white noise

ggAcf(MH.ts)
#The ACF plot confirms the same

#Storing the lambda value for boxcox transformation
MHlambda<-BoxCox.lambda(MH.ts)

MH.df <- as.data.frame(MH.ts)
write.table(MH.df, "C:/Users/HP/Downloads/Research Project/Data/MH_all.csv", sep=" ,", row.names=FALSE)


############################ ARIMA ######################################


MHarima <-auto.arima(ts(MH.ts,frequency=365),D=0,lambda=MHlambda)
MHarima.forecast <-forecast(MHarima,h=90)
autoplot(MHarima.forecast)

MHarima.acc<-accuracy(MHarima.forecast)
as.numeric(MHarima.forecast$mean)
MHarima.res=MHarima$res
squared.res.MHarima=MHarima.res^2
par(mfcol=c(3,1))
plot(squared.res.MHarima,main='Squared Residuals')
acf.squared212=acf(squared.res.MHarima,main='ACF Squared
                   Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared212=pacf(squared.res.MHarima,main='PACF Squared
                     Residuals',lag.max=100,ylim=c(-0.5,1))




####################### Neural Net ########################


MHnn=nnetar(MH.ts, size = 20, repeats=30, lambda = MHlambda, scale.inputs=TRUE)

MHnn.forecast=forecast(MHnn, h=90)
MHnn.acc<-accuracy(MHnn.forecast)

autoplot(MHnn.forecast)


################### Simple Exponential Smoothening ####################

MHses <- ses(MH.ts, h = 90, lambda = MHlambda)

# checking the error rates to evaluate the model
#summary(TNses.forecast)
MHses.acc<-accuracy(MHses)

# Add the one-step forecasts for the training data to the plot
autoplot(MHses) + autolayer(fitted(MHses))

########################## Prophet Model #############

MH.temporal <- read.csv("MH_all.csv")
#TN.temporal$ds=as.POSIXct(strptime(TN.temporal$ds, format= "%Y-%m-%d"))
MH.prophet<-prophet(MH.temporal[1:1155,], growth = 'linear', seasonality.mode = 'additive', daily.seasonality=F)
MH.prophetforecast <- predict(MH.prophet, MH.temporal[1156:1359,], type="response")

#TN.prophet<-prophet(TN.temporal, growth = 'linear', seasonality.mode = 'additive')
#TN.prophetforecast <- predict(TN.prophet, TN.temporal, type="response")

####### Errors
MHactual<- MH.temporal[1156:1359,2]


rmse(MHactual, MH.prophetforecast$yhat)
#mase(MHactual, MH.prophetforecast$yhat)
mae(MHactual, MH.prophetforecast$yhat)

rsq <- function (x, y) cor(x, y) ^ 2

rsq(MHactual, MH.prophetforecast$yhat)

summary(MH.prophet) # from here it can be seen that there are 25 times where there were sudden changes in the contineuty of the data


############## MLP #################

MHmlp<-mlp(MH.ts, hd=20, sel.lag=T,retrain = T)
MHmlp.forecast<-forecast(fit3,h=90)
autoplot(MHmlp.forecast)
MHmlp.acc<-accuracy(MHmlp.forecast)

####################Dynamic Harmonic Regression###################


#TNdhr<- auto.arima(TN.ts, xreg= fourier(TN.ts, K=3), seasonal = FALSE, lambda = 0)
test<-MH.ts
test[,1]<-(test[,1]+1)

MHdhr<- auto.arima(MH.ts, xreg= fourier(MH.ts, K=1), seasonal = FALSE, lambda = 0)
MHdhr<- auto.arima(test, xreg= fourier(test, K=5), seasonal = FALSE, lambda = 0)
summary(MHdhr) # checking the AICc value from summary of the fit model, the value of K is fixed with the minimum AICc value
MHdhr.forecast<-forecast(MHdhr, xreg= fourier(MH.ts, K=5, h=90))
MHdhr.acc<-accuracy(MHdhr.forecast)



