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

setwd("C:/Users/HP/Downloads/Research Project/Data")


#LOAD DATA
TN <- read.xlsx("TN_Chennai_Alandur.xlsx", 1)
TN = separate(TN, col = To.Date, into = c("Date"), sep = 10, remove=T)
#TN$Time<-NULL
TN$Date=strptime(TN$Date, format= "%d-%m-%Y")
TN$WS = as.character(TN$WS)
TN$WS = as.numeric(TN$WS)
#creating a zoo object
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
names(TN.df)[names(TN.df) == "TN_ag"] <- "y"
write.table(TN.df, "C:/Users/HP/Downloads/Research Project/Data/TN_all.csv", sep=" ,", row.names=FALSE)
autoplot(decompose(TN.ts))

autoplot(TN.ts) +
  ggtitle("Wind Speed data of Tamil Nadu(2016-2019)") +
  xlab("Year") +
  ylab("Meter/Second")

#Dickey-Fuller test for Stationarity
apply(TN.ts, 2, adf.test)


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
TNarima.acc<-as.data.frame(accuracy(TNarima.forecast))

#checkresiduals(TNarima)

as.numeric(TNarima.forecast$mean)
TNarima.res=TNarima$res
squared.res.TNarima=TNarima.res^2
par(mfcol=c(3,1))
plot(squared.res.TNarima,main='Squared Residuals')
acf.squared212=acf(squared.res.TNarima,main='ACF Squared
                   Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared212=pacf(squared.res.TNarima,main='PACF Squared
                     Residuals',lag.max=100,ylim=c(-0.5,1))



####################### Neural Net ########################

set.seed(18127355)
TNnn=nnetar(TN.ts,size =16,repeats=30, lambda = TNlambda, scale.inputs=TRUE)
TNnn.forecast=forecast(TNnn, h=180)
TNnn.acc<-data.frame(accuracy(TNnn.forecast))

autoplot(TNnn.forecast)


################### Simple Exponential Smoothening ####################

TNses <- ses(TN.ts, h = 180, lambda = TNlambda)

# checking the error rates to evaluate the model
#summary(TNses.forecast)
TNses.acc<-data.frame(accuracy(TNses))
checkresiduals(TNses)
# Add the one-step forecasts for the training data to the plot
autoplot(TNses) + autolayer(fitted(TNses))

########################## Prophet Model #############
library(Metrics)
TN.temporal <- read.csv("TN_all.csv")
#TN.temporal$ds=as.POSIXct(strptime(TN.temporal$ds, format= "%Y-%m-%d"))
TN.prophet<-prophet(TN.temporal[1:1203,], growth = 'linear', seasonality.mode = 'additive', daily.seasonality=F)
TN.prophetforecast <- predict(TN.prophet, TN.temporal[1204:1416,], type="response")

#TN.prophet<-prophet(TN.temporal, growth = 'linear', seasonality.mode = 'additive')
#TN.prophetforecast <- predict(TN.prophet, TN.temporal, type="response")

####### Errors
TNactual<- TN.temporal[1204:1416,2]

TNprophet.acc<-data.frame(0, rmse=rmse(TNactual, TN.prophetforecast$yhat), mae=mae(TNactual, TN.prophetforecast$yhat))

rmse(TNactual, TN.prophetforecast$yhat)
#mase(TNactual, TN.prophetforecast$yhat)
mae(TNactual, TN.prophetforecast$yhat)

summary(TN.prophet) # from here it can be seen that there are 25 times where there were sudden changes in the contineuty of the data
detach("package:Metrics", unload = TRUE)




############## MLP #################


TNmlp<-mlp(TN.ts,m=365, hd=16, sel.lag=T, reps=30)
TNmlp.forecast<-forecast(TNmlp,h=180)
autoplot(TNmlp.forecast)
TNmlp.acc<-data.frame(accuracy(TNmlp.forecast))



####################Dynamic Harmonic Regression###################

TNdhr<- auto.arima(TN.ts, xreg= fourier(TN.ts, K=3), seasonal = FALSE, lambda = 0)
summary(TNdhr) # checking the AICc value from summary of the fit model, the value of K is fixed with the minimum AICc value
TNdhr.forecast<-forecast(TNdhr, xreg= fourier(TN.ts, K=3, h=90))
TNdhr.acc<-accuracy(TNdhr.forecast)


############################################################################################################
############### Accuracy Table ######################
TamilNadu <- data.frame("Models" = c("ARIMA","NN", "SES", "PROPHET", "MLP"), "RMSE" =0, "MAE" =0)
TamilNadu[1,2:3] <- TNarima.acc[1,2:3]
TamilNadu[2,2:3] <- TNnn.acc[1,2:3]
#test<- c(TNarima.acc, TNnn.acc, TNses.acc)



  TamilNadu[1,2:3] <- TNarima.acc[1,2:3]
  TamilNadu[2,2:3] <- TNnn.acc[1,2:3]
  TamilNadu[3,2:3] <- TNses.acc[1,2:3]
  TamilNadu[4,2:3] <- TNprophet.acc[1,2:3]
  TamilNadu[5,2:3] <- TNmlp.acc[1,2:3]


######################################################################################################################
###################################################################################################################################
#############################################################################################################################
##########################################################################################################################
###########################################   Maharashtra     ##############################################################################
#################################################################################################################################
#########################################################################################################################
###############################################################################################################################
######################################################################################################################


MH <- read.xlsx("Maharashtra_Chandrapur.xlsx", 1)
MH = separate(MH, col = To.Date, into = c("Date"), sep = 10, remove=T)

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

autoplot(decompose(MH.ts))
autoplot(MH.ts) +
  ggtitle("Wind Speed data of Maharashtra(2016-2019)") +
  xlab("Year") +
  ylab("Meter/Second")

#Dickey-Fuller test for Stationarity
apply(MH.ts, 2, adf.test)


#From the pvalue, it can be seen that the data is not stationary
#install.packages("MTS")
#library(MTS)
#MH.ts= diff(MH.ts,2)


#differencing the time series data 2 times to make the data stationary
#Retest for stationarity
#apply(MH.ts, 2, adf.test)

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


MHarima <-auto.arima(ts(MH.ts,frequency=365), D=1, lambda=MHlambda)
MHarima.forecast <-forecast(MHarima,h=90)
autoplot(MHarima.forecast)
summary(MHarima)
MHarima.acc<-accuracy(MHarima)
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

MHdhr<- auto.arima(MH.ts, xreg= fourier(MH.ts, K=4), seasonal = FALSE, lambda = 0)
summary(MHdhr) # checking the AICc value from summary of the fit model, the value of K is fixed with the minimum AICc value
MHdhr.forecast<-forecast(MHdhr, xreg= fourier(MH.ts, K=5, h=90))
MHdhr.acc<-accuracy(MHdhr.forecast)



