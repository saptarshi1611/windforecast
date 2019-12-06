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

#spliting 85% of the original data for training the models
TN.train<-window(TN.ts,start=2016,end=c(2019,109))
#TN.test<-window(TN.ts, start=c(2019,110))

#Storing the lambda value for boxcox transformation

TNlambda<-BoxCox.lambda(TN.train)


######################################## ARIMA ##########################################

TNarima <-auto.arima(TN.train,D=0,lambda=TNlambda,seasonal = F,stationary=T, 
                     stepwise=F, trace=F, approximation=F, biasadj=T)
##Forecast with model
TNarima.forecast <-forecast(TNarima,h=212)
##Plot the forecast
autoplot(TNarima.forecast)
##Accuracy store
TNarima.acc<-as.data.frame(accuracy(TNarima.forecast))


#################################### Neural Net ###########################################

set.seed(18127355)
TNnn=nnetar(TN.train,P=3,size =16,repeats=30, lambda = TNlambda, scale.inputs=T)
TNnn.forecast=forecast(TNnn, h=212)
TNnn.acc<-data.frame(accuracy(TNnn.forecast))

autoplot(TNnn.forecast)

########################## Simple Exponential Smoothening ################################

TNses <- ses(TN.train, h = 212, lambda = TNlambda, initial="optimal", biasadj=F)

# checking the error rates to evaluate the model
TNses.acc<-data.frame(accuracy(TNses))

# Add the one-step forecasts for the training data to the plot
autoplot(TN.train) + autolayer(fitted(TNses), series = "ses")

################################ Prophet Model ##############################################

library(Metrics)
TN.temporal <- read.csv("TN_all.csv")
TN.prophet<-prophet(TN.temporal[1:1204,], growth = 'linear', changepoints = NULL,
                    seasonality.mode = 'additive', daily.seasonality=F, fit = T)
TNfuture <- make_future_dataframe(TN.prophet, periods = 212)
TN.prophetforecast <- predict(TN.prophet, TNfuture, type="response")

dyplot.prophet(TN.prophet, TN.prophetforecast)

####### Errors

TNactual<- TN.temporal[1205:1416,2]

TNprophet.acc<-data.frame(rmse=rmse(TNactual, TN.prophetforecast$yhat), mae=mae(TNactual, TN.prophetforecast$yhat), mape=mape(TNactual, TN.prophetforecast$yhat))

detach("package:Metrics", unload = TRUE)


####################Dynamic Harmonic Regression###################

TNdhr<- auto.arima(TN.train, xreg= fourier(TN.train, K=3), seasonal = F, lambda = 0, 
                   stepwise=F, trace=F, approximation=F, biasadj=T)
summary(TNdhr) # checking the AICc value from summary of the fit model, the value of K is fixed with the minimum AICc value
TNdhr.forecast<-forecast(TNdhr, xreg= fourier(TN.ts, K=3, h=212))
TNdhr.acc<-accuracy(TNdhr.forecast)


############################################################################################################
############### Accuracy Table ######################
TN.evaluation_table <- data.frame("Models" = c("ARIMA","NN", "SES", "PROPHET", "DHR"), "RMSE" =0, "MAE" =0, "MAPE"=0)


TN.evaluation_table[1,2:4]<-TNarima.acc[1,c(2,3,5)]
TN.evaluation_table[2,2:4] <- TNnn.acc[1,c(2,3,5)]
TN.evaluation_table[3,2:4] <- TNses.acc[1,c(2,3,5)]
TN.evaluation_table[4,2:4] <- TNprophet.acc[1,1:3]
TN.evaluation_table[5,2:4] <- TNdhr.acc[1,c(2,3,5)]
write.table(TamilNadu, "C:/Users/HP/Downloads/Research Project/Data/TN_accuracy.csv", sep=" ,", row.names=FALSE)

######################################################################################################################
###################################################################################################################################
#############################################################################################################################
##########################################################################################################################
###########################################    Maharashtra     ##############################################################################
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


#Ljung-Box Test for white noise
Box.test(MH.ts, lag = 24, fitdf = 0, type = "Ljung")
# The pvalue very less than 0.05 suggests that the data is not white noise
#spliting 85% of the original data for training the models
MH.train<-window(MH.ts,start=2016,end=c(2019,109))
#Storing the lambda value for boxcox transformation
MHlambda<-BoxCox.lambda(MH.train)

MH.df <- as.data.frame(MH.ts)
write.table(MH.df, "C:/Users/HP/Downloads/Research Project/Data/MH_all.csv", sep=" ,", row.names=FALSE)



############################ ARIMA ######################################

MHarima <-auto.arima(MH.train,D=0,lambda=MHlambda, stationary = T,
                     seasonal = F, stepwise=F, trace=F, approximation=F, biasadj=F)
checkresiduals(MHarima)
#by looking at the residuals, it can be seen that it is white noise, so the model is a fit
MHarima.forecast <-forecast(MHarima,h=212)
autoplot(MHarima.forecast)
summary(MHarima)
MHarima.acc<-accuracy(MHarima.forecast)

####################### Neural Net ########################
set.seed(18127355)
MHnn=nnetar(MH.train,P=3,size =16,repeats=30, lambda = MHlambda, scale.inputs=TRUE)

MHnn.forecast=forecast(MHnn, h=212)

MHnn.acc<-accuracy(MHnn.forecast)

autoplot(MHnn.forecast)


################### Simple Exponential Smoothening ####################

MHses <- ses(MH.train, h = 212, lambda = MHlambda, initial="optimal", biasadj=F)

# checking the error rates to evaluate the model

MHses.acc<-accuracy(MHses)

# Add the one-step forecasts for the training data to the plot
autoplot(MH.train) + autolayer(fitted(MHses), series = "ses")
########################## Prophet Model #############
library(Metrics)
MH.temporal <- read.csv("MH_all.csv")

MH.prophet<-prophet(MH.temporal[1:1204,], growth = 'linear', changepoints = NULL, seasonality.mode = 'additive', daily.seasonality=F,fit = T)
MHfuture <- make_future_dataframe(MH.prophet, periods = 212)
MH.prophetforecast <- predict(MH.prophet, MHfuture, type="response")

dyplot.prophet(MH.prophet, MH.prophetforecast)

####### Errors
MHactual<- MH.temporal[1205:1416,2]

MHprophet.acc<-data.frame(rmse=rmse(MHactual, MH.prophetforecast$yhat), mae=mae(MHactual, MH.prophetforecast$yhat), mape=mape(MHactual, MH.prophetforecast$yhat))
detach("package:Metrics", unload = TRUE)


####################Dynamic Harmonic Regression###################

MHdhr<- auto.arima(MH.train, xreg= fourier(MH.train, K=1), seasonal = F, lambda = 0, stationary = T,
                  stepwise=F, trace=F, approximation=F, biasadj=T)
summary(MHdhr) # checking the AICc value from summary of the fit model, the value of K is fixed with the minimum AICc value
MHdhr.forecast<-forecast(MHdhr, xreg= fourier(MH.ts, K=1, h=212))
MHdhr.acc<-accuracy(MHdhr.forecast)

############################################################################################################
############### Accuracy Table ######################
MH.evaluation_table <- data.frame("Models" = c("ARIMA","NN", "SES", "PROPHET", "DHR"), "RMSE" =0, "MAE" =0, "MAPE"=0)

MH.evaluation_table[1,2:4] <- MHarima.acc[1,c(2,3,5)]
MH.evaluation_table[2,2:4] <- MHnn.acc[1,c(2,3,5)]
MH.evaluation_table[3,2:4] <- MHses.acc[1,c(2,3,5)]
MH.evaluation_table[4,2:4] <- MHprophet.acc[1,1:3]
MH.evaluation_table[5,2:4] <- MHdhr.acc[1,c(2,3,5)]
write.table(MH.evaluation_table, "C:/Users/HP/Downloads/Research Project/Data/MH_accuracy.csv", sep=" ,", row.names=FALSE)

#################################################################################################################################################
##########################################################################################################################
######################################## Forecasting Wind power

#NN performed the best in handling both the location's wind speed data and forecasting it
#so now 180 days of future wind speed will be forecasted by NN

TNfit=nnetar(TN.ts,P=3,size =16 ,repeats=30, lambda = TNlambda, scale.inputs=T)
TNWS.forecast=forecast(TNfit, h=180)

MHfit=nnetar(MH.ts,P=3,size =16 ,repeats=30, lambda = MHlambda, scale.inputs=TRUE)
MHWS.forecast=forecast(MHfit, h=180)

##########Theoritically Calculating Wind Power with probable parameter values
#WP=0.5*Cp*q*A*V^3 
#[Cp=Max power coefficient(theoretical max-> 0.59)]
#[q=air density(1.25kg/m^3 at sea level)] 
#[A=Swept area of the rotor (largest rotor used in India for commercial onshore turbines are of 60m, so swept area=12470m^2) ]
#[V=Wind Speed(m/s), which in this case have been forecasted]
FutureWP<-data.frame("TNWP"=(0.5 * 0.59 * 1.25 * 2827.44* TNWS.forecast$mean^3), "MHWP"=(0.5 * 0.59 * 1.25 * 2827.44 * MHWS.forecast$mean^3))

write.table(FutureWP, "C:/Users/HP/Downloads/Research Project/Data/Future_WP.csv", sep=" ,", row.names=FALSE)

#autoplot(as.ts(FutureWP, start=c(2019, 11, 17), frequency=365))+ggtitle("Future Wind Power Produced in the 2 States") +xlab("Time") +ylab("MW")

