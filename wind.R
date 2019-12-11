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
#install.packages("CombMSC")
#install.packages("ggpubr")
library(ggpubr)
library(CombMSC)
library(lattice)
library(prophet)
library(forecast)
library(tseries)
library(zoo)
library(ggplot2)
library(xlsx)
library(tidyr)
library(dplyr)
setwd("C:/Users/HP/Downloads/Research Project/Data")


#############################################################################################################################
##########################################################################################################################
###########################################    Maharashtra     ##############################################################################
#################################################################################################################################
#########################################################################################################################


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

MH.df <- as.data.frame(MH.ts)
names(MH.df)[1] <- "y"
write.table(MH.df, "C:/Users/HP/Downloads/Research Project/Data/MH_all.csv",
            sep=" ,", row.names=FALSE)

autoplot(decompose(MH.ts))
autoplot(MH.ts) +
  ggtitle("Wind Speed data of Maharashtra(2016-2019)") +
  xlab("Year") +
  ylab("Wind Speed(m/sec)")
+
#Dickey-Fuller test for Stationarity
apply(MH.ts, 2, adf.test)


#From the pvalue, it can be seen that the data is not stationary


#Ljung-Box Test for white noise
Box.test(MH.ts, lag = 24, fitdf = 0, type = "Ljung")
# The pvalue very less than 0.05 suggests that the data is not white noise
#spliting 75% of the original data for training the models
Maharashtra<-splitTrainTest(MH.ts, numTrain = length(MH.ts) - 354)
#checking normality
ggqqplot(MH.df$y)+
  ggtitle("Maharashtra Data Linearity")
#Shapiro-Wilk normality test
shapiro.test(MH.df$y)
##though from the qqplot the data seems a bit non-linear, but from the 
#p value <0.05 of Shapiro-Wilk normality test, it can be said that the data is normally distributed

#As the data is normally distributed, BoxCox transformation is not required


############################ ARIMA ######################################

MHarima <-auto.arima(Maharashtra$train,D=0,lambda=NULL, stationary = T,
                     stepwise=F, trace=F, approximation=F, biasadj=F)
checkresiduals(MHarima)

#by looking at the residuals, it can be seen that it is white noise, so the model is a fit
MHarima.forecast <-forecast(MHarima,h=354)
autoplot(MHarima.forecast) + ylab("Wind Speed(m/sec)")

MHarima.acc<-accuracy(MHarima.forecast)

####################### Neural Net ###################################################
set.seed(18127355)
MHnn=nnetar(Maharashtra$train,p=41,P=2,size =19,repeats=50,
            lambda = NULL, scale.inputs=F)

MHnn.forecast=forecast(MHnn, h=354)

MHnn.acc<-accuracy(MHnn.forecast)

autoplot(MHnn.forecast) + ylab("Wind Speed(m/sec)")


################### Simple Exponential Smoothening ##########################################

MHses <- ses(Maharashtra$train, h = 354, lambda = NULL, initial="optimal", biasadj=F)

# checking the error rates to evaluate the model

MHses.acc<-accuracy(MHses)

# Add the one-step forecasts for the training data to the plot
autoplot(Maharashtra$train) + autolayer(fitted(MHses), series = "ses") + ylab("Wind Speed(m/sec)")

########################## Prophet Model #############################################################
library(Metrics)
MH.temporal <- read.csv("MH_all.csv")

MH.prophet<-prophet(MH.temporal[1:1062,], changepoints = NULL, 
                    seasonality.mode = 'additive', daily.seasonality=F,fit = T)
MHfuture <- make_future_dataframe(MH.prophet, periods = 354)
MH.prophetforecast <- predict(MH.prophet, MHfuture, type="response")

dyplot.prophet(MH.prophet, MH.prophetforecast)

####### Errors
MHactual<- MH.temporal[1063:1416,2]

MHprophet.acc<-data.frame(rmse=rmse(MHactual, MH.prophetforecast$yhat),
                          mae=mae(MHactual, MH.prophetforecast$yhat))
detach("package:Metrics", unload = TRUE)


####################Dynamic Harmonic Regression###################

MHdhr<- auto.arima(Maharashtra$train, xreg= fourier(Maharashtra$train, K=1),
                   lambda = 0, stationary = T,stepwise=F, trace=F, approximation=F, biasadj=T)
summary(MHdhr) # checking for the minimum AICc value to determine the value of K
MHdhr.forecast<-forecast(MHdhr, xreg= fourier(MH.ts, K=1, h=354))
MHdhr.acc<-accuracy(MHdhr.forecast)
checkresiduals(MHdhr)
autoplot(MHdhr.forecast) + ylab("Wind Speed(m/sec)")

############################################################################################################
############### Accuracy Table ######################
MH.evaluation_table <- data.frame("Models" = c("ARIMA","NN", "SES", "PROPHET", "DHR"), "RMSE" =0, "MAE" =0)

MH.evaluation_table[1,2:3] <- MHarima.acc[1,c(2,3)]
MH.evaluation_table[2,2:3] <- MHnn.acc[1,c(2,3)]
MH.evaluation_table[3,2:3] <- MHses.acc[1,c(2,3)]
MH.evaluation_table[4,2:3] <- MHprophet.acc[1,1:2]
MH.evaluation_table[5,2:3] <- MHdhr.acc[1,c(2,3)]
write.table(MH.evaluation_table, "C:/Users/HP/Downloads/Research Project/Data/MH_accuracy.csv",
            sep=" ,", row.names=FALSE)



################################################################################################################################
######################################################################################################################
######################################## Tamil Nadu #################################################################
#######################################################################################################################
########################################################################################################################

#LOAD DATA
TN <- read.xlsx("TN_Chennai_Alandur.xlsx", 1)
TN = separate(TN, col = To.Date, into = c("Date"), sep = 10, remove=T) #tidyr

TN$Date=strptime(TN$Date, format= "%d-%m-%Y") #convert factor to POSIXlt time object
#convert factor to numeric
TN$WS = as.character(TN$WS)
TN$WS = as.numeric(TN$WS)
#creating a zoo object
temp= zoo(TN %>% select(2), order.by = TN$Date) #dplyr#zoo
#replacing the outliers and null values with locally smoothed values
temp$WS <- tsclean(temp$WS) #forecast

#agregating the hourly data to daily format in oder to make mid-term predictions successfully
TN_ag= aggregate(temp, as.Date, mean)
TN_ag[,1]=round(TN_ag[,1],digits = 2)

TN.ts <- ts(TN_ag, start= c(2016, 01, 01), frequency=365)
TN.df <- as.data.frame(TN_ag)
names(TN.df)[1] <- "y"
write.table(TN.df, "C:/Users/HP/Downloads/Research Project/Data/TN_all.csv",
            sep=" ,", row.names=FALSE)
autoplot(decompose(TN.ts))

autoplot(TN.ts) +
  ggtitle("Wind Speed data of Tamil Nadu(2016-2019)") +
  xlab("Year") +
  ylab("Meter/Second") #ggplot2

#Dickey-Fuller test for Stationarity
apply(TN.ts, 2, adf.test) #tseries


#Ljung-Box Test for white noise
Box.test(TN.ts, lag = 24, fitdf = 0, type = "Ljung")

# The pvalue very less than 0.05 suggests that the data is not white noise

#spliting 75% of the original data for training the models

Tamil<-splitTrainTest(TN.ts, numTrain = length(TN.ts) - 354) #CombMSC

#checking normality
ggqqplot(TN.df$y)+
  ggtitle("TamilNadu Data Linearity")
#Shapiro-Wilk normality test
shapiro.test(TN.df$y) 
##though from the qqplot the data seems a bit non-linear, but from the 
#p value <0.05 of Shapiro-Wilk normality test, it can be said that the data is normally distributed
#As the data is normally distributed, BoxCox transformation is not required


######################################## ARIMA ##########################################

TNarima <-auto.arima(Tamil$train,lambda=NULL,stationary=T, 
                     stepwise=F, trace=F, approximation=F, biasadj=F)
##Forecast with model
TNarima.forecast <-forecast(TNarima,h=354)
##Plot the forecast
autoplot(TNarima.forecast) + ylab("Wind Speed(m/sec)") 
##Accuracy store
TNarima.acc<-as.data.frame(accuracy(TNarima.forecast))
checkresiduals(TNarima)

#################################### Neural Net ###########################################

TNnn=nnetar(Tamil$train,p=41,P=2,size=19, repeats=50, lambda = NULL, scale.inputs=F)

TNnn.forecast=forecast(TNnn, PI=F,h=354)
TNnn.acc<-data.frame(accuracy(TNnn.forecast(m/sec)))

autoplot(TNnn.forecast) + ylab("Wind Speed")

########################## Simple Exponential Smoothening ################################

TNses <- ses(Tamil$train, h = 354, lambda = NULL, initial="optimal", biasadj=F)

# checking the error rates to evaluate the model
TNses.acc<-data.frame(accuracy(TNses))

# Add the one-step forecasts for the training data to the plot
autoplot(Tamil$train) + autolayer(fitted(TNses), series = "ses") + ylab("Wind Speed(m/sec)")

################################ Prophet Model ##############################################
library(Metrics)
TN.temporal <- read.csv("TN_all.csv")

TN.prophet<-prophet(TN.temporal[1:1062,], changepoints =c("2016-12-12"),
                    seasonality.mode = 'additive', daily.seasonality=F, fit = T)
TNfuture <- make_future_dataframe(TN.prophet, periods = 354)
TN.prophetforecast <- predict(TN.prophet, TNfuture, type="response")

dyplot.prophet(TN.prophet, TN.prophetforecast)

####### Errors

TNactual<- TN.temporal[1063:1416,2]


TNprophet.acc<-data.frame(rmse=rmse(TNactual, TN.prophetforecast$yhat),
                          mae=mae(TNactual, TN.prophetforecast$yhat))

detach("package:Metrics", unload = TRUE)


####################Dynamic Harmonic Regression###################

TNdhr<- auto.arima(Tamil$train, xreg= fourier(Tamil$train, K=3), lambda = 0, 
                   stepwise=F, trace=F, approximation=F, biasadj=F)

summary(TNdhr) # checking the AICc value from summary of the fit model, the value of K is fixed with the minimum AICc value
TNdhr.forecast<-forecast(TNdhr, xreg= fourier(TN.ts, K=3, h=354))
TNdhr.acc<-accuracy(TNdhr.forecast)

autoplot(TNdhr.forecast) + ylab("Wind Speed(m/sec)")

############################################################################################################
############### Accuracy Table ######################
TN.evaluation_table <- data.frame("Models" = c("ARIMA","NN", "SES", "PROPHET", "DHR"), "RMSE" =0, "MAE" =0)


TN.evaluation_table[1,2:3]<-TNarima.acc[1,c(2,3)]
TN.evaluation_table[2,2:3] <- TNnn.acc[1,c(2,3)]
TN.evaluation_table[3,2:3] <- TNses.acc[1,c(2,3)]
TN.evaluation_table[4,2:3] <- TNprophet.acc[1,1:2]
TN.evaluation_table[5,2:3] <- TNdhr.acc[1,c(2,3)]
write.table(TN.evaluation_table, "C:/Users/HP/Downloads/Research Project/Data/TN_accuracy.csv",
            sep=" ,", row.names=FALSE)


#################################################################################################################################################
##########################################################################################################################
######################################## Forecasting Wind power########################################################
###########################################################################################################################

#NN performed the best in handling both the location's wind speed data and forecasting it
#so now 180 days of future wind speed will be forecasted by NN

TNfit=nnetar(TN.ts,p=41,P=3,size = 19,repeats=50, lambda = NULL, scale.inputs=T)
TNWS.forecast=forecast(TNfit, h=180)


MHfit=nnetar(MH.ts,p=41,P=3,size =19 ,repeats=50, lambda = NULL, scale.inputs=TRUE)
MHWS.forecast=forecast(MHfit, h=180)

########################################################################################################################

##########Theoritically Calculating Wind Power with probable parameter values##################################

##############################################################################################################
#WP=0.5*Cp*q*A*V^3 
#[Cp=Max power coefficient(theoretical max-> 0.59)]
#[q=air density(1.225kg/m^3 at sea level)] 
#[A=Swept area of the rotor (largest rotor diameter used in the current time is 129m, so swept area=13070m^2) ]
#[V=Wind Speed(m/s), which in this case have been forecasted]
FutureWP<-data.frame("Tamil Nadu Wind"=((0.5 * 0.59 * 1.225 * 13070 * TNWS.forecast$mean^3)/1000), 
                     "Maharashtra Wind Power"=((0.5 * 0.59 * 1.225 * 13070 * MHWS.forecast$mean^3)/1000))

write.table(FutureWP, "C:/Users/HP/Downloads/Research Project/Data/Future_WP.csv", sep=" ,", row.names=FALSE)





