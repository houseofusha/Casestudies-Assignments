
library(dplyr)
library(forecast)
library(tseries)
library(ggplot2)
require(cowplot)
library(plotrix)
library(gridExtra)
library(graphics)

Global.Superstore <- read.csv("Global Superstore.csv",stringsAsFactors = F)
View(Global.Superstore)

str(Global.Superstore)
summary(Global.Superstore)


##Data Cleaning & Preperation

#the required columns for analysis are order date, segment, market, sales quatity & profit
global.superstore<-Global.Superstore[, c(3, 8, 13, 19, 20, 22) ]

#checking for duplicate rows before we omit the IDs
sum(duplicated(global.superstore$Row.Id))

#checking for blank values
sapply(global.superstore, function(x) length(which(x == "")))

#checking for NA values 
sapply(global.superstore,function(x) sum(is.na(x)))


#formatting the order date to common format
global.superstore$Order.Date <- as.Date(global.superstore$Order.Date,"%d-%m-%Y")
str(global.superstore)


#exploratory data analysis

#extracting month and year from the "Order.Date" column
global.superstore$Order.Month <- as.numeric(format(as.POSIXct(global.superstore$Order.Date,format="%d-%m-%Y"),"%m"))
global.superstore$Order.Year <- as.numeric(format(as.POSIXct(global.superstore$Order.Date,format="%d-%m-%Y"),"%Y"))

## Grouping data
segmented.data <- global.superstore %>% group_by(Market,Segment,Order.Year,Order.Month) %>%summarise(sum(Sales),sum(Quantity),sum(Profit))
colnames(segmented.data)[4:7] <- c("Month","Sales","Quantity","Profit")
View(segmented.data)

#calculating cofficient of variation(CV) for the 21 buckets
segments.21 <- segmented.data %>% group_by(Market,Segment)%>%summarise(sd(Profit)/mean(Profit))
colnames(segments.21)[3] <- "CV"
View(segments.21)

top.2 <- as.data.frame(head(segments.21[order(segments.21$CV, decreasing = FALSE), ],2))
top.2

#Market  Segment        CV
#1     EU Consumer 0.6243052
#2   APAC Consumer 0.6321323

#checking for the top two most profitable segments
profit.21 <- segmented.data %>% group_by(Market,Segment)%>%summarise(sum(Profit))

top.2.profitable <- as.data.frame(head(profit.21[order(profit.21$`sum(Profit)`, decreasing = TRUE), ],2))
top.2.profitable

#Market  Segment sum(Profit)
#1   APAC Consumer    222817.6
#2     EU Consumer    188687.7

##therefore, top 2 segments are APAC-Consumer & EU-Consumer based on cofficient of variation and total profit

#segmenting the data of APAC-Consumer & EU-Consumer 

segmented.data <- as.data.frame(segmented.data)
segmented.APAC.Consumer <- filter(segmented.data, Market == "APAC" & Segment == "Consumer")
segmented.EU.Consumer <- filter(segmented.data, Market == "EU" & Segment == "Consumer")

View(segmented.APAC.Consumer)
View(segmented.EU.Consumer)

#plotting the two segments to check increasing trend for profit
plot_grid(ggplot(segmented.APAC.Consumer, aes(x=Month, y=Profit)) + geom_line()+ geom_smooth(method = "lm"),
          ggplot(segmented.EU.Consumer, aes(x=Month, y=Profit)) + geom_line()+ geom_smooth(method = "lm"), align = "h")

#removing unwanted columns from the two finalised segments
segmented.APAC.Consumer$Month <- c(1:48)
segmented.APAC.Consumer <- segmented.APAC.Consumer[,4:6]

segmented.EU.Consumer$Month <- c(1:48)
segmented.EU.Consumer <- segmented.EU.Consumer[,4:6]


#purpose of analysis is to pridict sales and quantity for the two segments
APAC.Consumer.Sales <- as.data.frame(segmented.APAC.Consumer[,c("Month","Sales")])
APAC.Consumer.Quantity <- as.data.frame(segmented.APAC.Consumer[,c("Month","Quantity")])
EU.Consumer.Sales <- as.data.frame(segmented.EU.Consumer[,c("Month","Sales")])
EU.Consumer.Quantity <- as.data.frame(segmented.EU.Consumer[,c("Month","Quantity")])



#############################forecasting APAC Consumer Sales

#creating time stamp for the APAC Consumer Sales data
total.APAC.Consumer.Sales.timeser <- ts(segmented.APAC.Consumer$Sales)

#creating the model using the first 42 row values and then test the model on the remaining 6 row values 
APAC.Consumer.Sales.indata <- APAC.Consumer.Sales[1:42,]
APAC.Consumer.Sales.timeser <- ts(APAC.Consumer.Sales.indata$Sales)

## Plotting the APAC consumer Sales timeseries
plot(APAC.Consumer.Sales.timeser)


#smoothing the series - moving average smoothing with width=1
w <- 1
APAC.Consumer.Sales.smoothedseries <- stats::filter(APAC.Consumer.Sales.timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#smoothing the left end of the time series
difference <- APAC.Consumer.Sales.smoothedseries[w+2] - APAC.Consumer.Sales.smoothedseries[w+1]
for (i in seq(w,1,-1)) {APAC.Consumer.Sales.smoothedseries[i] <- APAC.Consumer.Sales.smoothedseries[i+1] - difference}

#smoothing right end of the time series
n <- length(APAC.Consumer.Sales.timeser)
differ <- APAC.Consumer.Sales.smoothedseries[n-w] - APAC.Consumer.Sales.smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC.Consumer.Sales.smoothedseries[i] <- APAC.Consumer.Sales.smoothedseries[i-1] + differ
}

#plotting the smoothed time series
APAC.Consumer.Sales.timevals <- APAC.Consumer.Sales.indata$Month
plot(APAC.Consumer.Sales.smoothedseries, col="blue", lwd=2)



##building a model on the smoothed time series using classical decomposition method

#converting the time series to a dataframe
APAC.Consumer.Sales.df <- as.data.frame(cbind(APAC.Consumer.Sales.timevals, as.vector(APAC.Consumer.Sales.smoothedseries)))
colnames(APAC.Consumer.Sales.df) <- c('Month', 'Sales')

#fitting a multiplicative model with trend and seasonality to the data, 
#seasonality will be modeled using a sinusoid function
APAC.Consumer.Sales.lmfit <- lm(Sales ~ sin(1.0*Month) * poly(Month,3) + cos(0.473*Month) * poly(Month,2) + Month, data=APAC.Consumer.Sales.df)

#predicting the global values of the series - Seasonality and trend
APAC.Consumer.Sales.global.pred <- predict(APAC.Consumer.Sales.lmfit, Month=APAC.Consumer.Sales.timevals)
summary(APAC.Consumer.Sales.global.pred)

#plotting the globally predictable part
plot(APAC.Consumer.Sales.timevals, APAC.Consumer.Sales.global.pred, col='red', lwd=2)

#looking at the locally predictable series, we model it through ARMA series
APAC.Consumer.Sales.local.pred <- APAC.Consumer.Sales.timeser-APAC.Consumer.Sales.global.pred

#plotting the local component of the series
plot(APAC.Consumer.Sales.local.pred, col='red', type = "l")

#checking the Autocorrelation for the observations
acf(APAC.Consumer.Sales.local.pred)
acf(APAC.Consumer.Sales.local.pred, type="partial")

#modelling the local component using ARMA or auto.arima function
APAC.Consumer.Sales.armafit <- auto.arima(APAC.Consumer.Sales.local.pred)
tsdiag(APAC.Consumer.Sales.armafit)
APAC.Consumer.Sales.armafit

#checking if the residual series is white noise
APAC.Consumer.Sales.resi <- APAC.Consumer.Sales.local.pred-fitted(APAC.Consumer.Sales.armafit)

#running adf and kpss tests to confirm weather the residual series is stationary
adf.test(APAC.Consumer.Sales.resi,alternative = "stationary")
kpss.test(APAC.Consumer.Sales.resi)

#evaluating the model using MAPE

#########making a prediction for the last 6 months
APAC.Consumer.Sales.fcast <- APAC.Consumer.Sales[43:48,]
APAC.Consumer.Sales.timevals.out <- APAC.Consumer.Sales.fcast$Month

#predicting the six month values by "lmfit" as ARMA component is absent
APAC.Consumer.Sales.global.pred.out <- predict((APAC.Consumer.Sales.lmfit), data.frame(Month = APAC.Consumer.Sales.timevals.out))
APAC.Consumer.Sales.fcast <- APAC.Consumer.Sales.global.pred.out  # Predicted values

#compare our prediction with the actual values, using MAPE
APAC.Consumer.Sales.MAPE.class.dec <- accuracy(APAC.Consumer.Sales.fcast,APAC.Consumer.Sales.fcast[2])[5]
APAC.Consumer.Sales.MAPE.class.dec  #17.75247

#plotting the predictions along with original values to get a visuals of the fit
APAC.Consumer.Sales.class.dec.pred <- c(ts(APAC.Consumer.Sales.global.pred),ts(APAC.Consumer.Sales.global.pred.out))
plot(total.APAC.Consumer.Sales.timeser, col = "black")
lines(APAC.Consumer.Sales.class.dec.pred, col = "red")


##building the model using ARIMA

APAC.Consumer.Sales.auto.arima <- auto.arima(APAC.Consumer.Sales.timeser)
APAC.Consumer.Sales.auto.arima

#timeseries diagram for ARIMA model
tsdiag(APAC.Consumer.Sales.auto.arima)
plot(APAC.Consumer.Sales.auto.arima$x, col="black")
lines(fitted(APAC.Consumer.Sales.auto.arima), col="red")

#checking if the residual series is white noise
APAC.Consumer.Sales.resi.auto.arima <- APAC.Consumer.Sales.timeser - fitted(APAC.Consumer.Sales.auto.arima)

#running adf and kpss test to confirm weather the residual series is stationary
adf.test(APAC.Consumer.Sales.resi.auto.arima,alternative = "stationary")
kpss.test(APAC.Consumer.Sales.resi.auto.arima)

#eEvaluating the model using MAPE
APAC.Consumer.Sales.fcast.auto.arima <- predict(APAC.Consumer.Sales.auto.arima, n.ahead = 6)
APAC.Consumer.Sales.fcast.auto.arima$pred  # Predicted values

APAC.Consumer.Sales.MAPE.auto.arima <- accuracy(APAC.Consumer.Sales.fcast.auto.arima$pred,APAC.Consumer.Sales.fcast[2])[5]
APAC.Consumer.Sales.MAPE.auto.arima  #27.68952

#plotting the predictions along with original values to get a visuals of the fit
APAC.Consumer.Sales.auto.arima.pred <- c(fitted(APAC.Consumer.Sales.auto.arima),ts(APAC.Consumer.Sales.fcast.auto.arima$pred))
plot(total.APAC.Consumer.Sales.timeser, col = "black")
lines(APAC.Consumer.Sales.auto.arima.pred, col = "red")


#################################forecasting APAC Consumer Quantity

#creating the model using the first 42 rows and then we shall test the model on the remaining 6 rows later
#converting to time series
total.APAC.Consumer.Quantity.timeser <- ts(APAC.Consumer.Quantity$Quantity)
APAC.Consumer.Quantity.indata <- APAC.Consumer.Quantity[1:42,]
APAC.Consumer.Quantity.timeser <- ts(APAC.Consumer.Quantity.indata$Quantity)

#plotting the APAC consumer Sales timeseries
plot(APAC.Consumer.Quantity.timeser)


#smoothing the series - Moving Average Smoothing, with width=1
w <-1
APAC.Consumer.Quantity.smoothedseries <- stats::filter(APAC.Consumer.Quantity.timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

## Smoothing left end of the time series
diff <- APAC.Consumer.Quantity.smoothedseries[w+2] - APAC.Consumer.Quantity.smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  APAC.Consumer.Quantity.smoothedseries[i] <- APAC.Consumer.Quantity.smoothedseries[i+1] - diff
}

## Smoothing right end of the time series
n <- length(APAC.Consumer.Quantity.timeser)
diffin <- APAC.Consumer.Quantity.smoothedseries[n-w] - APAC.Consumer.Quantity.smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC.Consumer.Quantity.smoothedseries[i] <- APAC.Consumer.Quantity.smoothedseries[i-1] + diffin
}

#plot the smoothed time series
APAC.Consumer.Quantity.timevals <- APAC.Consumer.Quantity.indata$Month
lines(APAC.Consumer.Quantity.smoothedseries, col="blue", lwd=2)

#building a model on the smoothed time series using classical decomposition

#converting the time series to a dataframe
APAC.Consumer.Quantity.df <- as.data.frame(cbind(APAC.Consumer.Quantity.timevals, as.vector(APAC.Consumer.Quantity.smoothedseries)))
colnames(APAC.Consumer.Quantity.df) <- c('Month', 'Sales')

#fitting a multiplicative model with trend and seasonality to the data, Seasonality will be modeled using a sinusoid function
APAC.Consumer.Quantity.lmfit <- lm(Sales ~ sin(1.0*Month) * poly(Month,3) + cos(0.469*Month) * poly(Month,2) + Month, data=APAC.Consumer.Quantity.df)
APAC.Consumer.Quantity.lmfit
#0.469

#predicting the global values of the series i.e Seasonality and trend
APAC.Consumer.Quantity.global.pred <- predict(APAC.Consumer.Quantity.lmfit, Month=APAC.Consumer.Quantity.timevals)
summary(APAC.Consumer.Quantity.global.pred)

#plotting the globally predictable part
lines(APAC.Consumer.Quantity.timevals, APAC.Consumer.Quantity.global.pred, col='red', lwd=2)

#the locally predictable series, we model it through ARMA series
APAC.Consumer.Quantity.local.pred <- APAC.Consumer.Quantity.timeser - APAC.Consumer.Quantity.global.pred

#plotting the local component of the series
plot(APAC.Consumer.Quantity.local.pred, col='red', type = "l")

#checking the Autocorrelation for the observations
acf(APAC.Consumer.Quantity.local.pred)
acf(APAC.Consumer.Quantity.local.pred, type="partial")

#modelling the local component using ARMA or auto.arima function
APAC.Consumer.Quantity.armafit <- auto.arima(APAC.Consumer.Quantity.local.pred)
tsdiag(APAC.Consumer.Quantity.armafit)
APAC.Consumer.Quantity.armafit

#checking if the residual series is white noise
APAC.Consumer.Quantity.residue <- APAC.Consumer.Quantity.local.pred-fitted(APAC.Consumer.Quantity.armafit)

#running adf and kpss tests for conforming residual series is stationary
adf.test(APAC.Consumer.Quantity.residue,alternative = "stationary")
kpss.test(APAC.Consumer.Quantity.residue)

#evaluating the model using MAPE

#making a prediction for the last 6 months
APAC.Consumer.Quantity.outdata <- APAC.Consumer.Quantity[43:48,]
APAC.Consumer.Quantity.timevals.out <- APAC.Consumer.Quantity.outdata$Month

#predicting the six month values by "lmfit" as the ARMA component is absent
APAC.Consumer.Quantity.global.pred.out <- predict((APAC.Consumer.Quantity.lmfit), data.frame(Month = APAC.Consumer.Quantity.timevals.out))
APAC.Consumer.Quantity.fcast <- APAC.Consumer.Quantity.global.pred.out  # predicted values

#compare prediction with the actual values, using MAPE
APAC.Consumer.Quantity.MAPE.class.dec <- accuracy(APAC.Consumer.Quantity.fcast,APAC.Consumer.Quantity.outdata[,2])[5]
APAC.Consumer.Quantity.MAPE.class.dec  #28.78736

# Plotting the predictions along with original values, to get a visual feel of the fit
APAC.Consumer.Quantity.class.dec.pred <- c(ts(APAC.Consumer.Quantity.global.pred),ts(APAC.Consumer.Quantity.global.pred.out))
plot(total.APAC.Consumer.Quantity.timeser, col = "black")
lines(APAC.Consumer.Quantity.class.dec.pred, col = "red")


#model building using ARIMA

#building the model using ARIMA  
APAC.Consumer.Quantity.auto.arima <- auto.arima(APAC.Consumer.Quantity.timeser)
APAC.Consumer.Quantity.auto.arima

#timeseries diagram for ARIMA model
tsdiag(APAC.Consumer.Quantity.auto.arima)
plot(APAC.Consumer.Quantity.auto.arima$x, col="black")
lines(fitted(APAC.Consumer.Quantity.auto.arima), col="red")

#checking the residual series for white noise
APAC.Consumer.Quantity.resi.auto.arima <- APAC.Consumer.Quantity.timeser - fitted(APAC.Consumer.Quantity.auto.arima)

#adf and kpss tests for conforming residual series is stationary
adf.test(APAC.Consumer.Quantity.resi.auto.arima,alternative = "stationary")
kpss.test(APAC.Consumer.Quantity.resi.auto.arima)

#evaluating the model using MAPE
APAC.Consumer.Quantity.fcast.auto.arima <- predict(APAC.Consumer.Quantity.auto.arima, n.ahead = 6)
APAC.Consumer.Quantity.fcast.auto.arima$pred  # predicted values

APAC.Consumer.Quantity.MAPE.auto.arima <- accuracy(APAC.Consumer.Quantity.fcast.auto.arima$pred,APAC.Consumer.Quantity.outdata[,2])[5]
APAC.Consumer.Quantity.MAPE.auto.arima  
#26.24458

#plotting the predictions along with original values to get visuals of the fit
APAC.Consumer.Quantity.auto.arima.pred <- c(fitted(APAC.Consumer.Quantity.auto.arima),ts(APAC.Consumer.Quantity.fcast.auto.arima$pred))
plot(total.APAC.Consumer.Quantity.timeser, col = "black")
lines(APAC.Consumer.Quantity.auto.arima.pred, col = "red")   


#############################forecasting EU Consumer Sales##########################################

#creating time stamp for the APAC Consumer Sales data
total.EU.Cons.Sales.timeser <- ts(EU.Consumer.Sales)

#creating the model using the first 42 row values and then test the model on the remaining 6 row values 
EU.Cons.Sales.indata <- EU.Consumer.Sales[1:42,]
EU.Cons.Sales.timeser <- ts(EU.Cons.Sales.indata$Sales)

## Plotting the APAC consumer Sales timeseries
plot(EU.Cons.Sales.timeser)


#smoothing the series - moving average smoothing with width=1
w <- 1
EU.Cons.Sales.timeser.smooth <- stats::filter(EU.Cons.Sales.timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#smoothing the left end of the time series
difference <- EU.Cons.Sales.timeser.smooth[w+2] - EU.Cons.Sales.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {EU.Cons.Sales.timeser.smooth[i] <- EU.Cons.Sales.timeser.smooth[i+1] - difference}

#smoothing right end of the time series
n <- length(EU.Cons.Sales.timeser.smooth)
differ <- EU.Cons.Sales.timeser.smooth[n-w] - EU.Cons.Sales.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  EU.Cons.Sales.timeser.smooth[i] <- EU.Cons.Sales.timeser.smooth[i-1] + differ
}

#plotting the smoothed time series
EU.Cons.Sales.timevals <- EU.Cons.Sales.indata$Month
plot(EU.Cons.Sales.timeser.smooth, col="blue", lwd=2)


##building a model on the smoothed time series using classical decomposition method

#converting the time series to a dataframe
EU.Cons.Sales.df <- as.data.frame(cbind(EU.Cons.Sales.timevals, as.vector(EU.Cons.Sales.timeser.smooth)))
colnames(EU.Cons.Sales.df) <- c('Month', 'Sales')

#fitting a multiplicative model with trend and seasonality to the data, 
#seasonality will be modeled using a sinusoid function
EU.Cons.Sales.lmfit <- lm(Sales ~ sin(1.0*Month) * poly(Month,3) + cos(0.473*Month) * poly(Month,2) + Month, data=EU.Cons.Sales.df)

#predicting the global values of the series - Seasonality and trend
EU.Cons.Sales.global.pred <- predict(EU.Cons.Sales.lmfit, Month=EU.Cons.Sales.timevals)
summary(EU.Cons.Sales.global.pred)

#plotting the globally predictable part
plot(EU.Cons.Sales.timevals, EU.Cons.Sales.global.pred, col='red', lwd=2)

#looking at the locally predictable series, we model it through ARMA series
EU.Cons.Sales.local.pred <- EU.Cons.Sales.timeser-EU.Cons.Sales.global.pred

#plotting the local component of the series
plot(EU.Cons.Sales.local.pred, col='red', type = "l")

#checking the Autocorrelation for the observations
acf(EU.Cons.Sales.local.pred)
acf(EU.Cons.Sales.local.pred, type="partial")

#modelling the local component using ARMA or auto.arima function
EU.Cons.Sales.armafit <- auto.arima(EU.Cons.Sales.local.pred)
tsdiag(EU.Cons.Sales.armafit)
EU.Cons.Sales.armafit

#checking if the residual series is white noise
EU.Cons.Sales.resi <- EU.Cons.Sales.local.pred-fitted(EU.Cons.Sales.armafit)

#running adf and kpss tests to confirm weather the residual series is stationary
adf.test(EU.Cons.Sales.resi,alternative = "stationary")
kpss.test(EU.Cons.Sales.resi)

############evaluating the model using MAPE

#making a prediction for the last 6 months
EU.Cons.Sales.outdata <- EU.Consumer.Sales[43:48,]
EU.Cons.Sales.timevals.out <- EU.Cons.Sales.outdata$Month

#predicting the six month values by "lmfit" as ARMA component is absent
EU.Cons.Sales.global.pred.out <- predict((EU.Cons.Sales.lmfit), data.frame(Month = EU.Cons.Sales.timevals.out))
EU.Cons.Sales.fcast <- EU.Cons.Sales.global.pred.out  # Predicted values

#compare our prediction with the actual values, using MAPE
EU.Cons.Sales.MAPE.class.dec <- accuracy(EU.Cons.Sales.fcast,EU.Cons.Sales.outdata[,2])[5]
EU.Cons.Sales.MAPE.class.dec  #24.83744

#plotting the predictions along with original values to get a visuals of the fit
EU.Cons.Sales.MAPE.class.dec.pred <- c(ts(EU.Cons.Sales.global.pred),ts(EU.Cons.Sales.global.pred.out))
plot(EU.Cons.Sales.tmser, col = "black")
lines(EU.Cons.Sales.MAPE.class.dec.pred, col = "red")


##building the model using ARIMA

EU.Cons.Sales.auto.arima <- auto.arima(EU.Cons.Sales.timeser)
EU.Cons.Sales.auto.arima

#####timeseries diagram for ARIMA model
tsdiag(EU.Cons.Sales.auto.arima)
plot(EU.Cons.Sales.auto.arima$x, col="black")
lines(fitted(EU.Cons.Sales.auto.arima), col="red")

#checking if the residual series is white noise
EU.Cons.Sales.resi.auto.arima <- EU.Cons.Sales.timeser - fitted(EU.Cons.Sales.auto.arima)

#running adf and kpss test to confirm weather the residual series is stationary
adf.test(EU.Cons.Sales.resi.auto.arima,alternative = "stationary")
kpss.test(EU.Cons.Sales.resi.auto.arima)

#eEvaluating the model using MAPE



EU.Cons.Sales.fcast.auto.arima <- predict(EU.Cons.Sales.auto.arima, n.ahead = 6)
EU.Cons.Sales.fcast.auto.arima  # Predicted values

EU.Cons.Sales.MAPE.auto.arima <- accuracy(EU.Cons.Sales.fcast.auto.arima$pred,EU.Cons.Sales.outdata[,2])[5]
EU.Cons.Sales.MAPE.auto.arima  #28.9226

#plotting the predictions along with original values to get a visuals of the fit
EU.Cons.Sales.auto.arima.pred <- c(fitted(EU.Cons.Sales.auto.arima),ts(EU.Cons.Sales.fcast.auto.arima$pred))
plot(total.EU.Cons.Sales.timeser, col = "black")
lines(EU.Cons.Sales.auto.arima.pred, col = "red")


############################# forecasting EU Consumer Quantity ##########################################

#creating time stamp for the APAC Consumer Quantity data
total.EU.Cons.Quantity.timeser <- ts(EU.Consumer.Quantity)

#creating the model using the first 42 row values and then test the model on the remaining 6 row values 
EU.Cons.Quantity.indata <- EU.Consumer.Quantity[1:42,]
EU.Cons.Quantity.timeser <- ts(EU.Cons.Quantity.indata$Quantity)

## Plotting the APAC consumer Quantity timeseries
plot(EU.Cons.Quantity.timeser)


#smoothing the series - moving average smoothing with width=1
w <- 1
EU.Cons.Quantity.timeser.smooth <- stats::filter(EU.Cons.Quantity.timeser, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

#smoothing the left end of the time series
difference <- EU.Cons.Quantity.timeser.smooth[w+2] - EU.Cons.Quantity.timeser.smooth[w+1]
for (i in seq(w,1,-1)) {EU.Cons.Quantity.timeser.smooth[i] <- EU.Cons.Quantity.timeser.smooth[i+1] - difference}

#smoothing right end of the time series
n <- length(EU.Cons.Quantity.timeser.smooth)
differ <- EU.Cons.Quantity.timeser.smooth[n-w] - EU.Cons.Quantity.timeser.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  EU.Cons.Quantity.timeser.smooth[i] <- EU.Cons.Quantity.timeser.smooth[i-1] + differ
}

#plotting the smoothed time series
EU.Cons.Quantity.timevals <- EU.Cons.Quantity.indata$Month
plot(EU.Cons.Quantity.timeser.smooth, col="blue", lwd=2)


######building a model on the smoothed time series using classical decomposition method

#converting the time series to a dataframe
EU.Cons.Quantity.df <- as.data.frame(cbind(EU.Cons.Quantity.timevals, as.vector(EU.Cons.Quantity.timeser.smooth)))
colnames(EU.Cons.Quantity.df) <- c('Month', 'Quantity')

#fitting a multiplicative model with trend and seasonality to the data, 
#seasonality will be modeled using a sinusoid function
EU.Cons.Quantity.lmfit <- lm(Quantity ~ sin(1.0*Month) * poly(Month,3) + cos(0.473*Month) * poly(Month,2) + Month, data=EU.Cons.Quantity.df)

#predicting the global values of the series - Seasonality and trend
EU.Cons.Quantity.global.pred <- predict(EU.Cons.Quantity.lmfit, Month=EU.Cons.Quantity.timevals)
summary(EU.Cons.Quantity.global.pred)

#plotting the globally predictable part
plot(EU.Cons.Quantity.timevals, EU.Cons.Quantity.global.pred, col='red', lwd=2)

#looking at the locally predictable series, we model it through ARMA series
EU.Cons.Quantity.local.pred <- EU.Cons.Quantity.timeser-EU.Cons.Quantity.global.pred

#plotting the local component of the series
plot(EU.Cons.Quantity.local.pred, col='red', type = "l")

#checking the Autocorrelation for the observations
acf(EU.Cons.Quantity.local.pred)
acf(EU.Cons.Quantity.local.pred, type="partial")

#modelling the local component using ARMA or auto.arima function
EU.Cons.Quantity.armafit <- auto.arima(EU.Cons.Quantity.local.pred)
tsdiag(EU.Cons.Quantity.armafit)
EU.Cons.Quantity.armafit

#checking if the residual series is white noise
EU.Cons.Quantity.resi <- EU.Cons.Quantity.local.pred-fitted(EU.Cons.Quantity.armafit)

#running adf and kpss tests to confirm weather the residual series is stationary
adf.test(EU.Cons.Quantity.resi,alternative = "stationary")
kpss.test(EU.Cons.Quantity.resi)

############evaluating the model using MAPE

#making a prediction for the last 6 months
EU.Cons.Quantity.outdata <- EU.Consumer.Quantity[43:48,]
EU.Cons.Quantity.timevals.out <- EU.Cons.Quantity.outdata$Month


#predicting the six month values by "lmfit" as ARMA component is absent
EU.Cons.Quantity.global.pred.out <- predict((EU.Cons.Quantity.lmfit), data.frame(Month = EU.Cons.Quantity.timevals.out))
EU.Cons.Quantity.fcast <- EU.Cons.Quantity.global.pred.out  # Predicted values

#compare our prediction with the actual values, using MAPE
EU.Cons.Quantity.MAPE.class.dec <- accuracy(EU.Cons.Quantity.fcast,EU.Cons.Quantity.outdata[,2])[5]
EU.Cons.Quantity.MAPE.class.dec  #27.38659

#plotting the predictions along with original values to get a visuals of the fit
EU.Cons.Quantity.MAPE.class.dec.pred <- c(ts(EU.Cons.Quantity.global.pred),ts(EU.Cons.Quantity.global.pred.out))
plot(EU.Cons.Quantity.timeser, col = "black")
lines(EU.Cons.Quantity.MAPE.class.dec.pred, col = "red")


##building the model using ARIMA

EU.Cons.Quantity.auto.arima <- auto.arima(EU.Cons.Quantity.timeser)
EU.Cons.Quantity.auto.arima

#####timeseries diagram for ARIMA model
tsdiag(EU.Cons.Quantity.auto.arima)
plot(EU.Cons.Quantity.auto.arima$x, col="black")
lines(fitted(EU.Cons.Quantity.auto.arima), col="red")

#checking if the residual series is white noise
EU.Cons.Quantity.resi.auto.arima <- EU.Cons.Quantity.timeser - fitted(EU.Cons.Quantity.auto.arima)

#running adf and kpss test to confirm weather the residual series is stationary
adf.test(EU.Cons.Quantity.resi.auto.arima,alternative = "stationary")
kpss.test(EU.Cons.Quantity.resi.auto.arima)

#eEvaluating the model using MAPE
EU.Cons.Quantity.fcast.auto.arima <- predict(EU.Cons.Quantity.auto.arima, n.ahead = 6)
EU.Cons.Quantity.fcast.auto.arima$pred  # Predicted values

EU.Cons.Quantity.MAPE.auto.arima <- accuracy(EU.Cons.Quantity.fcast.auto.arima$pred,EU.Cons.Quantity.outdata[,2])[5]
EU.Cons.Quantity.MAPE.auto.arima  #30.13319

#plotting the predictions along with original values to get a visuals of the fit
EU.Cons.Quantity.auto.arima.pred <- c(fitted(EU.Cons.Quantity.auto.arima),ts(EU.Cons.Quantity.fcast.auto.arima$pred))
plot(total.EU.Cons.Quantity.timeser, col = "black")
lines(EU.Cons.Quantity.auto.arima.pred, col = "red")




