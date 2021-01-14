#Extracting the data
library(readxl)
AirPassengers <- read_excel("E:/STUFF/3-2/ECON F342 ApEc/Assignment 3/AirPassengers.xlsx", col_types = c("blank", "numeric"))
View(AirPassengers)

#Loading required Libraries
library(tseries)
library(aTSA)
library(forecast)
library(urca)

#Converting data into time-series format
APts = ts(AirPassengers, frequency = 12, start = 1949)
class(APts)
APts

#Plotting the time-series and checking general trend
plot.ts(APts)
abline(reg = lm(APts ~ time(APts)))

#We can notice that frequency of airplane travels increases every summer
#We can confirm this using Box Plot
boxplot(APts ~ cycle(APts), xlab = "Date", ylab = "Passenger frequency (1000s)", main = "Monthly Air Passengers Boxplot (1949-60)")
#Thus, we can observe that the frequency of flight travels are maximum in the month on July

### QUESTION 1.A)
#We can also notice that seasonality increases with general trend
#So this is indicative of a multiplicative model rather than an additive model
#Decomposing the time-series
APtsDecomp = decompose(APts, type = "multiplicative")
plot(APtsDecomp)

#We can notice that there is an increasing trend and
#From the decomposition we can observe that there isn't much randomness in our model
#So there is no need to smoothen the time-series

#We can observe that our data is strongly seasonal, so we will use seasonal difference
APtsadj = diff(APts, lag = 12)
plot(APtsadj)
abline(reg = lm(APtsadj ~ time(APtsadj)))

### QUESTION 2.A)
#Testing for stationarity
#From the plot itself we can observe that our dataset is not stationary

#Now we will homogenize the mean
APtsdiff1 = diff(APtsadj, differences = 1)
plot(APtsdiff1)

#Now we can test for stationarity
stationary.test(APtsdiff1, method = "adf")
stationary.test(APtsdiff1, method = "pp")
stationary.test(APtsdiff1, method = "kpss")

### QUESTION 2.B)
#Now we can analyze the acf and pacf plots
Acf(APtsdiff1, lag.max = 20)
Acf(APtsdiff1, lag.max = 20, plot = FALSE)
Pacf(APtsdiff1, lag.max = 20)
Pacf(APtsdiff1, lag.max = 20, plot = FALSE)

#Applying auto arima to our differenced and original time series
auto.arima(APtsdiff1)
auto.arima(APts)

### QUESTION 2.C)
#Fitting the model
finalmodel = Arima(APts, order=c(2,1,1), seasonal=list(order=c(0,1,0), period=12))
plot.ts(finalmodel$residuals) # make time plot of forecast errors

#Making forecasts for the next ten years
modelforecast = forecast(finalmodel, level = c(95), h=10*12)

#Since the given forecasts are in decimal, we will take the floor value
#Because the number of people travelling cannot be in decimals
modelforecast$mean = floor(modelforecast$mean)
modelforecast$upper = floor(modelforecast$upper)
modelforecast$lower = floor(modelforecast$lower)
modelforecast$x = floor(modelforecast$x)
modelforecast
plot(modelforecast)

### QUESTION 2.D)
#Testing validity
acf(ts(modelforecast$residuals), main = "ACF Residuals")
Box.test(modelforecast$residuals, lag=10, type="Ljung-Box")

#Analysing the forecast residuals
plot.ts(modelforecast$residuals)
hist(modelforecast$residuals) # make a histogram