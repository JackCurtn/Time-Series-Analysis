# Jack Curtin - 16181484
# Time series Project Code

library(TSA)
library(tseries)

#---------------------------------------------------------------------------------------------------------------------
# -- Checking the Data --

data <- read.csv("AverageIncome.csv", header=TRUE) # Reads the data
dataTS <- ts(data$average.income, start = c(1950)) # Converts to a time series
plot(dataTS, ylab="Average Income (Euro)", xlab="Year",xlim=c(1950,2019), main="Average income per year")
abline(v=2009, col="red",lwd=3, lty=2);legend(1950, 40000, legend=c("10% Cut off"), col=c("red"), lwd=3, lty=2)


# New time series with last 10% Removed
dataTSNew <- ts(data$average.income, start = c(1950), end=c(2009)) # Converts to a time series


# Looking at the ACf and differenced ACF
acf(dataTSNew) # ACF is significant -> Data is not a random walk
acf(diff(dataTSNew)) # Differenced ACF is significant -> Data is not white noise



#---------------------------------------------------------------------------------------------------------------------
# -- Analysing the Data --

# Augmented Dickey-Fuller Test
adf.test(dataTSNew) # p-val > 0.05 -> Not stationary -> Suggests that differencing is required


# Box-Cox Transformation
BC <- BoxCox.ar(dataTSNew, lambda=seq(-2,2,0.1))
BC$mle # MLE is lambda = -0.1
BC$ci # [-0.5, 0.2] -> Contains lambda = 0 so we can log-transform our data


# Log Transformed Data
dataTSLog <- log(dataTSNew)
plot(dataTSLog) # Our log transformed plot still has a trend, so we need to difference it
adf.test(dataTSLog)


# Differenced Log Transformed Data
dataTSLogDiff <- diff(log(dataTS)) # Differenced log data
adf.test(dataTSLogDiff) # p-val > 0.05 -> Not stationary -> Suggests we should difference again


# Differenced Twice Log Transformed Data
dataTSLogDiff2 <- diff(dataTSLogDiff) # Differenced twice log data
acf(dataTSLogDiff2)
adf.test(dataTSLogDiff2) # p-val < 0.05 -> Our data is now stationary
plot(dataTSLogDiff2)


qqnorm(dataTSLogDiff2); qqline(dataTSLogDiff2)
shapiro.test(dataTSLogDiff2) # p-val > 0.05 -> Data is normally distributed



#---------------------------------------------------------------------------------------------------------------------
# -- Modelling the Data --

# ACF, PACF and EACF of dataTSLogDiff2
acf(dataTSLogDiff2) # Suggests MA(1)
pacf(dataTSLogDiff2) # Suggests AR(1) or perhaps AR(14)
eacf(dataTSLogDiff2)

# BIC Plot
subs <-  armasubsets(dataTSLogDiff2)
plot(subs)


# ---------------------
# Model 1: ARIMA(0,2,1)

model1 <- arima(log(dataTSNew), order=c(0,2,1))
model1$aic # AIC = -230.02

# Residuals
resid <- rstandard(model1)
hist(resid, main = "Histogram of Residuals")
qqnorm(resid); qqline(resid); # QQ-Plot
shapiro.test(resid) # p-val 0.503 > 0.05 -> Data is normally distributed

# ACF and Ljung-Box
tsdiag(model1, gof.lag = 15) # Ljung-Box: Not all p-values above 0.05 -> Not all residuals are white noise


# ---------------------
# Model 2: ARIMA(2,2,1)

model2 <- arima(log(dataTSNew), order=c(2,2,1))
model2$aic # AIC = -230.71

# Residuals
resid <- rstandard(model2)
hist(resid, main = "Histogram of Residuals")
qqnorm(resid); qqline(resid); # QQ-Plot
shapiro.test(resid) # p-val 0.4176 > 0.05 -> Data is normally distributed

# ACF and Ljung-Box
tsdiag(model2, gof.lag = 15) # Ljung-Box: p-values all above 0.05 -> Residuals are white noise


# ---------------------
# Model 3: ARIMA(4,2,1)

model3 <- arima(log(dataTSNew), order=c(4,2,1))
model3$aic # AIC = -226.93

# Residuals
resid <- rstandard(model3)
hist(resid, main = "Histogram of Residuals")
qqnorm(resid); qqline(resid); # QQ-Plot
shapiro.test(resid) # p-val 0.6229 > 0.05 -> Data is normally distributed

# ACF and Ljung-Box
tsdiag(model3, gof.lag = 15) # Ljung-Box: p-values all above 0.05 -> Residuals are white noise



# ---------------------
# Chosen Model -> Model 2: ARIMA(2,2,1)

# Coefficients of Model
model2

# Plot of Model
plot(model2, n.ahead = 10,transform = exp, type = "o",col = "red", main = "Model 2", ylab="Average Income (Euro)", xlab="Year")
lines(dataTS, col = "green")

# Zoomed in plot of model
plot(model2, n1=2009, n.ahead = 10,transform = exp, type = "o",col = "red", main = "Model 2", ylab="Average Income (Euro)", xlab="Year")
lines(dataTS, col = "green", type = "o")
