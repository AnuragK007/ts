# TITLE: Topic 4 - Nonstationary & Seasonal Time Series Models (ARIMA)

library(tseries)
library(forecast)

data(AirPassengers)
x <- AirPassengers

# Q1. ADF test on raw data, then on the differenced log series
adf.test(x)

d <- diff(log(x))
adf.test(d)

# Q2. ARIMA(1,1,1) on the log series and 12-month forecast with 95% CI
model <- arima(log(x), order = c(1, 1, 1))
print(model)

f <- forecast(model, h = 12, level = 95)
plot(f)
