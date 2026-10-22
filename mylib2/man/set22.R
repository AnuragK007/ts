# Set 22: ARMA, stationarity, and ARIMA
data(AirPassengers)
x <- AirPassengers

# 1. ARMA(1,1) on log-differenced data
d <- diff(log(x))
m <- arima(d, order = c(1, 0, 1))
print(m)
print(m$coef)

# Check AR and MA roots
ar_root <- polyroot(c(1, -m$coef["ar1"]))
ma_root <- polyroot(c(1, m$coef["ma1"]))
print(ar_root)
print(ma_root)

if (all(Mod(ar_root) > 1)) print("AR is stationary")
if (all(Mod(ma_root) > 1)) print("MA is invertible")

# 2. ADF tests
library(tseries)
print(adf.test(x))
d <- diff(log(x))
print(adf.test(d))

# Fit ARIMA and forecast 12 months
library(forecast)
model <- Arima(log(x), order = c(1, 1, 1))
print(model)
f <- forecast(model, h = 12)
plot(f)
