# TITLE: Topic 2 - Auto Regressive (AR) & Moving Average (MA) Models

data(AirPassengers)
x <- AirPassengers
trend <- filter(x, rep(1/12, 12), sides = 2)

# Q1. ACF and PACF (original series and detrended series)
par(mfrow = c(2, 2))
acf(x)
pacf(x)

d <- na.omit(x - trend)   # detrended series
acf(d)
pacf(d)
par(mfrow = c(1, 1))

# Q2. AR(1) and MA(1) on the detrended series
fit_ar <- arima(d, order = c(1, 0, 0))
fit_ma <- arima(d, order = c(0, 0, 1))

print(fit_ar)
print(fit_ma)

# Coefficients and standard errors
print(fit_ar$coef)
print(sqrt(diag(fit_ar$var.coef)))

print(fit_ma$coef)
print(sqrt(diag(fit_ma$var.coef)))

# AR(1) is stationary if |phi| < 1
if (abs(fit_ar$coef["ar1"]) < 1) print("AR(1) is stationary") else print("AR(1) is NOT stationary")
