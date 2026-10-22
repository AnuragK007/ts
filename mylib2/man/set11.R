# Set 11: Smoothing and AR/MA foundations
data(AirPassengers)
x <- AirPassengers

# Moving average
plot(x, main = "Air Passengers")
trend <- stats::filter(x, rep(1/12, 12), sides = 2)
lines(trend, col = "red")

# Exponential smoothing
library(forecast)
ses_fit <- ses(x)
holt_fit <- holt(x)
add_fit <- hw(x, seasonal = "additive")
mul_fit <- hw(x, seasonal = "multiplicative")

plot(x, main = "Smoothing Comparison")
lines(fitted(ses_fit), col = "red")
lines(fitted(holt_fit), col = "blue")
lines(fitted(add_fit), col = "green")
lines(fitted(mul_fit), col = "purple")

print(ses_fit$model)
print(holt_fit$model)
print(add_fit$model)
print(mul_fit$model)

# ACF and PACF
par(mfrow = c(2, 2))
acf(x, main = "ACF: Original")
pacf(x, main = "PACF: Original")

# Detrend
d <- na.omit(x - trend)
acf(d, main = "ACF: Detrended")
pacf(d, main = "PACF: Detrended")

# AR(1) and MA(1)
ar_fit <- arima(d, order = c(1, 0, 0))
ma_fit <- arima(d, order = c(0, 0, 1))
print(ar_fit)
print(ma_fit)

# Coefficients and standard errors
print(ar_fit$coef)
print(sqrt(diag(ar_fit$var.coef)))
print(ma_fit$coef)
print(sqrt(diag(ma_fit$var.coef)))
