# TITLE: Topic 1 - Exponential, Moving Average & Holt-Winters Smoothing

data(AirPassengers)
x <- AirPassengers
par(mfrow = c(1, 1))

# Q1. 12-month centered moving average (trend)
plot(x, main = "Air Passengers")
trend <- filter(x, rep(1/12, 12), sides = 2)
lines(trend, col = "red")

# Q2. SES, Holt, Holt-Winters (additive and multiplicative)
library(forecast)

fit_ses  <- ses(x)
fit_holt <- holt(x)
fit_add  <- hw(x, seasonal = "additive")
fit_mul  <- hw(x, seasonal = "multiplicative")

plot(x, main = "Smoothing Comparison")
lines(fitted(fit_ses),  col = "red")
lines(fitted(fit_holt), col = "blue")
lines(fitted(fit_add),  col = "green")
lines(fitted(fit_mul),  col = "purple")
legend("topleft", legend = c("SES", "Holt", "HW additive", "HW multiplicative"),
       col = c("red", "blue", "green", "purple"), lty = 1, cex = 0.8)

# Smoothing parameters (alpha, beta, gamma)
print(fit_ses$model$par)
print(fit_holt$model$par)
print(fit_add$model$par)
print(fit_mul$model$par)
