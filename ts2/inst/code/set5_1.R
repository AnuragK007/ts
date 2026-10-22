# TITLE: Topic 1 - Exponential, Moving Average & Holt-Winters Smoothing (Forecasting)

library(forecast)

data(AirPassengers)

# Training (1949-1958) and test (1959-1960) data
train <- window(AirPassengers, end = c(1958, 12))
test  <- window(AirPassengers, start = c(1959, 1))

# Q1. Holt-Winters multiplicative forecast for 2 years
fit <- HoltWinters(train, seasonal = "multiplicative")
pred <- forecast(fit, h = 24)
plot(pred)

# MAPE
mape <- mean(abs((test - pred$mean) / test)) * 100
cat("MAPE =", mape, "%\n")

# Q2. Compare alpha = 0.2 and alpha = 0.8
fit1 <- HoltWinters(train, alpha = 0.2, beta = FALSE, gamma = FALSE)
fit2 <- HoltWinters(train, alpha = 0.8, beta = FALSE, gamma = FALSE)

plot(train)
lines(fitted(fit1)[, "xhat"], col = "blue")
lines(fitted(fit2)[, "xhat"], col = "red")

legend("topleft", legend = c("Alpha 0.2", "Alpha 0.8"),
       col = c("blue", "red"), lty = 1)
