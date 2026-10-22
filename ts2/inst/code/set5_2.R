# TITLE: Topic 5 - Seasonal ARIMA (SARIMA) Models (Forecast Comparison)

library(forecast)

data(AirPassengers)

train <- window(AirPassengers, end = c(1958, 12))
test  <- window(AirPassengers, start = c(1959, 1))

# Holt-Winters forecast (from Topic 1)
hw_fit  <- HoltWinters(train, seasonal = "multiplicative")
hw_pred <- forecast(hw_fit, h = 24)

# Q1. SARIMA(1,1,1)(1,1,0)[12] on the training set
fit <- Arima(train,
             order = c(1, 1, 1),
             seasonal = list(order = c(1, 1, 0), period = 12))
print(fit)

# Q2. Forecast for 1959-1960
sarima_pred <- forecast(fit, h = 24)
plot(sarima_pred)

# Accuracy comparison
hw_rmse <- sqrt(mean((test - hw_pred$mean)^2))
hw_mae  <- mean(abs(test - hw_pred$mean))

sarima_rmse <- sqrt(mean((test - sarima_pred$mean)^2))
sarima_mae  <- mean(abs(test - sarima_pred$mean))

cat("Holt-Winters RMSE:", hw_rmse, "\n")
cat("Holt-Winters MAE:", hw_mae, "\n")

cat("SARIMA RMSE:", sarima_rmse, "\n")
cat("SARIMA MAE:", sarima_mae, "\n")
