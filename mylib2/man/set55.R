# Set 55: Holt-Winters and seasonal forecasting comparison
library(forecast)
data(AirPassengers)

# Training and test data
train <- window(AirPassengers, end = c(1958, 12))
test <- window(AirPassengers, start = c(1959, 1))

# 1. Holt-Winters multiplicative forecast for 24 months
hw_fit <- HoltWinters(train, seasonal = "multiplicative")
hw_pred <- forecast(hw_fit, h = 24)
plot(hw_pred)

mape <- mean(abs((as.numeric(test) - as.numeric(hw_pred$mean)) / as.numeric(test))) * 100
cat("Holt-Winters MAPE =", mape, "%\\n")

# Compare alpha values using level-only exponential smoothing
fit_alpha_02 <- HoltWinters(train, alpha = 0.2, beta = FALSE, gamma = FALSE)
fit_alpha_08 <- HoltWinters(train, alpha = 0.8, beta = FALSE, gamma = FALSE)

plot(train, main = "Effect of Alpha")
lines(fitted(fit_alpha_02)[, "xhat"], col = "blue")
lines(fitted(fit_alpha_08)[, "xhat"], col = "red")
legend("topleft", legend = c("Alpha 0.2", "Alpha 0.8"),
       col = c("blue", "red"), lty = 1)

# 2. SARIMA forecast for same test period
sarima_fit <- Arima(
  train,
  order = c(1, 1, 1),
  seasonal = list(order = c(1, 1, 0), period = 12)
)
sarima_pred <- forecast(sarima_fit, h = 24)
plot(sarima_pred)

hw_rmse <- sqrt(mean((as.numeric(test) - as.numeric(hw_pred$mean))^2))
hw_mae <- mean(abs(as.numeric(test) - as.numeric(hw_pred$mean)))
sarima_rmse <- sqrt(mean((as.numeric(test) - as.numeric(sarima_pred$mean))^2))
sarima_mae <- mean(abs(as.numeric(test) - as.numeric(sarima_pred$mean)))

cat("Holt-Winters RMSE:", hw_rmse, "\\n")
cat("Holt-Winters MAE:", hw_mae, "\\n")
cat("SARIMA RMSE:", sarima_rmse, "\\n")
cat("SARIMA MAE:", sarima_mae, "\\n")
