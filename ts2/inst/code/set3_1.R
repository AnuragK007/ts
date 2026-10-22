# TITLE: Topic 5 - Seasonal ARIMA (SARIMA) Models & Time Series Regression

library(forecast)

data(AirPassengers)
x <- log(AirPassengers)

# Q1. Seasonality and SARIMA(0,1,1)(0,1,1)[12]
plot(x, main = "Air Passengers Seasonality")
monthplot(x)

sarima <- Arima(x, order = c(0, 1, 1), seasonal = c(0, 1, 1))
print(sarima)

# Q2. Regression with linear trend + monthly dummies
t <- 1:length(x)
month <- factor(cycle(x))
reg <- lm(as.numeric(x) ~ t + month)
print(summary(reg))

# Compare R-squared and RMSE
actual     <- as.numeric(x)
sarima_fit <- as.numeric(fitted(sarima))
reg_fit    <- fitted(reg)

cat("Regression R-squared:", summary(reg)$r.squared, "\n")
cat("Regression RMSE:", sqrt(mean((actual - reg_fit)^2)), "\n")
cat("SARIMA RMSE:", sqrt(mean((actual - sarima_fit)^2, na.rm = TRUE)), "\n")
