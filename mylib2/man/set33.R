# Set 33: Seasonality, regression, estimation, and grid search
data(AirPassengers)
x <- log(AirPassengers)

# 1. Identify seasonality
plot(x, main = "Log Air Passengers Seasonality")
monthplot(x)

# SARIMA(0,1,1)(0,1,1)[12]
library(forecast)
sarima_fit <- Arima(
  x,
  order = c(0, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 12)
)
print(sarima_fit)

# 2. Time-series regression
t <- seq_along(x)
month <- factor(cycle(x))
reg <- lm(as.numeric(x) ~ t + month)
print(summary(reg))

actual <- as.numeric(x)
sarima_fitted <- as.numeric(fitted(sarima_fit))
reg_fitted <- fitted(reg)

cat("Regression R-squared:", summary(reg)$r.squared, "\\n")
cat("Regression RMSE:", sqrt(mean((actual - reg_fitted)^2)), "\\n")
cat("SARIMA RMSE:", sqrt(mean((actual - sarima_fitted)^2, na.rm = TRUE)), "\\n")

# 3. MLE and conditional sum of squares
mle_fit <- arima(x, order = c(1, 1, 1), method = "ML")
css_fit <- arima(x, order = c(1, 1, 1), method = "CSS")
print(mle_fit)
print(css_fit)

# 4. Grid search p = 0..3, q = 0..3
results <- data.frame(p = integer(), q = integer(), AIC = numeric())
for (p in 0:3) {
  for (q in 0:3) {
    fit <- try(arima(x, order = c(p, 1, q), method = "ML"), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      results <- rbind(results, data.frame(p = p, q = q, AIC = AIC(fit)))
    }
  }
}
print(results[order(results$AIC), ])
