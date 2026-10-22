# Set 44: Information criteria, residual checks, ARCH/GARCH
data(AirPassengers)
x <- log(AirPassengers)

# 1. Compare candidate ARIMA models
m1 <- arima(x, order = c(1, 1, 1))
m2 <- arima(x, order = c(2, 1, 1))
m3 <- arima(x, order = c(0, 1, 2))
models <- list(m1, m2, m3)

comparison <- do.call(rbind, lapply(seq_along(models), function(i) {
  m <- models[[i]]
  n <- length(x)
  k <- length(m$coef)
  data.frame(
    Model = c("ARIMA(1,1,1)", "ARIMA(2,1,1)", "ARIMA(0,1,2)")[i],
    LogLik = as.numeric(logLik(m)),
    AIC = AIC(m),
    BIC = BIC(m),
    FPE = m$sigma2 * (n + k) / (n - k)
  )
}))
print(comparison)

# Residual analysis for model with lowest AIC
best_fit <- models[[which.min(sapply(models, AIC))]]
e <- residuals(best_fit)
acf(e, main = "Residual ACF")
hist(e, probability = TRUE, main = "Residual Histogram")
lines(density(e), col = "red")
qqnorm(e)
qqline(e)

# 2. Diagnostic tests and optional GARCH
library(FinTS)
library(rugarch)

print(Box.test(e, lag = 12, type = "Ljung-Box"))
print(Box.test(e^2, lag = 12, type = "Ljung-Box"))
arch_test <- ArchTest(e, lags = 12)
print(arch_test)

if (arch_test$p.value < 0.05) {
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
  )
  garch_fit <- ugarchfit(spec, data = as.numeric(e))
  print(garch_fit)
  plot(sigma(garch_fit)^2, type = "l", main = "GARCH Conditional Variance")
}
