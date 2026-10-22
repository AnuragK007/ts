# TITLE: Topic 8 - Diagnostic Checking, ARCH & GARCH Models

library(FinTS)
library(rugarch)

data(AirPassengers)
x <- log(AirPassengers)

fit <- arima(x, c(1, 1, 1))
e <- residuals(fit)

# Q1. Ljung-Box on residuals and squared residuals
Box.test(e,   lag = 12, type = "Ljung-Box")
Box.test(e^2, lag = 12, type = "Ljung-Box")

# Q2. Engle's ARCH LM test, then GARCH(1,1) if ARCH effect is present
test <- ArchTest(e, lags = 12)
print(test)

if (test$p.value < 0.05) {
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
  )

  g <- ugarchfit(spec, data = e)
  print(g)
  plot(sigma(g)^2, type = "l", main = "GARCH Conditional Variance")
} else {
  print("No ARCH effect found, GARCH not needed.")
}
