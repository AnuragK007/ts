# TITLE: Topic 7 - FPE, AIC, BIC & Residual Analysis (Order Selection)

library(forecast)
library(lmtest)

data(AirPassengers)
x <- log(AirPassengers)

# Q1. AIC and BIC for ARIMA(0,1,0) to ARIMA(3,1,3)
results <- data.frame()

for (p in 0:3) {
  for (q in 0:3) {
    fit <- try(Arima(x, order = c(p, 1, q)), silent = TRUE)
    if (!inherits(fit, "try-error"))
      results <- rbind(results, data.frame(
        p = p, q = q,
        AIC = AIC(fit),
        BIC = BIC(fit)
      ))
  }
}

print(results)

# Model with lowest BIC (BIC penalizes extra lags more than AIC)
best <- results[which.min(results$BIC), ]
fit <- Arima(x, order = c(best$p, 1, best$q))
print(fit)

# Q2. Durbin-Watson test and cumulative periodogram of residuals
e <- residuals(fit)

print(dwtest(e ~ 1))
cpgram(e)
