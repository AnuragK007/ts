# TITLE: Topic 7 - FPE, AIC, BIC & Residual Analysis

data(AirPassengers)
x <- log(AirPassengers)

# Q1. Compare ARIMA(1,1,1), (2,1,1), (0,1,2)
m1 <- arima(x, c(1, 1, 1))
m2 <- arima(x, c(2, 1, 1))
m3 <- arima(x, c(0, 1, 2))

models <- list(m1, m2, m3)
n <- length(x)

tab <- data.frame(
  Model  = c("ARIMA(1,1,1)", "ARIMA(2,1,1)", "ARIMA(0,1,2)"),
  LogLik = sapply(models, function(m) as.numeric(logLik(m))),
  AIC    = sapply(models, AIC),
  BIC    = sapply(models, BIC),
  FPE    = sapply(models, function(m) {
    k <- length(m$coef)
    m$sigma2 * (n + k) / (n - k)
  })
)
print(tab)

# Q2. Residuals of the best model (by AIC)
fit <- models[[which.min(sapply(models, AIC))]]
e <- residuals(fit)

par(mfrow = c(2, 2))
acf(e)
hist(e, probability = TRUE)
lines(density(e), col = "red")
qqnorm(e)
qqline(e)
par(mfrow = c(1, 1))
