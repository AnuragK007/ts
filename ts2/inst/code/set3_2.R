# TITLE: Topic 6 - Estimation of ARIMA Parameters & Order Selection

data(AirPassengers)
x <- log(AirPassengers)

# Q1. MLE vs CLS
mle <- arima(x, order = c(1, 1, 1), method = "ML")
cls <- arima(x, order = c(1, 1, 1), method = "CSS")

print(mle)
print(cls)

# Q2. Grid search: p = 0 to 3, q = 0 to 3
results <- data.frame()

for (p in 0:3) {
  for (q in 0:3) {
    m <- try(arima(x, order = c(p, 1, q), method = "ML"), silent = TRUE)
    if (!inherits(m, "try-error"))
      results <- rbind(results, data.frame(p = p, q = q, AIC = AIC(m)))
  }
}

print(results[order(results$AIC), ])
