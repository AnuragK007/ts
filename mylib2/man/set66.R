# Set 66: ARIMA parameter significance, model selection, diagnostics
library(forecast)
data(AirPassengers)
x <- log(AirPassengers)

# 1. Fit ARIMA(2,1,2), calculate approximate t-ratios and p-values
fit <- Arima(x, order = c(2, 1, 2))
print(fit)

se <- sqrt(diag(fit$var.coef))
t_ratio <- fit$coef / se
p_value <- 2 * (1 - pnorm(abs(t_ratio)))
result <- data.frame(
  Coefficient = fit$coef,
  Std_Error = se,
  T_Ratio = t_ratio,
  P_Value = p_value
)
print(result)

# Remove one AR and one MA term
fit2 <- Arima(x, order = c(1, 1, 1))
print(fit2)

# 2. Compare ARIMA models p = 0..3, q = 0..3
results <- data.frame(p = integer(), q = integer(),
                      AIC = numeric(), BIC = numeric())
for (p in 0:3) {
  for (q in 0:3) {
    model <- try(Arima(x, order = c(p, 1, q)), silent = TRUE)
    if (!inherits(model, "try-error")) {
      results <- rbind(results, data.frame(
        p = p, q = q, AIC = AIC(model), BIC = BIC(model)
      ))
    }
  }
}
print(results)

best <- results[which.min(results$BIC), ]
best_fit <- Arima(x, order = c(best$p, 1, best$q))
print(best_fit)

# Residual diagnostics
e <- residuals(best_fit)
library(lmtest)
print(dwtest(e ~ 1))
cpgram(e)
