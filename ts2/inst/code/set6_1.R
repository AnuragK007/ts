# TITLE: Topic 6 - Estimation of ARIMA Parameters (AR & MA Terms)

library(forecast)

data(AirPassengers)
x <- log(AirPassengers)

# Q1. Over-parameterized ARIMA(2,1,2) and t-ratios
fit <- Arima(x, order = c(2, 1, 2))
print(fit)

se <- sqrt(diag(fit$var.coef))
t_ratio <- fit$coef / se
p_value <- 2 * (1 - pnorm(abs(t_ratio)))

result <- data.frame(
  Coefficient = fit$coef,
  T_Ratio = t_ratio,
  P_Value = p_value
)
print(result)

# Q2. Remove one AR and one MA term -> ARIMA(1,1,1)
fit2 <- Arima(x, order = c(1, 1, 1))
print(fit2)

se2 <- sqrt(diag(fit2$var.coef))
print(data.frame(Coefficient = fit2$coef,
                 T_Ratio = fit2$coef / se2,
                 P_Value = 2 * (1 - pnorm(abs(fit2$coef / se2)))))
