\name{set3}
\alias{set3}
\title{SARIMA, Regression and Estimation}
\usage{
set3()
}
\details{
The following listing contains the practical exercises for Set 3.
}
\examples{
\dontrun{
QUESTION 1: SARIMA & Time Series Regression

data(AirPassengers)
x <- AirPassengers
plot(x, main="Monthly Air Passengers")
monthplot(x)

y <- log(x)
sarima <- arima(y, order=c(0,1,1),
                seasonal=list(order=c(0,1,1), period=12))
print(sarima)

trend <- 1:length(x)
month <- factor(cycle(x))
reg <- lm(as.numeric(y) ~ trend + month)
summary(reg)

actual <- as.numeric(y)
sarima_fit <- actual - as.numeric(residuals(sarima))
reg_fit <- fitted(reg)

r2 <- function(a, f) 1 - sum((a-f)^2)/sum((a-mean(a))^2)
rmse <- function(a, f) sqrt(mean((a-f)^2))

comparison <- data.frame(
  Model=c("SARIMA", "Regression"),
  R2=c(r2(actual, sarima_fit), r2(actual, reg_fit)),
  RMSE=c(rmse(actual, sarima_fit), rmse(actual, reg_fit))
)
print(comparison)

Interpretation: Interpretation: Higher R² and lower RMSE indicate better in-sample fit. The SARIMA residual-based fitted-value calculation is an approximation, especially around initialization.

QUESTION 2: MLE, Conditional Sum of Squares & AR/MA Grid Search

data(AirPassengers)
x <- diff(log(AirPassengers))

mle <- arima(x, order=c(1,0,1), method="ML")
cls <- arima(x, order=c(1,0,1), method="CSS")
print(mle)
print(cls)

results <- data.frame()
for(p in 0:3){
  for(q in 0:3){
    model <- try(arima(x, order=c(p,0,q), method="ML"), silent=TRUE)
    if(!inherits(model, "try-error")){
      results <- rbind(results,
        data.frame(p=p, q=q, AIC=AIC(model)))
    }
  }
}
results <- results[order(results$AIC), ]
print(results)
cat("Best model on differenced log data: ARMA(",
    results$p[1], ",", results$q[1], ")\n", sep="")

Interpretation: Interpretation: ML and CSS estimates can differ. The model with the lowest AIC is preferred among the successfully fitted candidates. Because x is already differenced log data, the selected model corresponds to ARIMA(p,1,q) on log(AirPassengers).
}
}
\keyword{documentation}
