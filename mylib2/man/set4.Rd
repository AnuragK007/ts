\name{set4}
\alias{set4}
\title{Model Selection and Volatility Diagnostics}
\usage{
set4()
}
\details{
The following listing contains the practical exercises for Set 4.
}
\examples{
\dontrun{
QUESTION 1: AIC, BIC, FPE & Residual Plots

data(AirPassengers)
x <- AirPassengers

m1 <- arima(x, order=c(1,1,1))
m2 <- arima(x, order=c(2,1,1))
m3 <- arima(x, order=c(0,1,2))
models <- list(m1, m2, m3)
n <- length(x)

results <- data.frame(
  Model=c("ARIMA(1,1,1)", "ARIMA(2,1,1)", "ARIMA(0,1,2)"),
  LogLik=sapply(models, logLik),
  AIC=sapply(models, AIC),
  BIC=sapply(models, BIC),
  FPE=sapply(models, function(m)
    m$sigma2*(n+length(m$coef))/(n-length(m$coef)))
)
print(results)

best <- models[[which.min(results$AIC)]]
res <- residuals(best)

par(mfrow=c(1,3))
acf(res, main="Residual ACF")
hist(res, probability=TRUE, main="Histogram")
lines(density(res), col="red")
qqnorm(res, main="Normal Q-Q Plot")
qqline(res, col="red")
par(mfrow=c(1,1))

Interpretation: Interpretation: Prefer lower AIC/BIC/FPE and higher log-likelihood. Residual ACF checks serial dependence; histogram and Q-Q plot assess approximate normality. FPE shown is an approximation based on the fitted variance and number of estimated coefficients.

QUESTION 2: Ljung-Box, ARCH LM & GARCH(1,1)

library(FinTS)
library(rugarch)
data(AirPassengers)
x <- AirPassengers

m1 <- arima(x, order=c(1,1,1))
m2 <- arima(x, order=c(2,1,1))
m3 <- arima(x, order=c(0,1,2))
models <- list(m1, m2, m3)
best <- models[[which.min(sapply(models, AIC))]]
res <- na.omit(as.numeric(residuals(best)))

Box.test(res, lag=12, type="Ljung-Box")
Box.test(res^2, lag=12, type="Ljung-Box")
arch <- ArchTest(res, lags=12)
print(arch)

if(arch$p.value < 0.05){
  spec <- ugarchspec(
    variance.model=list(model="sGARCH", garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(0,0), include.mean=FALSE)
  )
  fit <- ugarchfit(spec, data=res)
  show(fit)
  plot(as.numeric(sigma(fit))^2, type="l",
       main="GARCH(1,1) Conditional Variance",
       xlab="Time", ylab="Variance")
}

Interpretation: Interpretation: For Ljung-Box and ARCH LM tests, p < 0.05 indicates evidence against the null of no dependence/effects. GARCH models changing conditional variance.
}
}
\keyword{documentation}
