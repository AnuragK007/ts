\name{set2}
\alias{set2}
\title{ARMA Models and ARIMA Forecasting}
\usage{
set2()
}
\details{
The following listing contains the practical exercises for Set 2.
}
\examples{
\dontrun{
QUESTION 1: ARMA(1,1): Estimation, Roots, Stationarity & Invertibility

data(AirPassengers)
x <- AirPassengers
d <- diff(x)

model <- arima(d, order=c(1,0,1))
print(model)

phi <- model$coef["ar1"]
theta <- model$coef["ma1"]
mu <- model$coef["intercept"]

cat("ARMA Equation: Yt -", mu, "=", phi,
    "(Yt-1 -", mu, ") + et +", theta, "et-1\n")

ar_root <- polyroot(c(1, -phi))
ma_root <- polyroot(c(1, theta))
print(ar_root)
print(ma_root)

cat("Stationary:", all(Mod(ar_root) > 1), "\n")
cat("Invertible:", all(Mod(ma_root) > 1))

Interpretation: Interpretation: AR roots must lie outside the unit circle for stationarity; MA roots must lie outside the unit circle for invertibility.

QUESTION 2: ADF Test, Differencing & ARIMA Forecasting

library(tseries)
library(forecast)
data(AirPassengers)
x <- AirPassengers

adf.test(x)
d <- ndiffs(x)
print(d)

dx <- diff(x, differences=d)
adf.test(dx)

model <- auto.arima(x, d=d, seasonal=FALSE)
summary(model)

fc <- forecast(model, h=12, level=95)
print(fc)
plot(fc, main="ARIMA 12-Month Forecast")

Interpretation: Interpretation: ADF null hypothesis is a unit root. A p-value below 0.05 is evidence against the unit-root null. Lower differencing order is preferred when it achieves stationarity.
}
}
\keyword{documentation}
