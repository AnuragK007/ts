\name{set2}
\alias{set2}
\title{Time Series Analysis Practical Codebook — Set 2}
\description{
Complete source code for both practical questions in Set 2. The code is displayed as text for reading and copying; it is not run by opening this Help page.
}
\details{
\preformatted{
QUESTION 1: ARMA(1,1): Estimation, Roots, Stationarity \& Invertibility
Complete R Code:
data(AirPassengers)
x <- AirPassengers
d <- diff(x)

model <- arima(d, order=c(1,0,1))
print(model)

phi <- model\$coef["ar1"]
theta <- model\$coef["ma1"]
mu <- model\$coef["intercept"]

cat("ARMA Equation: Yt -", mu, "=", phi,
    "(Yt-1 -", mu, ") + et +", theta, "et-1\\n")

ar\_root <- polyroot(c(1, -phi))
ma\_root <- polyroot(c(1, theta))
print(ar\_root)
print(ma\_root)

cat("Stationary:", all(Mod(ar\_root) > 1), "\\n")
cat("Invertible:", all(Mod(ma\_root) > 1))
Brief Interpretation: Interpretation: AR roots must lie outside the unit circle for stationarity; MA roots must lie outside the unit circle for invertibility.

QUESTION 2: ADF Test, Differencing \& ARIMA Forecasting
Complete R Code:
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
Brief Interpretation: Interpretation: ADF null hypothesis is a unit root. A p-value below 0.05 is evidence against the unit-root null. Lower differencing order is preferred when it achieves stationarity.
}
}
\usage{
set2()
}
\value{
Opens the Help page for Set 2. No practical code is executed.
}
\seealso{
\code{help(package = "mylib")}
}
