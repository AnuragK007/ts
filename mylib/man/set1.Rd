\name{set1}
\alias{set1}
\title{Time Series Analysis Practical Codebook — Set 1}
\description{
Complete source code for both practical questions in Set 1. The code is displayed as text for reading and copying; it is not run by opening this Help page.
}
\details{
\preformatted{
QUESTION 1: Exponential, Moving Average \& Holt-Winters Smoothing
Complete R Code:
library(forecast)
data(AirPassengers)
x <- AirPassengers

plot(x, main="Monthly Air Passengers")
ma <- stats::filter(x, rep(1/12, 12), sides=2)
ma <- stats::filter(ma, c(0.5, 0.5), sides=2)
lines(ma, col="red", lwd=2)

ses <- ses(x, h=12)
holt\_model <- holt(x, h=12)
add <- hw(x, seasonal="additive", h=12)
mul <- hw(x, seasonal="multiplicative", h=12)

parameters <- data.frame(
  Method=c("SES", "Holt", "Additive", "Multiplicative"),
  Alpha=c(ses\$model\$par["alpha"], holt\_model\$model\$par["alpha"],
          add\$model\$par["alpha"], mul\$model\$par["alpha"]),
  Beta=c(NA, holt\_model\$model\$par["beta"],
         add\$model\$par["beta"], mul\$model\$par["beta"]),
  Gamma=c(NA, NA, add\$model\$par["gamma"], mul\$model\$par["gamma"])
)
print(parameters)

plot(x, main="Smoothing Methods Comparison", col="black")
lines(fitted(ses), col="red")
lines(fitted(holt\_model), col="blue")
lines(fitted(add), col="green")
lines(fitted(mul), col="purple")
legend("topleft",
       legend=c("Original", "SES", "Holt", "Additive", "Multiplicative"),
       col=c("black", "red", "blue", "green", "purple"), lty=1)
Brief Interpretation: Interpretation: Alpha controls level smoothing, beta controls trend smoothing, and gamma controls seasonal smoothing. Multiplicative Holt-Winters is often suitable when seasonal swings grow with the series level.

QUESTION 2: AR \& MA Models
Complete R Code:
data(AirPassengers)
x <- AirPassengers

acf(x, main="ACF")
pacf(x, main="PACF")

trend <- lm(as.numeric(x) \~{} time(x))
d <- ts(residuals(trend), start=start(x), frequency=12)

ar1 <- arima(d, order=c(1,0,0))
ma1 <- arima(d, order=c(0,0,1))

print(ar1)
print(ma1)
print(sqrt(diag(ar1\$var.coef)))
print(sqrt(diag(ma1\$var.coef)))

phi <- ar1\$coef["ar1"]
cat("AR(1) Stationary:", abs(phi) < 1)
Brief Interpretation: Interpretation: For AR(1), stationarity requires |phi| < 1. The ACF and PACF help identify dependence across lags. Detrending does not necessarily remove seasonality.
}
}
\usage{
set1()
}
\value{
Opens the Help page for Set 1. No practical code is executed.
}
\seealso{
\code{help(package = "mylib")}
}
