\name{set1}
\alias{set1}
\title{Set 1: Smoothing & Autoregressive / Moving Average Foundations}
\usage{
set1(topic = NULL, run = FALSE, copy = FALSE)
}
\arguments{
\item{topic}{Topic number (1, 2, ...) or a word from the topic title.
\code{NULL} (default) means all topics of the set.}

\item{run}{If \code{TRUE}, run the code in the console instead of printing it.}

\item{copy}{If \code{TRUE}, also copy the code to the clipboard (needs clipr).}
}
\value{
Invisibly, the code lines that were shown.
}
\description{
Code for Set 1: Smoothing & Autoregressive / Moving Average Foundations, split topic-wise. The full code is in the
Examples section below. Topics in this set:
\itemize{
\item Topic 1 - Exponential, Moving Average & Holt-Winters Smoothing
\item Topic 2 - Auto Regressive (AR) & Moving Average (MA) Models
}
}
\examples{
######################################################################
# Topic 1 - Exponential, Moving Average & Holt-Winters Smoothing
######################################################################
data(AirPassengers)
x <- AirPassengers
par(mfrow = c(1, 1))

# Q1. 12-month centered moving average (trend)
plot(x, main = "Air Passengers")
trend <- filter(x, rep(1/12, 12), sides = 2)
lines(trend, col = "red")

# Q2. SES, Holt, Holt-Winters (additive and multiplicative)
library(forecast)

fit_ses  <- ses(x)
fit_holt <- holt(x)
fit_add  <- hw(x, seasonal = "additive")
fit_mul  <- hw(x, seasonal = "multiplicative")

plot(x, main = "Smoothing Comparison")
lines(fitted(fit_ses),  col = "red")
lines(fitted(fit_holt), col = "blue")
lines(fitted(fit_add),  col = "green")
lines(fitted(fit_mul),  col = "purple")
legend("topleft", legend = c("SES", "Holt", "HW additive", "HW multiplicative"),
       col = c("red", "blue", "green", "purple"), lty = 1, cex = 0.8)

# Smoothing parameters (alpha, beta, gamma)
print(fit_ses$model$par)
print(fit_holt$model$par)
print(fit_add$model$par)
print(fit_mul$model$par)
######################################################################
# Topic 2 - Auto Regressive (AR) & Moving Average (MA) Models
######################################################################
data(AirPassengers)
x <- AirPassengers
trend <- filter(x, rep(1/12, 12), sides = 2)

# Q1. ACF and PACF (original series and detrended series)
par(mfrow = c(2, 2))
acf(x)
pacf(x)

d <- na.omit(x - trend)   # detrended series
acf(d)
pacf(d)
par(mfrow = c(1, 1))

# Q2. AR(1) and MA(1) on the detrended series
fit_ar <- arima(d, order = c(1, 0, 0))
fit_ma <- arima(d, order = c(0, 0, 1))

print(fit_ar)
print(fit_ma)

# Coefficients and standard errors
print(fit_ar$coef)
print(sqrt(diag(fit_ar$var.coef)))

print(fit_ma$coef)
print(sqrt(diag(fit_ma$var.coef)))

# AR(1) is stationary if |phi| < 1
if (abs(fit_ar$coef["ar1"]) < 1) print("AR(1) is stationary") else print("AR(1) is NOT stationary")
}
