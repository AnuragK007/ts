\name{set2}
\alias{set2}
\title{Set 2: Stationary & Nonstationary Process Modeling}
\usage{
set2(topic = NULL, run = FALSE, copy = FALSE)
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
Code for Set 2: Stationary & Nonstationary Process Modeling, split topic-wise. The full code is in the
Examples section below. Topics in this set:
\itemize{
\item Topic 3 - Autoregressive Moving Average (ARMA)
\item Topic 4 - Nonstationary & Seasonal Time Series Models (ARIMA)
}
}
\examples{
######################################################################
# Topic 3 - Autoregressive Moving Average (ARMA)
######################################################################
data(AirPassengers)
x <- AirPassengers

# Q1. ARMA(1,1) on the differenced log series
d <- diff(log(x))
m <- arima(d, order = c(1, 0, 1))
print(m)

# Estimated equation
print(m$coef)
cat("d_t - mu =", round(m$coef["ar1"], 4), "* (d_t-1 - mu) + e_t +",
    round(m$coef["ma1"], 4), "* e_t-1 ,  mu =", round(m$coef["intercept"], 4), "\\n")

# Q2. Roots of the AR and MA polynomials
ar_root <- polyroot(c(1, -m$coef["ar1"]))
ma_root <- polyroot(c(1,  m$coef["ma1"]))

print(ar_root)
print(ma_root)

if (all(Mod(ar_root) > 1)) print("AR is stationary") else print("AR is NOT stationary")
if (all(Mod(ma_root) > 1)) print("MA is invertible") else print("MA is NOT invertible")
######################################################################
# Topic 4 - Nonstationary & Seasonal Time Series Models (ARIMA)
######################################################################
library(tseries)
library(forecast)

data(AirPassengers)
x <- AirPassengers

# Q1. ADF test on raw data, then on the differenced log series
adf.test(x)

d <- diff(log(x))
adf.test(d)

# Q2. ARIMA(1,1,1) on the log series and 12-month forecast with 95\% CI
model <- arima(log(x), order = c(1, 1, 1))
print(model)

f <- forecast(model, h = 12, level = 95)
plot(f)
}
