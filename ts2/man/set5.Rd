\name{set5}
\alias{set5}
\title{Set 5: Smoothing & Seasonal Forecasting}
\usage{
set5(topic = NULL, run = FALSE, copy = FALSE)
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
Code for Set 5: Smoothing & Seasonal Forecasting, split topic-wise. The full code is in the
Examples section below. Topics in this set:
\itemize{
\item Topic 1 - Exponential, Moving Average & Holt-Winters Smoothing (Forecasting)
\item Topic 5 - Seasonal ARIMA (SARIMA) Models (Forecast Comparison)
}
}
\examples{
######################################################################
# Topic 1 - Exponential, Moving Average & Holt-Winters Smoothing (Forecasting)
######################################################################
library(forecast)

data(AirPassengers)

# Training (1949-1958) and test (1959-1960) data
train <- window(AirPassengers, end = c(1958, 12))
test  <- window(AirPassengers, start = c(1959, 1))

# Q1. Holt-Winters multiplicative forecast for 2 years
fit <- HoltWinters(train, seasonal = "multiplicative")
pred <- forecast(fit, h = 24)
plot(pred)

# MAPE
mape <- mean(abs((test - pred$mean) / test)) * 100
cat("MAPE =", mape, "\%\\n")

# Q2. Compare alpha = 0.2 and alpha = 0.8
fit1 <- HoltWinters(train, alpha = 0.2, beta = FALSE, gamma = FALSE)
fit2 <- HoltWinters(train, alpha = 0.8, beta = FALSE, gamma = FALSE)

plot(train)
lines(fitted(fit1)[, "xhat"], col = "blue")
lines(fitted(fit2)[, "xhat"], col = "red")

legend("topleft", legend = c("Alpha 0.2", "Alpha 0.8"),
       col = c("blue", "red"), lty = 1)
######################################################################
# Topic 5 - Seasonal ARIMA (SARIMA) Models (Forecast Comparison)
######################################################################
library(forecast)

data(AirPassengers)

train <- window(AirPassengers, end = c(1958, 12))
test  <- window(AirPassengers, start = c(1959, 1))

# Holt-Winters forecast (from Topic 1)
hw_fit  <- HoltWinters(train, seasonal = "multiplicative")
hw_pred <- forecast(hw_fit, h = 24)

# Q1. SARIMA(1,1,1)(1,1,0)[12] on the training set
fit <- Arima(train,
             order = c(1, 1, 1),
             seasonal = list(order = c(1, 1, 0), period = 12))
print(fit)

# Q2. Forecast for 1959-1960
sarima_pred <- forecast(fit, h = 24)
plot(sarima_pred)

# Accuracy comparison
hw_rmse <- sqrt(mean((test - hw_pred$mean)^2))
hw_mae  <- mean(abs(test - hw_pred$mean))

sarima_rmse <- sqrt(mean((test - sarima_pred$mean)^2))
sarima_mae  <- mean(abs(test - sarima_pred$mean))

cat("Holt-Winters RMSE:", hw_rmse, "\\n")
cat("Holt-Winters MAE:", hw_mae, "\\n")

cat("SARIMA RMSE:", sarima_rmse, "\\n")
cat("SARIMA MAE:", sarima_mae, "\\n")
}
