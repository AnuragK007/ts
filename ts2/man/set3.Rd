\name{set3}
\alias{set3}
\title{Set 3: Seasonal Modeling & Parameter Estimation}
\usage{
set3(topic = NULL, run = FALSE, copy = FALSE)
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
Code for Set 3: Seasonal Modeling & Parameter Estimation, split topic-wise. The full code is in the
Examples section below. Topics in this set:
\itemize{
\item Topic 5 - Seasonal ARIMA (SARIMA) Models & Time Series Regression
\item Topic 6 - Estimation of ARIMA Parameters & Order Selection
}
}
\examples{
######################################################################
# Topic 5 - Seasonal ARIMA (SARIMA) Models & Time Series Regression
######################################################################
library(forecast)

data(AirPassengers)
x <- log(AirPassengers)

# Q1. Seasonality and SARIMA(0,1,1)(0,1,1)[12]
plot(x, main = "Air Passengers Seasonality")
monthplot(x)

sarima <- Arima(x, order = c(0, 1, 1), seasonal = c(0, 1, 1))
print(sarima)

# Q2. Regression with linear trend + monthly dummies
t <- 1:length(x)
month <- factor(cycle(x))
reg <- lm(as.numeric(x) ~ t + month)
print(summary(reg))

# Compare R-squared and RMSE
actual     <- as.numeric(x)
sarima_fit <- as.numeric(fitted(sarima))
reg_fit    <- fitted(reg)

cat("Regression R-squared:", summary(reg)$r.squared, "\\n")
cat("Regression RMSE:", sqrt(mean((actual - reg_fit)^2)), "\\n")
cat("SARIMA RMSE:", sqrt(mean((actual - sarima_fit)^2, na.rm = TRUE)), "\\n")
######################################################################
# Topic 6 - Estimation of ARIMA Parameters & Order Selection
######################################################################
data(AirPassengers)
x <- log(AirPassengers)

# Q1. MLE vs CLS
mle <- arima(x, order = c(1, 1, 1), method = "ML")
cls <- arima(x, order = c(1, 1, 1), method = "CSS")

print(mle)
print(cls)

# Q2. Grid search: p = 0 to 3, q = 0 to 3
results <- data.frame()

for (p in 0:3) {
  for (q in 0:3) {
    m <- try(arima(x, order = c(p, 1, q), method = "ML"), silent = TRUE)
    if (!inherits(m, "try-error"))
      results <- rbind(results, data.frame(p = p, q = q, AIC = AIC(m)))
  }
}

print(results[order(results$AIC), ])
}
