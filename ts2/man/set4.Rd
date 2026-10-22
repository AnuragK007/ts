\name{set4}
\alias{set4}
\title{Set 4: Information Criteria & Volatility Modeling}
\usage{
set4(topic = NULL, run = FALSE, copy = FALSE)
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
Code for Set 4: Information Criteria & Volatility Modeling, split topic-wise. The full code is in the
Examples section below. Topics in this set:
\itemize{
\item Topic 7 - FPE, AIC, BIC & Residual Analysis
\item Topic 8 - Diagnostic Checking, ARCH & GARCH Models
}
}
\examples{
######################################################################
# Topic 7 - FPE, AIC, BIC & Residual Analysis
######################################################################
data(AirPassengers)
x <- log(AirPassengers)

# Q1. Compare ARIMA(1,1,1), (2,1,1), (0,1,2)
m1 <- arima(x, c(1, 1, 1))
m2 <- arima(x, c(2, 1, 1))
m3 <- arima(x, c(0, 1, 2))

models <- list(m1, m2, m3)
n <- length(x)

tab <- data.frame(
  Model  = c("ARIMA(1,1,1)", "ARIMA(2,1,1)", "ARIMA(0,1,2)"),
  LogLik = sapply(models, function(m) as.numeric(logLik(m))),
  AIC    = sapply(models, AIC),
  BIC    = sapply(models, BIC),
  FPE    = sapply(models, function(m) {
    k <- length(m$coef)
    m$sigma2 * (n + k) / (n - k)
  })
)
print(tab)

# Q2. Residuals of the best model (by AIC)
fit <- models[[which.min(sapply(models, AIC))]]
e <- residuals(fit)

par(mfrow = c(2, 2))
acf(e)
hist(e, probability = TRUE)
lines(density(e), col = "red")
qqnorm(e)
qqline(e)
par(mfrow = c(1, 1))
######################################################################
# Topic 8 - Diagnostic Checking, ARCH & GARCH Models
######################################################################
library(FinTS)
library(rugarch)

data(AirPassengers)
x <- log(AirPassengers)

fit <- arima(x, c(1, 1, 1))
e <- residuals(fit)

# Q1. Ljung-Box on residuals and squared residuals
Box.test(e,   lag = 12, type = "Ljung-Box")
Box.test(e^2, lag = 12, type = "Ljung-Box")

# Q2. Engle's ARCH LM test, then GARCH(1,1) if ARCH effect is present
test <- ArchTest(e, lags = 12)
print(test)

if (test$p.value < 0.05) {
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
  )

  g <- ugarchfit(spec, data = e)
  print(g)
  plot(sigma(g)^2, type = "l", main = "GARCH Conditional Variance")
} else {
  print("No ARCH effect found, GARCH not needed.")
}
}
