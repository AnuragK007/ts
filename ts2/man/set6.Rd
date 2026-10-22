\name{set6}
\alias{set6}
\title{Set 6: Combined Order Selection & Residual Diagnostics}
\usage{
set6(topic = NULL, run = FALSE, copy = FALSE)
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
Code for Set 6: Combined Order Selection & Residual Diagnostics, split topic-wise. The full code is in the
Examples section below. Topics in this set:
\itemize{
\item Topic 6 - Estimation of ARIMA Parameters (AR & MA Terms)
\item Topic 7 - FPE, AIC, BIC & Residual Analysis (Order Selection)
}
}
\examples{
######################################################################
# Topic 6 - Estimation of ARIMA Parameters (AR & MA Terms)
######################################################################
library(forecast)

data(AirPassengers)
x <- log(AirPassengers)

# Q1. Over-parameterized ARIMA(2,1,2) and t-ratios
fit <- Arima(x, order = c(2, 1, 2))
print(fit)

se <- sqrt(diag(fit$var.coef))
t_ratio <- fit$coef / se
p_value <- 2 * (1 - pnorm(abs(t_ratio)))

result <- data.frame(
  Coefficient = fit$coef,
  T_Ratio = t_ratio,
  P_Value = p_value
)
print(result)

# Q2. Remove one AR and one MA term -> ARIMA(1,1,1)
fit2 <- Arima(x, order = c(1, 1, 1))
print(fit2)

se2 <- sqrt(diag(fit2$var.coef))
print(data.frame(Coefficient = fit2$coef,
                 T_Ratio = fit2$coef / se2,
                 P_Value = 2 * (1 - pnorm(abs(fit2$coef / se2)))))
######################################################################
# Topic 7 - FPE, AIC, BIC & Residual Analysis (Order Selection)
######################################################################
library(forecast)
library(lmtest)

data(AirPassengers)
x <- log(AirPassengers)

# Q1. AIC and BIC for ARIMA(0,1,0) to ARIMA(3,1,3)
results <- data.frame()

for (p in 0:3) {
  for (q in 0:3) {
    fit <- try(Arima(x, order = c(p, 1, q)), silent = TRUE)
    if (!inherits(fit, "try-error"))
      results <- rbind(results, data.frame(
        p = p, q = q,
        AIC = AIC(fit),
        BIC = BIC(fit)
      ))
  }
}

print(results)

# Model with lowest BIC (BIC penalizes extra lags more than AIC)
best <- results[which.min(results$BIC), ]
fit <- Arima(x, order = c(best$p, 1, best$q))
print(fit)

# Q2. Durbin-Watson test and cumulative periodogram of residuals
e <- residuals(fit)

print(dwtest(e ~ 1))
cpgram(e)
}
