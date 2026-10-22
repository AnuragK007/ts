\name{set6}
\alias{set6}
\title{Time Series Analysis Practical Codebook — Set 6}
\description{
Complete source code for both practical questions in Set 6. The code is displayed as text for reading and copying; it is not run by opening this Help page.
}
\details{
\preformatted{
QUESTION 1: ARIMA Parameter Significance \& Simplification
Complete R Code:
data(AirPassengers)
x <- log(AirPassengers)
model <- arima(x, order=c(2,1,2))
print(model)

se <- sqrt(diag(model\$var.coef))
t\_value <- model\$coef/se
p\_value <- 2*pnorm(-abs(t\_value))
print(data.frame(Coefficient=model\$coef, SE=se,
                 t\_value=t\_value, p\_value=p\_value))

candidates <- list(
  c(2,1,2), c(1,1,2), c(2,1,1), c(1,1,1),
  c(2,1,0), c(0,1,2), c(1,1,0), c(0,1,1)
)
fits <- lapply(candidates, function(o)
  try(arima(x, order=o), silent=TRUE))
ok <- !sapply(fits, inherits, "try-error")
fits <- fits[ok]
orders <- candidates[ok]
aics <- sapply(fits, AIC)
print(data.frame(
  Model=sapply(orders, function(o) paste0("ARIMA(", paste(o, collapse=","), ")")),
  AIC=aics
))
print(fits[[which.min(aics)]])
Brief Interpretation: Interpretation: Approximate p < 0.05 indicates a statistically significant coefficient. Compare simpler candidates and residual diagnostics before choosing a parsimonious model; AIC alone does not prove coefficient significance.

QUESTION 2: AIC/BIC Grid Search, Durbin-Watson \& Cumulative Periodogram
Complete R Code:
library(lmtest)
data(AirPassengers)
x <- log(AirPassengers)
results <- data.frame()

for(p in 0:3)\{
  for(q in 0:3)\{
    m <- try(arima(x, order=c(p,1,q)), silent=TRUE)
    if(!inherits(m, "try-error"))\{
      results <- rbind(results, data.frame(p=p, q=q,
                                           AIC=AIC(m), BIC=BIC(m)))
    \}
  \}
\}
print(results)
best <- results[which.min(results\$BIC), ]
print(best)

model <- arima(x, order=c(best\$p,1,best\$q))
res <- na.omit(as.numeric(residuals(model)))
dwtest(res \~{} 1)
cpgram(res, main="Cumulative Periodogram")
Brief Interpretation: Interpretation: Lower AIC/BIC is preferred. BIC penalizes complexity more strongly for this sample size. Durbin-Watson near 2 suggests little first-order autocorrelation; the cumulative periodogram helps assess residual periodicity.
}
}
\usage{
set6()
}
\value{
Opens the Help page for Set 6. No practical code is executed.
}
\seealso{
\code{help(package = "mylib")}
}
