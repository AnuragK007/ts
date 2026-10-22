\name{set5}
\alias{set5}
\title{Forecast Evaluation and Model Comparison}
\usage{
set5()
}
\details{
The following listing contains the practical exercises for Set 5.
}
\examples{
\dontrun{
QUESTION 1: Holt-Winters Forecasting & MAPE

data(AirPassengers)
x <- AirPassengers
train <- window(x, end=c(1958,12))
test <- window(x, start=c(1959,1))

model <- HoltWinters(train, seasonal="multiplicative")
fc <- predict(model, n.ahead=24)

plot(x, main="Holt-Winters Forecast")
lines(fc, col="red", lwd=2)
mape <- mean(abs((as.numeric(test)-as.numeric(fc))/as.numeric(test)))*100
print(mape)

m1 <- HoltWinters(train, alpha=0.2, seasonal="multiplicative")
m2 <- HoltWinters(train, alpha=0.8, seasonal="multiplicative")
plot(train, main="Effect of Alpha")
lines(fitted(m1)[,"xhat"], col="blue")
lines(fitted(m2)[,"xhat"], col="red")
legend("topleft", legend=c("Original", "Alpha=0.2", "Alpha=0.8"),
       col=c("black","blue","red"), lty=1)

Interpretation: Interpretation: Lower MAPE indicates better forecast accuracy. A higher alpha generally responds more quickly to recent changes; a lower alpha gives smoother estimates.

QUESTION 2: SARIMA Forecast & Holt-Winters Comparison

library(forecast)
data(AirPassengers)
x <- AirPassengers
train <- window(x, end=c(1958,12))
test <- window(x, start=c(1959,1))

sarima <- Arima(train, order=c(1,1,1),
                seasonal=list(order=c(1,1,0), period=12))
print(sarima)
sf <- forecast(sarima, h=24)
plot(sf, main="SARIMA Forecast")
lines(test, col="red")

hw <- HoltWinters(train, seasonal="multiplicative")
hf <- predict(hw, n.ahead=24)

rmse <- function(a,f) sqrt(mean((as.numeric(a)-as.numeric(f))^2))
mae <- function(a,f) mean(abs(as.numeric(a)-as.numeric(f)))
comparison <- data.frame(
  Model=c("SARIMA", "Holt-Winters"),
  RMSE=c(rmse(test, sf$mean), rmse(test, hf)),
  MAE=c(mae(test, sf$mean), mae(test, hf))
)
print(comparison)

Interpretation: Interpretation: The model with lower RMSE/MAE has better test-set accuracy. RMSE penalizes large errors more heavily than MAE.
}
}
\keyword{documentation}
