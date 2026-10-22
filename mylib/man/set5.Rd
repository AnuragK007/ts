\name{set5}
\alias{set5}
\title{Time Series Analysis Practical Codebook — Set 5}
\description{
Complete source code for both practical questions in Set 5. The code is displayed as text for reading and copying; it is not run by opening this Help page.
}
\details{
\preformatted{
QUESTION 1: Holt-Winters Forecasting \& MAPE
Complete R Code:
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
Brief Interpretation: Interpretation: Lower MAPE indicates better forecast accuracy. A higher alpha generally responds more quickly to recent changes; a lower alpha gives smoother estimates.

QUESTION 2: SARIMA Forecast \& Holt-Winters Comparison
Complete R Code:
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

rmse <- function(a,f) sqrt(mean((as.numeric(a)-as.numeric(f))\^{}2))
mae <- function(a,f) mean(abs(as.numeric(a)-as.numeric(f)))
comparison <- data.frame(
  Model=c("SARIMA", "Holt-Winters"),
  RMSE=c(rmse(test, sf\$mean), rmse(test, hf)),
  MAE=c(mae(test, sf\$mean), mae(test, hf))
)
print(comparison)
Brief Interpretation: Interpretation: The model with lower RMSE/MAE has better test-set accuracy. RMSE penalizes large errors more heavily than MAE.
}
}
\usage{
set5()
}
\value{
Opens the Help page for Set 5. No practical code is executed.
}
\seealso{
\code{help(package = "mylib")}
}
