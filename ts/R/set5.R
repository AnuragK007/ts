#' Holt-Winters and SARIMA Forecast Comparison
#'
#' Split AirPassengers into training and test periods, forecast using multiplicative Holt-Winters smoothing, compare smoothing parameters, and compare Holt-Winters with SARIMA using RMSE and MAE.
#'
#' @return Character string containing copyable R code.
#' @export
set5 <- function() {
  paste(
    "library(forecast)",
    "data(\"AirPassengers\")",
    "",
    "train <- window(AirPassengers, end = c(1958, 12))",
    "test <- window(AirPassengers, start = c(1959, 1))",
    "",
    "hw_fit <- HoltWinters(train, seasonal = \"multiplicative\")",
    "hw_fc <- forecast(hw_fit, h = 24)",
    "plot(hw_fc)",
    "mape <- mean(abs((test - hw_fc$mean) / test)) * 100",
    "cat(\"Holt-Winters MAPE =\", mape, \"%\\n\")",
    "",
    "fit_alpha_02 <- HoltWinters(train, alpha = 0.2, beta = FALSE, gamma = FALSE)",
    "fit_alpha_08 <- HoltWinters(train, alpha = 0.8, beta = FALSE, gamma = FALSE)",
    "plot(train, main = \"Effect of Alpha\")",
    "lines(fitted(fit_alpha_02)[, \"xhat\"], col = \"blue\")",
    "lines(fitted(fit_alpha_08)[, \"xhat\"], col = \"red\")",
    "legend(\"topleft\", legend = c(\"Alpha 0.2\", \"Alpha 0.8\"),",
    "       col = c(\"blue\", \"red\"), lty = 1)",
    "",
    "sarima_fit <- Arima(train, order = c(1, 1, 1),",
    "                    seasonal = list(order = c(1, 1, 0), period = 12))",
    "sarima_fc <- forecast(sarima_fit, h = 24)",
    "plot(sarima_fc)",
    "",
    "cat(\"Holt-Winters RMSE:\", sqrt(mean((test - hw_fc$mean)^2)), \"\\n\")",
    "cat(\"Holt-Winters MAE:\", mean(abs(test - hw_fc$mean)), \"\\n\")",
    "cat(\"SARIMA RMSE:\", sqrt(mean((test - sarima_fc$mean)^2)), \"\\n\")",
    "cat(\"SARIMA MAE:\", mean(abs(test - sarima_fc$mean)), \"\\n\")",
    sep = "\\n"
  )
}
