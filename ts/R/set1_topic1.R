#' Moving Average and Exponential Smoothing
#'
#' Plot AirPassengers, calculate a 12-month centered moving average, and compare simple exponential smoothing, Holt's linear trend method, and additive and multiplicative Holt-Winters smoothing.
#'
#' @return Character string containing copyable R code.
#' @export
set1_topic1 <- function() {
  paste(
    "data(\"AirPassengers\")",
    "x <- AirPassengers",
    "",
    "# Moving average",
    "plot(x, main = \"Air Passengers\")",
    "trend <- stats::filter(x, rep(1 / 12, 12), sides = 2)",
    "lines(trend, col = \"red\")",
    "",
    "# Exponential smoothing",
    "library(forecast)",
    "ses_fit <- ses(x)",
    "holt_fit <- holt(x)",
    "add_fit <- hw(x, seasonal = \"additive\")",
    "mul_fit <- hw(x, seasonal = \"multiplicative\")",
    "",
    "plot(x, main = \"Smoothing Comparison\")",
    "lines(fitted(ses_fit), col = \"red\")",
    "lines(fitted(holt_fit), col = \"blue\")",
    "lines(fitted(add_fit), col = \"green\")",
    "lines(fitted(mul_fit), col = \"purple\")",
    "",
    "print(ses_fit$model$par)",
    "print(holt_fit$model$par)",
    "print(add_fit$model$par)",
    "print(mul_fit$model$par)",
    sep = "\\n"
  )
}
