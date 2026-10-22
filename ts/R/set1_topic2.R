#' ACF, PACF, AR(1), and MA(1) Models
#'
#' Inspect the autocorrelation and partial autocorrelation functions, detrend the passenger series using a 12-month moving average, and fit AR(1) and MA(1) models with coefficient standard errors.
#'
#' @return Character string containing copyable R code.
#' @export
set1_topic2 <- function() {
  paste(
    "data(\"AirPassengers\")",
    "x <- AirPassengers",
    "",
    "trend <- stats::filter(x, rep(1 / 12, 12), sides = 2)",
    "par(mfrow = c(2, 2))",
    "acf(x, main = \"ACF: Original Series\")",
    "pacf(x, main = \"PACF: Original Series\")",
    "",
    "d <- na.omit(x - trend)",
    "acf(d, main = \"ACF: Detrended Series\")",
    "pacf(d, main = \"PACF: Detrended Series\")",
    "par(mfrow = c(1, 1))",
    "",
    "ar_fit <- arima(d, order = c(1, 0, 0))",
    "ma_fit <- arima(d, order = c(0, 0, 1))",
    "print(ar_fit)",
    "print(ma_fit)",
    "",
    "print(ar_fit$coef)",
    "print(sqrt(diag(ar_fit$var.coef)))",
    "print(ma_fit$coef)",
    "print(sqrt(diag(ma_fit$var.coef)))",
    "",
    "phi1 <- unname(ar_fit$coef[\"ar1\"])",
    "cat(\"AR(1) stationary:\", abs(phi1) < 1, \"\\n\")",
    sep = "\\n"
  )
}
