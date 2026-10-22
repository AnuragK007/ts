#' ARMA Roots, ADF Test, and ARIMA Forecasting
#'
#' Fit an ARMA(1,1) model, inspect characteristic roots, apply the Augmented Dickey-Fuller test before and after differencing, and forecast 12 months using ARIMA.
#'
#' @return Character string containing copyable R code.
#' @export
set2 <- function() {
  paste(
    "data(\"AirPassengers\")",
    "x <- AirPassengers",
    "",
    "d <- diff(log(x))",
    "m <- arima(d, order = c(1, 0, 1))",
    "print(m)",
    "print(m$coef)",
    "",
    "ar_roots <- polyroot(c(1, -m$coef[\"ar1\"]))",
    "ma_roots <- polyroot(c(1,  m$coef[\"ma1\"]))",
    "print(ar_roots)",
    "print(ma_roots)",
    "if (all(Mod(ar_roots) > 1)) print(\"AR component is stationary\")",
    "if (all(Mod(ma_roots) > 1)) print(\"MA component is invertible\")",
    "",
    "library(tseries)",
    "print(adf.test(x))",
    "print(adf.test(d))",
    "",
    "library(forecast)",
    "model <- arima(log(x), order = c(1, 1, 1))",
    "print(model)",
    "fc <- forecast(model, h = 12)",
    "plot(fc)",
    sep = "\\n"
  )
}
