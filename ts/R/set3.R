#' SARIMA, Regression, and Parameter Estimation
#'
#' Identify annual seasonality, fit a seasonal ARIMA model, compare it with a monthly-dummy regression, contrast ML and CSS estimates, and search candidate ARIMA orders using AIC.
#'
#' @return Character string containing copyable R code.
#' @export
set3 <- function() {
  paste(
    "data(\"AirPassengers\")",
    "x <- log(AirPassengers)",
    "",
    "plot(x, main = \"Air Passengers Seasonality\")",
    "monthplot(x)",
    "",
    "library(forecast)",
    "sarima_fit <- Arima(x, order = c(0, 1, 1), seasonal = c(0, 1, 1))",
    "print(sarima_fit)",
    "",
    "t <- seq_along(x)",
    "month <- factor(cycle(x))",
    "reg_fit <- lm(as.numeric(x) ~ t + month)",
    "print(summary(reg_fit))",
    "",
    "actual <- as.numeric(x)",
    "sarima_fitted <- as.numeric(fitted(sarima_fit))",
    "reg_fitted <- fitted(reg_fit)",
    "cat(\"Regression R-squared:\", summary(reg_fit)$r.squared, \"\\n\")",
    "cat(\"Regression RMSE:\", sqrt(mean((actual - reg_fitted)^2)), \"\\n\")",
    "cat(\"SARIMA RMSE:\", sqrt(mean((actual - sarima_fitted)^2, na.rm = TRUE)), \"\\n\")",
    "",
    "mle_fit <- arima(x, order = c(1, 1, 1), method = \"ML\")",
    "css_fit <- arima(x, order = c(1, 1, 1), method = \"CSS\")",
    "print(mle_fit)",
    "print(css_fit)",
    "",
    "results <- data.frame(p = integer(), q = integer(), AIC = numeric())",
    "for (p in 0:3) {",
    "  for (q in 0:3) {",
    "    fit <- try(arima(x, order = c(p, 1, q), method = \"ML\"), silent = TRUE)",
    "    if (!inherits(fit, \"try-error\"))",
    "      results <- rbind(results, data.frame(p = p, q = q, AIC = AIC(fit)))",
    "  }",
    "}",
    "print(results[order(results$AIC), ])",
    sep = "\\n"
  )
}
