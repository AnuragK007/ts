#' ARIMA Significance, Order Selection, and Residual Diagnostics
#'
#' Inspect coefficient significance in ARIMA(2,1,2), compare candidate ARIMA orders using AIC and BIC, and examine residuals with the Durbin-Watson test and cumulative periodogram.
#'
#' @return Character string containing copyable R code.
#' @export
set6 <- function() {
  paste(
    "library(forecast)",
    "data(\"AirPassengers\")",
    "x <- log(AirPassengers)",
    "",
    "fit <- Arima(x, order = c(2, 1, 2))",
    "print(fit)",
    "se <- sqrt(diag(fit$var.coef))",
    "t_ratio <- fit$coef / se",
    "p_value <- 2 * pnorm(abs(t_ratio), lower.tail = FALSE)",
    "print(data.frame(Coefficient = fit$coef, Standard_Error = se,",
    "                 T_Ratio = t_ratio, P_Value = p_value))",
    "",
    "fit_small <- Arima(x, order = c(1, 1, 1))",
    "print(fit_small)",
    "",
    "results <- data.frame(p = integer(), q = integer(),",
    "                      AIC = numeric(), BIC = numeric())",
    "for (p in 0:3) {",
    "  for (q in 0:3) {",
    "    candidate <- try(Arima(x, order = c(p, 1, q)), silent = TRUE)",
    "    if (!inherits(candidate, \"try-error\")) {",
    "      results <- rbind(results, data.frame(",
    "        p = p, q = q, AIC = AIC(candidate), BIC = BIC(candidate)))",
    "    }",
    "  }",
    "}",
    "print(results)",
    "",
    "best <- results[which.min(results$BIC), ]",
    "best_fit <- Arima(x, order = c(best$p, 1, best$q))",
    "print(best_fit)",
    "",
    "e <- residuals(best_fit)",
    "library(lmtest)",
    "print(dwtest(e ~ 1))",
    "cpgram(e)",
    sep = "\\n"
  )
}
