#' Information Criteria, Residual Diagnostics, and GARCH
#'
#' Compare ARIMA models with log-likelihood, AIC, BIC, and FPE; inspect residual plots; apply Ljung-Box and ARCH LM tests; and conditionally fit a GARCH(1,1) model.
#'
#' @return Character string containing copyable R code.
#' @export
set4 <- function() {
  paste(
    "data(\"AirPassengers\")",
    "x <- log(AirPassengers)",
    "",
    "m1 <- arima(x, order = c(1, 1, 1))",
    "m2 <- arima(x, order = c(2, 1, 1))",
    "m3 <- arima(x, order = c(0, 1, 2))",
    "models <- list(ARIMA111 = m1, ARIMA211 = m2, ARIMA012 = m3)",
    "",
    "for (m in models) {",
    "  n <- length(x)",
    "  k <- length(m$coef)",
    "  fpe <- m$sigma2 * (n + k) / (n - k)",
    "  cat(\"LogLik:\", as.numeric(logLik(m)), \"AIC:\", AIC(m),",
    "      \"BIC:\", BIC(m), \"FPE:\", fpe, \"\\n\")",
    "}",
    "",
    "best_fit <- models[[which.min(sapply(models, AIC))]]",
    "e <- residuals(best_fit)",
    "acf(e)",
    "hist(e, probability = TRUE)",
    "lines(density(e), col = \"red\")",
    "qqnorm(e)",
    "qqline(e)",
    "",
    "library(FinTS)",
    "print(Box.test(e, lag = 12, type = \"Ljung-Box\"))",
    "print(Box.test(e^2, lag = 12, type = \"Ljung-Box\"))",
    "arch_test <- ArchTest(e, lags = 12)",
    "print(arch_test)",
    "",
    "if (arch_test$p.value < 0.05) {",
    "  library(rugarch)",
    "  spec <- ugarchspec(",
    "    variance.model = list(model = \"sGARCH\", garchOrder = c(1, 1)),",
    "    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)",
    "  )",
    "  garch_fit <- ugarchfit(spec, data = e)",
    "  print(garch_fit)",
    "  plot(sigma(garch_fit)^2, type = \"l\", main = \"GARCH Conditional Variance\")",
    "}",
    sep = "\\n"
  )
}
