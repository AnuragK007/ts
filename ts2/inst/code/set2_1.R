# TITLE: Topic 3 - Autoregressive Moving Average (ARMA)

data(AirPassengers)
x <- AirPassengers

# Q1. ARMA(1,1) on the differenced log series
d <- diff(log(x))
m <- arima(d, order = c(1, 0, 1))
print(m)

# Estimated equation
print(m$coef)
cat("d_t - mu =", round(m$coef["ar1"], 4), "* (d_t-1 - mu) + e_t +",
    round(m$coef["ma1"], 4), "* e_t-1 ,  mu =", round(m$coef["intercept"], 4), "\n")

# Q2. Roots of the AR and MA polynomials
ar_root <- polyroot(c(1, -m$coef["ar1"]))
ma_root <- polyroot(c(1,  m$coef["ma1"]))

print(ar_root)
print(ma_root)

if (all(Mod(ar_root) > 1)) print("AR is stationary") else print("AR is NOT stationary")
if (all(Mod(ma_root) > 1)) print("MA is invertible") else print("MA is NOT invertible")
