library(tstools)

u <- import.fred("unrate.csv")
rhs.linear <- lags(u, 1)
fit.linear <- tsreg(u, rhs.linear)
fit.linear

# Identify the low unemployment regime
# u(t-1) < 6%
d <- (lags(u, 1) < 6)
d
mean(u)
# Be sure to include d as a regressor
# Allows the intercept to switch across regimes
rhs.nonlinear <- ts.combine(lags(u, 1), d, d*lags(u, 1))
fit.nonlinear <- tsreg(u, rhs.nonlinear)
fit.nonlinear

# Close to unit root
# Work with the first difference
du <- u - lags(u,1)
rhs.linear <- lags(du, 1)
fit.linear <- tsreg(du, rhs.linear)
fit.linear
# Movements in the change in u are random fluctuations with
# no persistence
rhs.nonlinear <- ts.combine(lags(du, 1), d, d*lags(du, 1))
fit.nonlinear <- tsreg(du, rhs.nonlinear)
fit.nonlinear
# These dynamics are *very* different
u.sim <- 4
du.sim <- 0.00

res <- residuals(fit.nonlinear)
res.sim <- sample(res, size=2, replace=TRUE)

# Given this, calculate du(T+1)
du.next <- if (last(u.sim) > 6) {
  -0.04 - 0.04*last(du.sim) + res[1]
} else {
  0.03 + 0.63*last(du.sim) + res[1]
}
du.next
du.sim <- c(du.sim, du.next)
# To know the regime
u.sim <- c(u.sim, last(u.sim) + du.next)
# du(T+2)
du.next <- if (last(u.sim) > 6) {
  -0.04 - 0.04*last(du.sim) + res[2]
} else {
  0.03 + 0.63*last(du.sim) + res[2]
}
du.next
du.sim <- c(du.sim, du.next)
u.sim <- c(u.sim, last(u.sim) + du.next)
du.sim
u.sim  
  
  
  
