library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")
rhs <- lags(inf %~% u, 1)
rhs
fit.inf <- tsreg(inf, rhs)
fit.u <- tsreg(u, rhs)
# Can use these equations to forecast
last(u)
last(inf)
# Pretend we don't have the Aug 2022 u value
# Grab the July 2022 value
tsobs(u, c(2022,7))
0.12 + 0.99*8.48 - 0.01*3.5
0.15 + 0.01*8.48 + 0.97*3.5
# Could use these forecasts to forecast Sep 2022
library(vars)
dataset <- ts.combine(inf, u)
varfit <- VAR(dataset, p=1)
varfit
F <- formF(varfit)
F
# 1-step ahead predictions
z <- c(8.48, 3.5, 1)
F %*% z
F %*% F %*% z
nextForecast <- function(lastForecast, newinfo) {
  return(F %*% lastForecast)
}
nextForecast(F %*% z, 1)
forecasts <- Reduce(nextForecast, 1:12, init=z, accumulate=TRUE)
forecasts
?VARselect
VARselect(dataset, lag.max=12)$selection["AIC(n)"]
# We should be using a VAR(2)
varfit <- VAR(dataset, p=2)
F <- formF(varfit)
z <- c(8.48, 3.5, 8.99, 3.6, 1)
F %*% z
# 6-step ahead forecast of inflation using direct estimation
rhs6 <- lags(inf %~% u, 6)
fit6 <- tsreg(inf, rhs6)
0.62848 + 0.85*(8.48) - 0.02*(3.5)
# Jan 2023 forecast of inflation is 7.77% using a 6-step
# ahead projection approach
last(residuals(fit.u))
last(fit.u$fitted)
cor(fit.inf$resids %~% fit.u$resids)


