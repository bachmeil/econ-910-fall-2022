library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")
rhs <- lags(inf %~% u, 1)
fit.inf <- tsreg(inf, rhs)
fit.inf
fit.u <- tsreg(u, rhs)
fit.u
cor(fit.u$resids %~% fit.inf$resids)

# Conventional IRF computations
library(vars)
dataset <- ts.combine(inf, u)
varfit <- VAR(dataset, 1)
F <- formF(varfit)
F

nextForecast <- function(lastForecast, newinfo) {
  return(F %*% lastForecast)
}
last(inf)
tsobs(u, c(2022,7))
# Make forecasts of inflation Aug 2022 through July 2023
forecasts.data <- Reduce(nextForecast, 1:12,
                         init=c(8.48, 3.5, 1),
                         accumulate=TRUE)
forecasts.data
# Pull the first element and put in a vector
nochange <- collect(forecasts.data,
                    transform=function(el) {el[1]},
                    output="numeric")
nochange
# Now forecast inflation if a shock causes u to rise to 4.5
# What happens to inflation at time T?
# Assume the shock has no direct effect on inflation
# Assume u has no indirect effect on inflation
forecasts.highu <- Reduce(nextForecast, 1:12,
                         init=c(8.48, 4.5, 1),
                         accumulate=TRUE)
forecasts.highu
highu <- collect(forecasts.highu,
                 transform=function(el) {el[1]},
                 output="numeric")
highu
irf <- highu - nochange
irf
plot(ts(irf))
tsreg(inf, lags(u,0:1) %~% lags(inf,1))
# Inflation falls about 0.18 points as u rises 1 point
forecasts.highu2 <- Reduce(nextForecast, 1:12,
                          init=c(8.30, 4.5, 1),
                          accumulate=TRUE)
forecasts.highu2
highu2 <- collect(forecasts.highu2,
                 transform=function(el) {el[1]},
                 output="numeric")
irf2 <- highu2 - nochange
irf2
irf
# Both of these systems is recursive, but we've reversed
# the contemporaneous effect


# Local projections
# Direct estimation (h-step ahead projection)
fit1 <- tsreg(inf, lags(inf %~% u, 1))
fit1
fit2 <- tsreg(inf, lags(inf %~% u, 2))
fit2
fit3 <- tsreg(inf, lags(inf %~% u, 3))
fit3
0.24361 + 0.96563*8.48 - 0.02191*3.5
0.24361 + 0.96563*8.48 - 0.02191*4.5

lpreg <- function(h) {
  tsreg(inf, lags(inf %~% u, h))
}
lpreg(1)
lpreg(2)
lpregs <- lapply(1:12, lpreg)
lpregs
lpcoef <- collect(lpregs,
                  transform=function(el) {coefficients(el)},
                  output="rows")
lpcoef  
lp.data <- lpcoef %*% matrix(c(8.48, 3.5, 1))  
lp.data  
lp.highu <- lpcoef %*% matrix(c(8.48, 4.5, 1))  
lp.highu  
lp.highu2 <- lpcoef %*% matrix(c(8.30, 4.5, 1))
lp.highu2
irf.highu <- lp.highu - lp.data
irf.highu
irf.highu2 <- lp.highu2 - lp.data
irf.highu2
# Are the residuals correlated?
# If about zero, don't worry about identification.
