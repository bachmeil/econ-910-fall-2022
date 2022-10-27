# For nleqslv, return a vector
# First element: Deviation from first equation
# Need three elements because three equations
nleq.obj <- function(par) {
  b <- 0
  c <- par[1]
  su <- par[2]
  sinf <- par[3]
  dev1 <- su + (b^2)*sinf - 0.173
  dev2 <- (c^2)*su + sinf - 0.2
  dev3 <- c*su + b*sinf + 0.031
  return(c(dev1, dev2, dev3))
}
nleq.obj(c(0,0,0))

library(nleqslv)
nleqslv(c(0.1, 0.1, 0.1), nleq.obj)

# Estimation from Tuesday to get the residuals
library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")

# Estimate the reduced form VAR
rhs <- lags(inf %~% u, 1)
fit.inf <- tsreg(inf, rhs)
fit.u <- tsreg(u, rhs)

# Covariance matrix of the residuals
inf.res <- fit.inf$resids
u.res <- fit.u$resids
res <- cbind(inf.res, u.res)

gmm.obj <- function(par, data) {
  inf.res <- data[, "inf.res"]
  u.res <- data[, "u.res"]
  b <- 0
  c <- par[1]
  su <- par[2]
  sinf <- par[2]
  dev1 <- u.res^2 + b^2*(inf.res^2) - 0.173
  dev2 <- c^2*(u.res^2) + inf.res^2 - 0.2
  dev3 <- c*(u.res^2) + b*(inf.res^2) + 0.031
  return(ts(cbind(dev1, dev2, dev3)))
}
gmm.obj(c(0.1, 0.1, 0.1), res)
library(gmm)
gmm(gmm.obj, res, c(0, 0.1, 0.1))

library(vars)
varfit <- VAR(inf %~% u, 1)
F <- formF(varfit)
F

nextForecast <- function(lastForecast, newinfo) {
  return(F %*% lastForecast)
}

forecasts.data <- Reduce(nextForecast, 1:12,
                         init=c(8.48, 3.5, 1),
                         accumulate=TRUE)

nochange <- collect(forecasts.data,
                    transform=function(el) {el[1]},
                    output="numeric")
nochange

forecasts.counterfactual <- Reduce(nextForecast, 1:12,
                         init=c(8.30, 4.5, 1),
                         accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                    transform=function(el) {el[1]},
                    output="numeric")
counterfactual

irf <- counterfactual - nochange
irf
# If c=0, IRF is always negative
# This is acceptable

nleq.obj <- function(par) {
  b <- par[1]
  c <- 0.02
  su <- par[2]
  sinf <- par[3]
  dev1 <- su + (b^2)*sinf - 0.173
  dev2 <- (c^2)*su + sinf - 0.2
  dev3 <- c*su + b*sinf + 0.031
  return(c(dev1, dev2, dev3))
}
nleqslv(c(0.1, 0.1, 0.1), nleq.obj)

forecasts.counterfactual <- Reduce(nextForecast, 
                            1:12,
                            init=c(8.31, 4.5, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[1]},
                          output="numeric")
counterfactual

irf <- counterfactual - nochange
irf
# Acceptable


nleq.obj <- function(par) {
  b <- par[1]
  c <- 0.65
  su <- par[2]
  sinf <- par[3]
  dev1 <- su + (b^2)*sinf - 0.173
  dev2 <- (c^2)*su + sinf - 0.2
  dev3 <- c*su + b*sinf + 0.031
  return(c(dev1, dev2, dev3))
}
nleqslv(c(0.1, 0.1, 0.1), nleq.obj)

forecasts.counterfactual <- Reduce(nextForecast, 
                                   1:12,
                                   init=c(7.83, 4.5, 1),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[1]},
                          output="numeric")
counterfactual

irf <- counterfactual - nochange
irf
# Acceptable
