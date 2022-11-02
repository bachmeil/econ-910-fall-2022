library(vars)
# Estimate RF VAR
# Get F for making forecasts
varfit <- VAR(inf %~% u, 1)
F <- formF(varfit)
F

# Given last forecast, make the next
nextForecast <- function(lastForecast, newinfo) {
  return(F %*% lastForecast)
}

# Make the forecasts using the actual data
forecasts.data <- Reduce(nextForecast, 1:12,
                         init=c(8.48, 3.5, 1),
                         accumulate=TRUE)
# Forecast of inf(T+1) ... inf(T+12)
# Using the data
nochange <- collect(forecasts.data,
                    transform=function(el) {el[1]},
                    output="numeric")
nochange

# Get counterfactual forecast
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

library(nleqslv)
library(tstools)
# Automate the identification of set of acceptable
# values of c
#
# Acceptable: inflation response to unemployment
# rate shock is non-positive for h=1,...,12
irf.solve <- function(b.guess) {
  # Objective function depends on b.guess
  nleq.obj <- function(par) {
    b <- b.guess
    c <- par[1]
    su <- par[2]
    sinf <- par[3]
    dev1 <- su + (b^2)*sinf - 0.173
    dev2 <- (c^2)*su + sinf - 0.2
    dev3 <- c*su + b*sinf + 0.031
    return(c(dev1, dev2, dev3))
  }
  # Solve for b = b.guess
  soln <- nleqslv(c(0.1, 0.1, 0.1), nleq.obj)
  c.value <- soln$x[1]
  
  # Compute counterfactual forecasts using
  # c = c.value
  forecasts.counterfactual <- 
    Reduce(nextForecast, 1:12,
           init=c(8.48+c.value, 4.5, 1),
           accumulate=TRUE)
  # Collect the forecasts of inflation
  counterfactual <- 
    collect(forecasts.counterfactual,
            transform=function(el) {el[1]},
            output="numeric")
  # Calculate the IRF
  counterfactual - nochange
  
  # Is this acceptable?
  # Are all values non-positive?
  return(list(
    check=all( (counterfactual - nochange) < 0.0 ),
    b=b.guess))
}
irf.solve(0.2)
sign.check <- lapply(seq(0, 4.5, by=0.05), irf.solve)
sign.check
# Find all elements where $check is TRUE
foo <- function(element) {
  return(element$check)
}
foo(list(b=0.4, check=TRUE))
Filter(foo, sign.check)
