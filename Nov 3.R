library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")

library(vars)
dataset <- ts.combine(inf, u)
varfit <- VAR(dataset, 1)
F <- formF(varfit)

nextForecast <- function(lastForecast, newinfo) {
  return(F %*% lastForecast)
}

forecasts.data <- Reduce(nextForecast, 1:12,
                         init=c(8.48, 3.5, 1),
                         accumulate=TRUE)
# Forecast of inf(T+1) ... inf(T+12)
# Using the data
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
# Effect
counterfactual - nochange

# What if the intercept is zero?
F <- formF(varfit, constant=FALSE)
F

forecasts.data <- Reduce(nextForecast, 1:12,
                         init=c(8.48, 3.5),
                         accumulate=TRUE)
nochange <- collect(forecasts.data,
                    transform=function(el) {el[1]},
                    output="numeric")
forecasts.counterfactual <- Reduce(nextForecast, 1:12,
                                   init=c(8.30, 4.5),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[1]},
                          output="numeric")
counterfactual - nochange

# Linear model
# What matters is the change, not the starting point
forecasts.data <- Reduce(nextForecast, 1:12,
                         init=c(0, 0),
                         accumulate=TRUE)
nochange <- collect(forecasts.data,
                    transform=function(el) {el[1]},
                    output="numeric")
forecasts.counterfactual <- Reduce(nextForecast, 1:12,
                                   init=c(-0.18, 1.0),
                                   accumulate=TRUE)
counterfactual <- collect(forecasts.counterfactual,
                          transform=function(el) {el[1]},
                          output="numeric")
counterfactual - nochange
# All that matters is the change in the variables, not the
# starting point, so this is the same as before.
nochange
counterfactual
# In a linear model, the counterfactual forecast is the IRF
# if the intercept is zero
# and the lagged variable values are zero.





