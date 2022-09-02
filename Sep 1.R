library(AER)
library(tstools)
data(USMacroG)
dgdp <- 400*pctChange(USMacroG[,"gdp"])
arma11 <- arima(dgdp, order=c(1,0,1))
arma11
BIC(arma11)

criteria <- function(pq) {
  fit <- arima(dgdp, order=c(pq[1], 0, pq[2]))
  list(aic=AIC(fit), sic=BIC(fit), ar.lags=pq[1], ma.lags=pq[2])
}
criteria(c(1,1))
criteria(c(2,2))
potential.lags <- expand.grid(0:4, 0:4)
potential.lags

# Send each row of potential.lags to criteria function
# and save the output
apply(potential.lags, MARGIN=1, criteria)
infcrit <- apply(potential.lags, MARGIN=1, criteria)
infcrit[[13]]
length(infcrit)
# Return the aic value for one element
aic.value <- function(crit) { crit$aic }
aic.value(infcrit[[13]])
# Go over all combinations we tried
aic.values <- lapply(infcrit, aic.value)
aic.values
which.min(aic.values)
infcrit[[which.min(aic.values)]]
# ARMA(1,0) or AR(1) is the best possible model



arma11 <- arima(dgdp, order=c(1,0,1))
arma12 <- arima(dgdp, order=c(1,0,2))
AIC(arma11)
AIC(arm)
