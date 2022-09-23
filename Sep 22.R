# Simulate from ARMA model
arima.sim(list(ar=c(0.795,0.1), ma=c(0.3, 0.1, 0.2)), n=97)
irf <- ARMAtoMA(ar=c(0.6, 0.2), lag.max=100)
irf
which(irf < 0.5)
min(which(irf < 0.5))
library(tstools)
set.seed(100)
output <- replicate(5000, {
  esim <- arima.sim(list(ar=0.795), n=97)
  # Estimate AR(1) model
  fit <- tsreg(esim, lags(esim,1))
  # Calculate half-life
  log(0.5)/log(coefficients(fit)[2])
})
plot(density(output))
quantile(output, probs=c(0.025, 0.975))

# Convert it to a function
ar1.bootstrap <- function(a) {
  set.seed(100)
  output <- replicate(5000, {
    esim <- arima.sim(list(ar=a), n=97)
    fit <- tsreg(esim, lags(esim,1))
    log(0.5)/log(coefficients(fit)[2])
  })
  quantile(output, probs=c(0.025, 0.975))
}
ar1.bootstrap(0.795)
ar1.bootstrap(0.595)
lapply(c(0.795, 0.595, 0.771, 0.889, 0.793, 0.867), 
       ar1.bootstrap)

arp.bootstrap <- function(a) {
  set.seed(100)
  output <- replicate(5000, {
    # This stays the same
    esim <- arima.sim(list(ar=a), n=97)
    # Do AR(p) estimation, inferring the lag length
    fit <- tsreg(esim, lags(esim,1:length(a)))
    # Can't use this
    # log(0.5)/log(coefficients(fit)[2])
    # Drop the intercept when calculating the IRF
    irf <- ARMAtoMA(ar=coefficients(fit)[-1], lag.max=100)
    tmp <- which(irf < 0.5)
    if (length(tmp) == 0) { 100 } else { min(tmp) }
  })
  quantile(output, probs=c(0.025, 0.975))
}
arp.bootstrap(c(0.7, 0.2, 0.05))
arp.bootstrap(c(0.3, 0.1, 0.1, 0.1))
# Persistence of any AR(p) process
# And you can do bootstrapping
