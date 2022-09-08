# Spurious regression
# Uncorrelated x and y
# y = a + bx => b = 0, adj R-squared about 0
set.seed(100)
y <- rnorm(100)
x <- rnorm(100)
fit <- lm(y ~ x)
names(summary(fit))
summary(fit)$adj.r.squared

set.seed(100)
b <- 0.5
ar1 <- function(y.last, newinfo) {
  return(b*y.last + newinfo)
}
library(tstools)
simoutput.stationary <- replicate(500, {
  e.x <- rnorm(500)
  e.y <- rnorm(500)
  simx <- ts(Reduce(ar1, e.x, init=0.0, accumulate=TRUE))
  simy <- ts(Reduce(ar1, e.y, init=0.0, accumulate=TRUE))
  fit <- tsreg(simy, simx)
  summary(fit)$adj.r.squared
}, simplify="array")
plot(density(simoutput.stationary))

# Now keep it the same, but with unit roots
# in x and y
set.seed(100)
b <- 1.0
simoutput.nonstationary <- replicate(500, {
  e.x <- rnorm(500)
  e.y <- rnorm(500)
  simx <- ts(Reduce(ar1, e.x, init=0.0, accumulate=TRUE))
  simy <- ts(Reduce(ar1, e.y, init=0.0, accumulate=TRUE))
  fit <- tsreg(simy, simx)
  summary(fit)$adj.r.squared
}, simplify="array")
plot(density(simoutput.nonstationary))

# Calculate IRF of unit root process
# Intercept = 0
# Lagged values are zero at time of the shock
# One-time shock, future shocks are zero
irf <- function(y.last, newinfo) {
  return(b*y.last)
}

b <- 0.5
e <- rep(0.0, 12)
# Set init to the shock
irf.0.5 <- ts(Reduce(irf, e, init=1.0,
              accumulate=TRUE))
irf.0.5
plot(irf.0.5)

b <- 1.0
e <- rep(0.0, 12)
# Set init to the shock
irf.unit <- ts(Reduce(irf, e, init=1.0,
                     accumulate=TRUE))
irf.unit
plot(irf.unit)

# IRF of y(t) = 1.5+0.4y(t-1)+0.2y(t-2)
b1 <- 0.4
b2 <- 0.2
irf.ar2 <- function(y.last, newinfo) {
  return(c(b1*y.last[1] + b2*y.last[2],
           y.last[1]))
}
irf.ar2(c(1.0, 1.0), 0.0)
irf.ar2(c(0.6, 1.0), 0.0)
irf.ar2(c(0.44, 0.6), 0.0)

e <- rep(0.0, 12)
irf2 <- Reduce(irf.ar2, e, init=c(1.0, 0.0),
               accumulate=TRUE)
irf2
pull.first <- function(z) { z[1] }
pull.first(c(6.4, 2.1))
irf.values <- sapply(irf2, pull.first)
irf.values
plot(ts(irf.values))
# Would work for any ARMA model
b1 <- 0.8
b2 <- 0.2
e <- rep(0.0, 12)
irf.ar2 <- function(y.last, newinfo) {
  return(c(b1*y.last[1] + b2*y.last[2],
           y.last[1]))
}
irf2 <- Reduce(irf.ar2, e, init=c(1.0, 0.0),
               accumulate=TRUE)
irf.values <- sapply(irf2, pull.first)
plot(ts(irf.values), ylim=c(0,1))
# Permanent shock





