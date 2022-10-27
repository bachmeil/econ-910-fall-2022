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
res <- inf.res %~% u.res
res
cov(res)

# Solve for c and the two structural shock variances
# assuming b=0 (recursive)
# Numerical optimization
# Parameters are c, su, sinf
dev <- function(par) {
  b <- 0
  c <- par[1]
  su <- par[2]
  sinf <- par[3]
  dev1 <- su + (b^2)*sinf - 0.173
  dev2 <- (c^2)*su + sinf - 0.2
  dev3 <- c*su + b*sinf + 0.031
  return(dev1^2 + dev2^2 + dev3^2)
}
dev(c(0,0,0))
dev(c(0, 0.1, 0.1))
optim(c(0, 0.1, 0.1), dev)

# What if c=0?
dev <- function(par) {
  b <- par[1]
  c <- 0
  su <- par[2]
  sinf <- par[3]
  dev1 <- su + (b^2)*sinf - 0.173
  dev2 <- (c^2)*su + sinf - 0.2
  dev3 <- c*su + b*sinf + 0.031
  return(dev1^2 + dev2^2 + dev3^2)
}
dev(c(0,0,0))
dev(c(0, 0.1, 0.1))
optim(c(0, 0.1, 0.1), dev)

# What if b=c?
dev <- function(par) {
  b <- par[1]
  c <- par[1]
  su <- par[2]
  sinf <- par[3]
  dev1 <- su + (b^2)*sinf - 0.173
  dev2 <- (c^2)*su + sinf - 0.2
  dev3 <- c*su + b*sinf + 0.031
  return(dev1^2 + dev2^2 + dev3^2)
}
dev(c(0,0,0))
dev(c(0, 0.1, 0.1))
optim(c(0, 0.1, 0.1), dev)

# What if su=sinf?
dev <- function(par) {
  b <- par[1]
  c <- par[2]
  su <- par[3]
  sinf <- par[3]
  dev1 <- su + (b^2)*sinf - 0.173
  dev2 <- (c^2)*su + sinf - 0.2
  dev3 <- c*su + b*sinf + 0.031
  return(dev1^2 + dev2^2 + dev3^2)
}
dev(c(0,0,0))
dev(c(0, 0.1, 0.1))
optim(c(0, 0.1, 0.1), dev)


