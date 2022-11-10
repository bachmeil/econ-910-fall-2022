library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")

# Estimate using all data
rhs <- lags(u %~% inf, 1)
fit.u <- tsreg(u, rhs)
fit.inf <- tsreg(inf, rhs)
res.u1 <- window(fit.u$resids, end=c(1994,12))
res.inf1 <- window(fit.inf$resids, end=c(1994,12))
cov1 <- cov(res.u1 %~% res.inf1)
# Covariance matrix of the residuals in regime 1
res.u2 <- window(fit.u$resids, start=c(1995,1))
res.inf2 <- window(fit.inf$resids, start=c(1995,1))
cov2 <- cov(res.u2 %~% res.inf2)
cov1
cov2
# Very different reduced form residual behavior in the
# two regimes
# Has to be due to changes in the structural shock variances
nleq.obj <- function(par) {
  b <- par[1]
  c <- par[2]
  su1 <- par[3]
  sinf1 <- par[4]
  su2 <- par[5]
  sinf2 <- par[6]
  dev1 <- su1 + (b^2)*sinf1 - 0.05194833
  dev2 <- (c^2)*su1 + sinf1 - 0.21286096
  dev3 <- c*su1 + b*sinf1 + 0.01862744
  dev4 <- su2 + (b^2)*sinf2 - 0.37922750
  dev5 <- (c^2)*su2 + sinf2 - 0.17894608
  dev6 <- c*su2 + b*sinf2 + 0.05231784
  return(c(dev1, dev2, dev3, dev4, dev5, dev6))
}
nleqslv(c(0, 0, 0.1, 0.1, 0.1, 0.2), nleq.obj)
# Unemployment shock variance seven times as high
# Inflation shock variance lower, very stable later time period


# Bootstrapping (just identified VAR)
# Get draws of the reduced form VAR coefficients
# Solve for structural coefficients using your identification
# restrictions
# Generate new data and estimate reduced form VAR again
# Wild bootstrap
# Deals with conditional heteroskedasticity
# Easy to implement
u.fitted <- fit.u$fitted
inf.fitted <- fit.inf$fitted
data.fitted <- u.fitted %~% inf.fitted
u.resids <- tsextend(res.u1, res.u2)
inf.resids <- tsextend(res.inf1, res.inf2)
inf.resids
data.resids <- u.resids %~% inf.resids
z <- sample(c(-1, 1), size=nrow(data.resids),
            replace=TRUE)
z
data.sim <- data.fitted + z*data.resids
data.sim
colnames(data.sim) <- c("u", "inf")

rhs <- lags(data.sim, 1)
fit.u <- tsreg(u, rhs)
fit.inf <- tsreg(inf, rhs)
res.u1 <- window(fit.u$resids, end=c(1994,12))
res.inf1 <- window(fit.inf$resids, end=c(1994,12))
cov1 <- cov(res.u1 %~% res.inf1)
# Covariance matrix of the residuals in regime 1
res.u2 <- window(fit.u$resids, start=c(1995,1))
res.inf2 <- window(fit.inf$resids, start=c(1995,1))
cov2 <- cov(res.u2 %~% res.inf2)
cov1
cov2
nleq.obj <- function(par) {
  b <- par[1]
  c <- par[2]
  su1 <- par[3]
  sinf1 <- par[4]
  su2 <- par[5]
  sinf2 <- par[6]
  dev1 <- su1 + (b^2)*sinf1 - 0.18
  dev2 <- (c^2)*su1 + sinf1 - 0.78
  dev3 <- c*su1 + b*sinf1 + 0.06
  dev4 <- su2 + (b^2)*sinf2 - 1.53
  dev5 <- (c^2)*su2 + sinf2 - 0.78
  dev6 <- c*su2 + b*sinf2 + 0.25
  return(c(dev1, dev2, dev3, dev4, dev5, dev6))
}
nleqslv(c(0, 0, 0.1, 0.1, 0.1, 0.2), nleq.obj)










