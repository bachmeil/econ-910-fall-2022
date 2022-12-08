library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")

# Need to draw from MVN distribution
library(mvtnorm)

rhs <- inf %~% u
fit <- tsreg(inf, lags(rhs, 1))
b <- coefficients(fit)
s2 <- summary(fit)$sigma^2
nu <- length(fit$fitted)
xx <- cbind(1, lags(inf %~% u, 1))
head(xx)
xx.inv <- solve(crossprod(xx))
# These are the inputs for the GS algorithm
# Now take draws of alpha and beta

draws <- replicate(1000, {
  tau.draw <- rgamma(1, nu/2, nu*s2/2)
  rmvnorm(1, b, (1/tau.draw)*xx.inv)
}, simplify="matrix")
dim(draws)
# Posterior density of the intercept
plot(density(draws[1,]))
plot(density(draws[2,]))
plot(density(draws[3,]))
mean(draws[3,] < 0)
rowMeans(draws)
fit$coefficients
sd(draws[1,])
sd(draws[2,])
sd(draws[3,])
summary(fit)

# Linear RF VAR model
library(BVAR)
data <- inf %~% u
vardraws <- bvar(data, lags=1, n_draw=10000, n_burn=1000,
                 verbose=FALSE)
class(vardraws$beta)
dim(vardraws$beta)
b.eq1 <- vardraws$beta[,,1]
dim(b.eq1)
plot(density(b.eq1[,3]))

