trim.dates <- function(x,y) {
  return(window(x,
                start=max(tsp(x)[1], tsp(y)[1]),
                end=min(tsp(x)[2], tsp(y)[2])))
}
library(tstools)
u.raw <- import.fred("unrate.csv")
inf.raw <- import.fred("inflation.csv")
# Make the u and inf series have matching dates
# This won't work if they have different lengths
u <- trim.dates(u.raw, inf.raw)
inf <- trim.dates(inf.raw, u.raw)
fit <- tsreg(inf, u)
# This is the iid bootstrap from before
# Now change how we generate the simulated residuals
# Use Rademacher as the pick distribution
sample(c(-1,1), size=10, replace=TRUE)

set.seed(100)
simcoef <- replicate(1000, {
  # Draws from pick distribution
  z <- sample(c(-1,1), size=length(u), replace=TRUE)
  # Generate simulated residuals
  res.sim <- z*fit$resids
  # Generate the simulated inflation data
  inf.sim <- 2.88 + 0.109 * u + res.sim
  fit.sim <- tsreg(inf.sim, u)
  coefficients(fit.sim)[2]
})
mean(simcoef)
sd(simcoef)

set.seed(100)
tstats <- replicate(1000, {
  # Draws from pick distribution
  z <- sample(c(-1,1), size=length(u), replace=TRUE)
  # Generate simulated residuals
  res.sim <- z*fit$resids
  # Generate the simulated inflation data
  inf.sim <- 2.88 + 0.109 * u + res.sim
  fit.sim <- tsreg(inf.sim, u)
  # Pull the t-stat
  summary(fit.sim)$coefficients[2,3]
})
sort(tstats)
# This doesn't make sense
# If beta=0, many t-stats should be below zero

# Recenter the t-stat
set.seed(100)
tstats <- replicate(1000, {
  # Draws from pick distribution
  z <- sample(c(-1,1), size=length(u), replace=TRUE)
  # Generate simulated residuals
  res.sim <- z*fit$resids
  # Generate the simulated inflation data
  inf.sim <- 2.88 + 0.109 * u + res.sim
  fit.sim <- tsreg(inf.sim, u)
  # Pull the t-stat
  # Recentered t-stat
  summary(fit.sim)$coefficients[2,3] - 1.901907
})
sort(tstats)
# This is the distribution of the t-stat under the null
# hypothesis. As expected, +-1.96 are the correct critical
# values.
