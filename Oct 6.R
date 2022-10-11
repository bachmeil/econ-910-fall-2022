trim.dates <- function(x,y) {
  return(window(x,
                start=max(tsp(x)[1], tsp(y)[1]),
                end=min(tsp(x)[2], tsp(y)[2])))
}

# IID bootstrap
# Assume residuals are IID
library(tstools)
u.raw <- import.fred("unrate.csv")
inf.raw <- import.fred("inflation.csv")
u <- trim.dates(u.raw, inf.raw)
inf <- trim.dates(inf.raw, u.raw)

fit <- tsreg(inf, u)
fit

# How do we generate a new sample?
res <- residuals(fit)
res
# Use sample to do resampling
res.sim <- sample(res, replace=TRUE)
res.sim
unique(res.sim)
length(res.sim)

# Use the new residuals to generate a new sample
inf.sim <- 2.88 + 0.109 * u + res.sim
plot(ts.combine(inf.sim, inf), plot.type="single")
ts.combine(inf.sim, inf)
fit.sim <- tsreg(inf.sim, u)
fit.sim

# New sample from the population gave beta=0.25
# Do this many times
set.seed(100)
simcoef <- replicate(1000, {
  res.sim <- sample(res, replace=TRUE)
  inf.sim <- 2.88 + 0.109 * u + res.sim
  fit.sim <- tsreg(inf.sim, u)
  coefficients(fit.sim)[2]
})
simcoef
plot(density(simcoef))
sd(simcoef)
0.109/sd(simcoef)
# Bootstrapped t-statistic
sd(1/(simcoef^0.3))

# Pairs bootstrap
set.seed(100)
simcoef <- replicate(1000, {
  # Sample observation numbers
  obs <- sample(1:length(u), replace=TRUE)
  inf.sim <- ts(inf[obs])
  u.sim <- ts(u[obs])
  fit.sim <- tsreg(inf.sim, u.sim)
  coefficients(fit.sim)[2]
})
sd(simcoef)

# Serial correlation
# Block bootstrap
# How to construct the simulated residuals
12 - 0:3
# T=702, block length 5 => 140 nonoverlapping blocks
# Block numbers
1:140
# If you chose block 72
(72*5) - 0:4
block.numbers <- sample(1:140, replace=TRUE)
block.numbers
(102*5) - 0:4
(34*5) - 0:4
obs <- integer(0)
for (block in block.numbers) {
  obs <- c(obs, (block*5) - 4:0)
}
obs
inf[obs]
u[obs]
