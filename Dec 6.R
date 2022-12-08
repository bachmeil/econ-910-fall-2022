# Take a draw from the posterior of beta
b <- rnorm(1, mean=1.0, sd=sqrt(0.5))

# Now calculate inflation conditional on ffr=10%.
inf <- 8 - b*(10-4)
b
inf

set.seed(100)
inf.values <- replicate(10000, {
  b <- rnorm(1, mean=1.0, sd=sqrt(0.5))
  8 - b*(10-4)
})
# 10,000 draws from posterior distribution of inflation
mean(inf.values)
sd(inf.values)
plot(density(inf.values))
# Probability of deflation?
mean(inf.values < 0)
# 32% chance of deflation
mean(inf.values > 5)

# What if ffr = 12%?
set.seed(100)
inf.values <- replicate(10000, {
  b <- rnorm(1, mean=1.0, sd=sqrt(0.5))
  8 - b*(12-4)
})
mean(inf.values < 0)
mean(inf.values > 5)
# Rule out beta < 0.2
# Prior distribution for beta
# Modify the simulation to account for this
set.seed(100)
# Simulate from the posterior distribution of inflation
# when we have an informative prior distribution
inf.values <- replicate(10000, {
  b <- -5
  while (b < 0.2) {
    b <- rnorm(1, mean=1.0, sd=sqrt(0.5))
  }
  8 - b*(10-4)
})
mean(inf.values < 0)
mean(inf.values > 5)
plot(density(inf.values))









