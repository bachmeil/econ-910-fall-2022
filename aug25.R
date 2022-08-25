# Calculate IRFs by hand
y5 <- 0.5*y4 + 0.0
y4 <- 0.5*y3 + 0.0
y3 <- 0.5*y2 + 0.0
y2 <- 0.5*y1 + 0.0
y1 <- 0.5*y0 + 1.0
y0 <- 0.0
y1
y2
y3
y4
y5

# Reduce this using a function
ar1.calc <- function(y.last, e.next) {
  return(0.5*y.last + e.next)
}
ar1.calc(0.0, 1.0)
ar1.calc(1.0, 0.0)
ar1.calc(0.5, 0.0)
ar1.calc(0.25, 0.0)
ar1.calc(0.125, 0.0)

# Minimize input to prevent mistakes
# Do it in one line
Reduce(ar1.calc, c(1.0, rep(0.0, 24)), init=0.0)

# Use accumulate to save all the computed values
Reduce(ar1.calc, c(1.0, rep(0.0, 24)), init=0.0, accumulate=TRUE)

# Terrible RNG algorithm
4357^2
8344^2
2233^2
8628^2


1452^2
0830^2
8890^2
# Repeats at 0 forever

set.seed(100)
y <- runif(1000000)
y
plot(density(y))

x <- rnorm(1000000, mean=0.0, sd=1.0)
plot(density(x))
library(tseries)

set.seed(100)
x <- rt(10000, 4)
y <- rf(10000, 12, 24)
plot(density(y))
plot(density(x))
z <- (1/x) + (1/sin(y))
sd(z)
mean(z)
plot(density(z))
