a <- 5.0
b1 <- 0.5
b2 <- 0.1
# First subsample
ar1.1 <- function(y.last, newinfo) {
  return(a + b1*y.last + newinfo)
}
# Second subsample
ar1.2 <- function(y.last, newinfo) {
  return(a + b2*y.last + newinfo)
}

library(tstools)
set.seed(100)
e <- rnorm(500)
x1 <- Reduce(ar1.1, e[1:350], init=10.0, accumulate=TRUE)[-1]
x1
x2 <- Reduce(ar1.2, e[351:500], init=last(x1), accumulate=TRUE)[-1]
x2
x <- ts(c(x1,x2), start=c(1980,1), frequency=12)
plot(x)
fit <- tsreg(x, lags(x,1))
fit
plot(fit$resids)
# Problem: Bad model specification
# Can't condition on the necessary information
dum <- time.dummy(x, c(2009,3), "end")
dum
rhs <- ts.combine(lags(x,1), dum*lags(x,1))
tsreg(x, rhs)
