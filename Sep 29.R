library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")
plot(u)
plot(inf)

dev1 <- function(a, b) {
  # lag (not lags) with 1 is pi(t+1)
  ds <- ts.combine(u, lag(inf,1), lags(inf, 0:1))
  return( (ds[,1] - a - b*ds[,2])*ds[,3] )
}
dev1(0, 0)
mean(dev1(0, 0))
mean(dev1(6, 0))

dev2 <- function(a, b) {
  ds <- ts.combine(u, lag(inf,-1), lags(inf, 0:1))
  return( (ds[,1] - a - b*ds[,2])*ds[,4] )
}
mean(dev2(6, 0))

# Combine into one objective function
objfun <- function(parameters) {
  a <- parameters[1]
  b <- parameters[2]
  return(
    mean(dev1(a, b))^2 + mean(dev2(a, b))^2
  )
}
optim(c(0,0), objfun)
optim(c(0,0), objfun, control=list(maxit=10))








