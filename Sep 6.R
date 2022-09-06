rgdp.raw <- read.csv("https://raw.githubusercontent.com/bachmeil/econ-910-fall-2022/main/rgdp.csv",
                     header=TRUE)
rgdp <- ts(rgdp.raw[,2], start=c(1947,1), frequency=4)
plot(rgdp)
library(tstools)
fit <- tsreg(rgdp, lags(rgdp,8:11))
trend <- fit$fitted
trend
plot(trend, main="Real GDP Trend")
rgdp.both <- ts.combine(rgdp, trend)
plot(rgdp.both, plot.type="single",
     main="Real GDP and Its Trend",
     lty=c(1,2))
cyclical <- fit$resids
plot(cyclical)

ffr.raw <- read.csv("https://raw.githubusercontent.com/bachmeil/econ-910-fall-2022/main/fedfunds.csv",
                    header=TRUE)
ffr <- ts(ffr.raw[,2], start=c(1954,7),
          frequency=12)
plot(ffr)
fit <- tsreg(ffr, lags(ffr,12:15))
trend <- fit$fitted
plot(trend)
transitory <- fit$resids
plot(transitory, main="Deviation from FFR Trend")


# Simulate from an AR(1) process
b <- 0.5
ar1 <- function(y.last, newinfo) {
  return(b*y.last + newinfo)
}
ar1(2.2, -0.3)
# 0.5*2.2 - 0.3 = 0.8

set.seed(100)
e <- rnorm(500)
plot(e)
Reduce(ar1, e, init=0.0, accumulate=TRUE)
# 0.5*0.0 - 0.5021924
e[1]
ysim <- ts(Reduce(ar1, e, init=0.0, accumulate=TRUE))
plot(ysim)

# Look at distribution of 10th observation
set.seed(100)
simoutput.10 <- replicate(200, {
  e <- rnorm(500)
  Reduce(ar1, e, init=0.0, accumulate=TRUE)[10]
}, simplify="array")
simoutput.10
plot(density(simoutput.10))
sd(simoutput.10)

set.seed(100)
simoutput.100 <- replicate(200, {
  e <- rnorm(500)
  Reduce(ar1, e, init=0.0, accumulate=TRUE)[100]
}, simplify="array")
simoutput.100
plot(density(simoutput.100))
sd(simoutput.100)


# Redo but with unit root process
b <- 1
set.seed(100)
simoutput.10 <- replicate(200, {
  e <- rnorm(500)
  Reduce(ar1, e, init=0.0, accumulate=TRUE)[10]
}, simplify="array")
simoutput.10
plot(density(simoutput.10))
sd(simoutput.10)

set.seed(100)
simoutput.100 <- replicate(200, {
  e <- rnorm(500)
  Reduce(ar1, e, init=0.0, accumulate=TRUE)[100]
}, simplify="array")
simoutput.100
plot(density(simoutput.100))
sd(simoutput.100)


set.seed(100)
simoutput.500 <- replicate(200, {
  e <- rnorm(500)
  Reduce(ar1, e, init=0.0, accumulate=TRUE)[500]
}, simplify="array")
simoutput.500
plot(density(simoutput.500))
sd(simoutput.500)

