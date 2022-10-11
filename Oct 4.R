library(tstools)
u <- import.fred("unrate.csv")
fit.nochange <- tsreg(u, lags(u, 1:2))
fit.nochange

d <- time.dummy(u, c(1980,1), "end")
d
rhs <- ts.intersect(lags(u,1:2), d*lags(u, 1:2))
rhs[,1]
rhs[,3]
fit.change <- tsreg(u, rhs)
fit.change
library(lmtest)
lrtest(fit.change, fit.nochange)
# Don't reject
rhs1 <- rhs[,1:2]
# Break
fit.large <- tsreg(u, rhs)
# No break
fit.small <- tsreg(u, rhs1)
waldtest(fit.large, fit.small)
waldtest(fit.large, fit.small, test="Chisq")
# 15% of the sample in each regime
floor(0.15*length(u))
floor(0.85*length(u))
time(u)[134]
time(u)[761]
# We'll look for a break between Feb 1959 and Apr 2011
possible.breaks <- dates(c(1959,2), c(2011,4), 12)
possible.breaks

f.calc <- function(break.date) {
  d <- time.dummy(u, break.date, "end")
  rhs <- ts.intersect(lags(u,1:2), d*lags(u,1:2))
  rhs1 <- rhs[,1:2]
  fit.large <- tsreg(u, rhs)
  fit.small <- tsreg(u, rhs1)
  test <- waldtest(fit.large, fit.small)
  return(test[2,3])
}
f.calc(possible.breaks[1])
f.calc(possible.breaks[104])

ftest.stats <- sapply(possible.breaks, f.calc)
ftest.stats
plot(ftest.stats, type="l")
which.max(ftest.stats)
possible.breaks[623]
# Dec 2010 is the estimated break date
dataset <- lags(u, 0:2)
colnames(dataset) <- c("u", "u1", "u2")
model <- u ~ u1 + u2
library(strucchange)
fs <- Fstats(model, from=c(1959,2), to=c(2011,4),
             data=dataset)
plot(fs, aveF=TRUE)
sctest(fs, type="supF")
sctest(fs, type="aveF")
sctest(fs, type="expF")

# This is usually all you do!
sctest(model, type="expF", from=c(1959,2), to=c(2011,4),
       data=dataset)
floor(0.15*length(u))
623 - 134
# 489 would be the last
second.break <- sapply(possible.breaks[1:489], f.calc)
second.break
which.max(second.break)
possible.breaks[294]
# If there are two break dates, July 1983 is one of them
# Dec 2010 is the other
# Two breaks at 327, 522, T=1000
# Want third break date
enough <- function(obs) {
  return(
    (obs >= 150) &
    (obs <= 850) &
    (abs(obs-327) >= 150) &
    (abs(obs-522) >= 150)
  )
}
which(enough(1:1000))
