du.next <- function(current, newresid) {
  du <- current[1]
  u <- current[2]
  newdu <- if (last(u) > 6) {
    -0.04 - 0.04*last(du) + newresid
  } else {
    0.03 + 0.63*last(du) + newresid
  }
  return(c(newdu, u+newdu))
}
du.next(c(du=0.00, u=4.0), 0.5)
du.next(c(du=0.53, u=4.53), 0.0)

# Use average unemployment rate
mean(u)
# And 'typical' shock
sd(fit.nonlinear$resids)
res <- residuals(fit.nonlinear)

# Future shocks
simres <- sample(res, size=12, replace=TRUE)

# Get forecast with no shock
fcst.baseline <- Reduce(du.next, simres,
                        init=c(du=0.0, u=5.74),
                        accumulate=TRUE)
fcst.baseline
u.baseline <- collect(fcst.baseline,
                      name="u", output="numeric")
u.baseline
# That's the evolution of u for this set of shocks
# Now redo that, but with different initial values
# to account for the shock
fcst.posShock <- Reduce(du.next, simres,
                        init=c(du=0.41, u=6.15),
                        accumulate=TRUE)
fcst.posShock
u.posShock <- collect(fcst.posShock,
                      name="u", output="numeric")
u.posShock
# This is the unemployment rate with the same set of future
# shocks, but initial shock is +0.41.
# IRF is the difference in forecast with and without
# the shock
irf.pos <- ts(u.posShock - u.baseline)
irf.pos

# Now compute for negative shocks
fcst.negShock <- Reduce(du.next, simres,
                        init=c(du=-0.41, u=5.33),
                        accumulate=TRUE)
fcst.negShock
u.negShock <- collect(fcst.negShock,
                      name="u", output="numeric")
u.negShock
irf.neg <- ts(u.negShock - u.baseline)
irf.neg
# Are the two shocks symmetric?
plot(irf.pos %~% irf.neg, plot.type="single")
# Problem: We have to average over future shocks
# Have to repeat this, but with a different draw of the
# shocks each time

set.seed(200)
irfs.pos <- replicate(100, {
  simres <- sample(res, size=12, replace=TRUE)
  fcst.baseline <- Reduce(du.next, simres,
                          init=c(du=0.0, u=5.74),
                          accumulate=TRUE)
  u.baseline <- collect(fcst.baseline,
                        name="u", output="numeric")
  fcst.posShock <- Reduce(du.next, simres,
                          init=c(du=0.41, u=6.15),
                          accumulate=TRUE)
  u.posShock <- collect(fcst.posShock,
                        name="u", output="numeric")
  # Save the IRF for this choice of simres
  ts(u.posShock - u.baseline)
})
dim(irfs.pos)
# Need the average of each row
pos.avg <- ts(apply(irfs.pos, MARGIN=1, mean))
pos.avg
# This is the IRF following a positive 1 sd shock
# It is the average over all possible sets of future shocks
mean(irfs.pos[2,])
mean(irfs.pos[3,])

set.seed(200)
irfs.neg <- replicate(100, {
  simres <- sample(res, size=12, replace=TRUE)
  fcst.baseline <- Reduce(du.next, simres,
                          init=c(du=0.0, u=5.74),
                          accumulate=TRUE)
  u.baseline <- collect(fcst.baseline,
                        name="u", output="numeric")
  fcst.negShock <- Reduce(du.next, simres,
                          init=c(du=-0.41, u=5.33),
                          accumulate=TRUE)
  u.negShock <- collect(fcst.negShock,
                        name="u", output="numeric")
  # Save the IRF for this choice of simres
  ts(u.negShock - u.baseline)
})
neg.avg <- ts(apply(irfs.neg, MARGIN=1, mean))
neg.avg

# Is there symmetry?
plot(pos.avg %~% neg.avg, plot.type="single")

# What about u=3% for a starting value?
set.seed(200)
irfs.pos <- replicate(100, {
  simres <- sample(res, size=12, replace=TRUE)
  fcst.baseline <- Reduce(du.next, simres,
                          init=c(du=0.0, u=3.0),
                          accumulate=TRUE)
  u.baseline <- collect(fcst.baseline,
                        name="u", output="numeric")
  fcst.posShock <- Reduce(du.next, simres,
                          init=c(du=0.41, u=3.41),
                          accumulate=TRUE)
  u.posShock <- collect(fcst.posShock,
                        name="u", output="numeric")
  # Save the IRF for this choice of simres
  ts(u.posShock - u.baseline)
})
# Need the average of each row
pos.avg <- ts(apply(irfs.pos, MARGIN=1, mean))

set.seed(200)
irfs.neg <- replicate(100, {
  simres <- sample(res, size=12, replace=TRUE)
  fcst.baseline <- Reduce(du.next, simres,
                          init=c(du=0.0, u=3.0),
                          accumulate=TRUE)
  u.baseline <- collect(fcst.baseline,
                        name="u", output="numeric")
  fcst.negShock <- Reduce(du.next, simres,
                          init=c(du=-0.41, u=2.59),
                          accumulate=TRUE)
  u.negShock <- collect(fcst.negShock,
                        name="u", output="numeric")
  # Save the IRF for this choice of simres
  ts(u.negShock - u.baseline)
})
neg.avg <- ts(apply(irfs.neg, MARGIN=1, mean))

plot(pos.avg %~% neg.avg, plot.type="single")
# Symmetry because this is, more or less, a linear model
# The initial u is so low that we're always in the
# low u regime

set.seed(200)
irfs.pos <- replicate(100, {
  simres <- sample(res, size=12, replace=TRUE)
  fcst.baseline <- Reduce(du.next, simres,
                          init=c(du=0.0, u=9.0),
                          accumulate=TRUE)
  u.baseline <- collect(fcst.baseline,
                        name="u", output="numeric")
  fcst.posShock <- Reduce(du.next, simres,
                          init=c(du=0.41, u=9.41),
                          accumulate=TRUE)
  u.posShock <- collect(fcst.posShock,
                        name="u", output="numeric")
  # Save the IRF for this choice of simres
  ts(u.posShock - u.baseline)
})
# Need the average of each row
pos.avg <- ts(apply(irfs.pos, MARGIN=1, mean))

set.seed(200)
irfs.neg <- replicate(100, {
  simres <- sample(res, size=12, replace=TRUE)
  fcst.baseline <- Reduce(du.next, simres,
                          init=c(du=0.0, u=9.0),
                          accumulate=TRUE)
  u.baseline <- collect(fcst.baseline,
                        name="u", output="numeric")
  fcst.negShock <- Reduce(du.next, simres,
                          init=c(du=-0.41, u=8.59),
                          accumulate=TRUE)
  u.negShock <- collect(fcst.negShock,
                        name="u", output="numeric")
  # Save the IRF for this choice of simres
  ts(u.negShock - u.baseline)
})
neg.avg <- ts(apply(irfs.neg, MARGIN=1, mean))

plot(pos.avg %~% neg.avg, plot.type="single")

