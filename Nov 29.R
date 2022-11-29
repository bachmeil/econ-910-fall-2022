# Now forecast with the shock of +13%
fcst.shock <- Reduce(fcst.next, shocks.list,
                        init=c(y.init+0.0232*0.13, oil.init+0.13),
                        accumulate=TRUE)

# Initial oil value is 13% higher
# What happens to GDP?
# Regress GDP residual on the oil shock
# That coefficient is the contemporaneous response of GDP
tsreg(fit.nonlinear$resids, s.oil)

fcst.shock

# Pull out the GDP forecast at each horizon
baseline <- collect(fcst.baseline,
                    transform=function(z) { z[1] },
                    output="numeric")
baseline
shock.pos <- collect(fcst.shock,
                     transform=function(z) { z[1] },
                     output="numeric")
shock.pos
# Impulse response for GDP following a one std deviation
# positive oil price shock
shock.pos - baseline

# That's the IRF for that choice of initial state
# and that set of future shocks
# Now we need to take draws of both
# Compute IRF for that draw
# Average over many draws
set.seed(327)
irf.draws <- replicate(100, {
  # Get draw of state of economy and future shocks
  date.draw <- sample(dates(c(1947, 2), c(2022, 3), 4), size=1)
  y.init <- tsobs(dgdp, date.draw)
  oil.init <- tsobs(doil, date.draw)
  oil.shocks <- sample(s.oil, size=4, replace=TRUE)
  gdp.shocks <- sample(s.gdp, size=4, replace=TRUE)
  shocks.list <- zip(oil.shocks, gdp.shocks)
  fcst.baseline <- Reduce(fcst.next, shocks.list,
                          init=c(y.init, oil.init),
                          accumulate=TRUE)
  fcst.shock <- Reduce(fcst.next, shocks.list,
                       init=c(y.init+0.0232*0.13, 
                              oil.init+0.13),
                       accumulate=TRUE)
  collect(fcst.shock,
          transform=function(z) { z[1] },
          output="numeric") - 
  collect(fcst.baseline,
            transform=function(z) { z[1] },
            output="numeric")
}, simplify="array")

oil.shocks
gdp.shocks
zip(oil.shocks, gdp.shocks)

dim(irf.draws)
# Second IRF (one quarter after the shock)
mean(irf.draws[2,])
mean(irf.draws[3,])
max(irf.draws[2,])
min(irf.draws[2,])
# The IRF is the average over all possible outcomes
