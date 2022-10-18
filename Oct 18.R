# Decomposing RF VAR forecasting error (residual)
library(tstools)
inf <- import.fred("inflation.csv")
u <- import.fred("unrate.csv")
rhs <- lags(inf %~% u, 1)
fit.inf <- tsreg(inf, rhs)
fit.u <- tsreg(u, rhs)
residuals(fit.inf)
fitted(fit.inf)
inf
last(u,2)
last(u)
last(inf)
# Predict Aug 2022 and Sep 2022
fit.inf
fit.u
# Inf 1-step Aug 2022
0.12 + 0.99*8.5 - 0.01*3.5
# U 1-step Aug 2022
0.15 + 0.01*8.5 + 0.97*3.5
# Inf Sep 2022
0.12 + 0.99*8.5 - 0.01*3.63
# What if u was 4.5 in July?
# 1 percentage point shock to u
0.12 + 0.99*8.5 - 0.01*4.5
0.15 + 0.01*8.5 + 0.97*4.5
0.12 + 0.99*8.49 - 0.01*4.6
# 8.5 and 8.4987
# 8.49 and 8.4791
# Difference is effect of shock to u
# Recursive system: Can estimate by OLS
fit.inf <- tsreg(inf, lags(inf,1) %~% lags(u,0:1))
fit.inf
# u -> 4.5
# inf -> 8.3
# Can then do the predictions
# Take shock (treatment) and calculate effect on time T
# variables
# Predict T+1 and later values using your RF VAR model













