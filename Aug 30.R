inf.raw <- read.csv("inflation.csv", header=TRUE)
inf <- ts(inf.raw[,2], start=c(1948,1), frequency=12)
plot(inf)
inf

library(tstools)
e1 <- tsobs(inf, c(1948,1))
e2 <- as.numeric(tsobs(inf, c(1948,2))) - 0.4*e1
e2
e3 <- as.numeric(tsobs(inf, c(1948,3))) - 0.4*e2
e3
pnorm(e1) * pnorm(e2) * pnorm(e3)
pnorm(e1)

library(AER)
data(USMacroG)
dgdp <- 400*pctChange(USMacroG[,"gdp"])
plot(dgdp)
mean(dgdp)
arma11 <- arima(dgdp, order=c(1, 0, 1))
arma11
predict(arma11)
arma23 <- arima(dgdp, order=c(2, 0, 3))
arma23
predict(arma23)
AIC(arma11)
AIC(arma23)
BIC(arma11)
BIC(arma23)
