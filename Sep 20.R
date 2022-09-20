log(0.5)/log(0.8)
log(0.5)/log(0.9)
log(0.5)/log(0.92)
ARMAtoMA(ar=c(1.2, -0.2, -0.2), lag.max=10)
# Half-life is 5 quarters (first horizon less
# than 0.5
ARMAtoMA(ar=0.8, lag.max=10)
ARMAtoMA(ar=c(1.2, -0.2, -0.2), lag.max=30)
ARMAtoMA(ar=c(1.2, -0.2, -0.2), lag.max=300)
ARMAtoMA(ar=0.95, lag.max=10)
ARMAtoMA(ar=0.9, lag.max=10)
ARMAtoMA(ar=0.914, lag.max=20)
