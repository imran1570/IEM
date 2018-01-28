library(sROC)
library(BMS)
set.seed(123)
n <- 10000
x <- rnorm(n,0,1)
x.CDF <- kCDF(x, bw = 1)
x.CDF
plot(x.CDF, main="Kernel estimate of distribution function")
curve(pnorm(x), add=TRUE, lty=2, col="blue")


qnorm(0.5)
pnorm(0)

x.dns = density(x, bw = 1)

BMS::quantile.density(x.dns,0.5) #use this to get the underlying value

quantile(ecdf(x), 0.5) 

hist(cumsum(x.dns$y))

cumsum(x.CDF$Fhat)

plot(cumsum(x.dns$y))

plot(ecdf(x))(quantile(x,0.1))
