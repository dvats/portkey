\name{portkey-package}
\alias{portkey-package}
\docType{package}
\title{
Bernoulli Factory MCMC: Portkey Two-Coin Algorithm for Markov Chain Monte Carlo
}
\description{
The package is built to be embedded within an MCMC algorithm and runs a Portkey two-coin Bernoulli factory to obtain the next step of the Markov chain.
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{portkey}
\packageIndices{portkey}
Use the package within an MCMC algorithm via a Portkey two-coin Bernoulli factory to obtain the next step of the Markov chain.
}
\author{
\packageAuthor{portkey}

Maintainer: \packageMaintainer{portkey}
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }

\examples{
# The following example implements the portkey-two coin algorithm for a
# Gamma mixture of Weibulls as presented in Vats et. al. (2020)
set.seed(10)  # seed is chosen so that example is fast
Cf <- function(value, k)
{
return(k/(exp(1)*value))
}
pf <- function(value, k, lam1, a1, b1)
{
lam1 <- rgamma(1, shape = a1, rate = b1)
dweibull(value, shape = k, scale = lam1)/Cf(value = value, k = k)
}
mcmc <- function(N = 1e3, beta = .95, k = 5, a1 = 2, b1 = 5)
{
out <- numeric(length = N)
loops <- numeric(length = N)
out[1] <- .1
acc <- 0
for(i in 2:N)
{
	prop <- rnorm(1, out[i-1], sd = sqrt(.001))
	if(prop < 0) 
	{
		out[i] <- out[i-1]
		next
	}
	interim <- portkey(prop = prop, curr = out[i-1], pf = pf, Cprop = Cf(prop, k), 
		Ccurr = Cf(out[i-1], k), beta = beta, k = k, a1 = a1, b1 = b1)
	out[i] <-  interim[[1]]
	if(out[i] != out[i-1]) acc <- acc+1
	loops[i] <- interim[[2]]
}
return(list("mcmc" = out, "loops" = loops, "accept" = acc/N))
}

a1 <- 10
b1 <- 100
k <- 10

bark <- mcmc(N = 5e3, beta = 1,  k = 10, a1 = 10, b1 = 100)
port <- mcmc(N = 5e3, beta = .99, k = 10, a1 = 10, b1 = 100)
plot(1:5e3, bark$loops, pch = 3)
points(1:5e3, port$loops, pch = 1, col = "red")
par(mfrow = c(1,2))
acf(bark$mcmc)
acf(port$mcmc)
par(mfrow = c(1,1))
}
