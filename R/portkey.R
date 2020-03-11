#' Portkey two-coin algorithm
#'
#' @param prop The proposed value in the MCMC step
#' @param curr The current value in the MCMC step
#' @param beta The portkey parameter with default being .99. If beta = 1, this is the regular two-coin algorithm
#' @param pf A function that produces a Bern(p) realization, with argument \code{value} for the state 
#'     of the Markov chain
#' @param Cprop Upper bound for the proposed value
#' @param Ccurr Upper bound for the current value
#' @param ... additional arguments that go into \code{pf} and \code{Cf}
#' @return  a variable \code{x} which is either \code{curr} or \code{prop} and integer \code{loops} that returns the number 
#'     of loops the Bernoulli factory took
#' @examples
#' # The following example implements the portkey-two coin algorithm for a
#' # Gamma mixture of Weibulls as presented in Vats et. al. (2020)
#' set.seed(10)  # seed is chosen so that example is fast
#'Cf <- function(value, k)
#'{
#'	return(k/(exp(1)*value))
#'}
#'pf <- function(value, k, lam1, a1, b1)
#'{
#'	lam1 <- rgamma(1, shape = a1, rate = b1)
#'	dweibull(value, shape = k, scale = lam1)/Cf(value = value, k = k)
#'}
#'mcmc <- function(N = 1e3, beta = .95, k = 5, a1 = 2, b1 = 5)
#'{
#'	out <- numeric(length = N)
#'	loops <- numeric(length = N)
#'	out[1] <- .1
#'	acc <- 0
#'	for(i in 2:N)
#'	{
#'		prop <- rnorm(1, out[i-1], sd = sqrt(.001))
#'		if(prop < 0) 
#'		{
#'			out[i] <- out[i-1]
#'			next
#'		}
#'		interim <- portkey(prop = prop, curr = out[i-1], pf = pf, Cprop = Cf(prop, k), Ccurr = Cf(out[i-1], k), beta = beta, k = k, a1 = a1, b1 = b1)
#'		out[i] <-  interim[[1]]
#'		if(out[i] != out[i-1]) acc <- acc+1
#'		loops[i] <- interim[[2]]
#'	}
#'	return(list("mcmc" = out, "loops" = loops, "accept" = acc/N))
#'}
#'
#'a1 <- 10
#'b1 <- 100
#'k <- 10
#'
#'bark <- mcmc(N = 5e3, beta = 1,  k = 10, a1 = 10, b1 = 100)
#'port <- mcmc(N = 5e3, beta = .99, k = 10, a1 = 10, b1 = 100)
#'plot(1:5e3, bark$loops, pch = 3)
#'points(1:5e3, port$loops, pch = 1, col = "red")
#'par(mfrow = c(1,2))
#'acf(bark$mcmc)
#'acf(port$mcmc)
#'par(mfrow = c(1,1))
#' @export
#' @author Dootika Vats, \email{dootika@iitk.ac.in}
portkey <- function(prop, curr, beta = .99, pf, Cprop, Ccurr, ...)
{
	x <- NA
	loops <- 0
	# Cx <- Cf(value = curr,...)
	# Cy <- Cf(value = prop,... )
	C <- Cprop/(Ccurr + Cprop)
	S <- 1

	while(is.na(x))
	{
		loops <- loops + 1
		if(beta != 1) S <- rbinom(1,1, beta)
		if(S == 0) 
		{
			x <- curr
			return(list("x" = x, "loops" = loops))
		} else{
			C1 <- rbinom(1, 1, C)
			if(C1 == 1)
			{
				p1 <- pf(value = prop, ...)
				C2 <- rbinom(1, 1, p1)
				if(C2 == 1)
				{
					x <- prop
					return(list("x" = x, "loops" = loops))
				} 
			} else{
				p2 <- pf(value = curr, ...)
				C2 <- rbinom(1, 1, p2)
				if(C2 == 1)
				{
					x <- curr
					return(list("x" = x, "loops" = loops))
				} 
			}
		}
	}
}


#' Two-coin algorithm
#'
#' @param prop The proposed value in the MCMC step
#' @param curr The current value in the MCMC step
#' @param pf A function that produces a Bern(p) realization, with argument \code{value} for the state 
#'     of the Markov chain
#' @param Cprop Upper bound for the proposed value
#' @param Ccurr Upper bound for the current value
#' @param ... additional arguments that go into \code{pf} and \code{Cf}
#' @return  a variable \code{x} which is either \code{curr} or \code{prop} and integer \code{loops} that returns the number 
#'     of loops the Bernoulli factory took
#' @export

twocoin <- function(prop, curr, pf, Cprop, Ccurr, ...)
{
	rtn <- portkey(prop = prop, curr = curr, beta = 1, pf = pf, Cprop = Cprop, Ccurr = Ccurr, ...)
	return(rtn)
}




