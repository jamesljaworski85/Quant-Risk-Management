##############################################################################################
# The following code returns a value for the Black-Scholes model using the variable inputs.
# Analytical delta, gamma, rho, vega are calculated. These formulas are used for Full 
# revaluation
##############################################################################################
# Variable Inputs
##############################################################################################
# S = Asset price
# K = Strike price
# r = Risk-free rate
# q = continuously compounded dividend yield
# tau = Time to maturity
# sigma = Volatility of asset price
# EPS is used to calculate the price of the option (and the greeks) if the option has a very
# small time to expiration. Thus EPS = 1/365 (1 day expressed in years). For this homework
# it is not very significant.
##############################################################################################

small <- 1./365. 

bscall.value <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  d2 <- d1 - sigma*sqrt(tau)
  if (tau < small) { # Small time to expiry
    return(max(S-K,0))
  } else {
    return(S*exp(-q*tau)*pnorm(d1) - K*exp(-r*(tau))*pnorm(d2))
  }
}

bsput.value <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  d2 <- d1 - sigma*sqrt(tau)
  if (tau < small) { # Small time to expiry
    return(max(K-S,0))
  } else {
    return(K*exp(-r*(tau))*pnorm(-d2) - S*exp(-q*tau)*pnorm(-d1))
  }
}

bscall.delta <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  if (tau < small) { # by hand for small time to expiry
    if (K-S < 0) {
      return(1.0)
    } else {
      return(0.0)
    }
  } else {
    return(exp(-q*tau)*pnorm(d1))
  }
}

bsput.delta <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  if (tau < small) { # by hand for small time to expiry
    if (K-S > 0) {
      return(-1.0)
    } else {
      return(0.0)
    }
  } else {
    return(exp(-q*tau)*(pnorm(d1)-1))
  }
}

bscall.gamma <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  return((exp(-q*tau)*exp((-(d1^2)/2)))/(S*sigma*sqrt(tau)*sqrt(2*3.14159265358979)))
}

a <- (exp(-0.02*1)*exp((-0.281616758610292^2)/2))/(148.26*0.25*sqrt(1)*sqrt(2*3.14159265358979))

bscall.theta <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  d2 <- d1 - sigma*sqrt(tau)
  return(-S*dnorm(d1)*sigma/(2*sqrt(tau)) - r*K*exp(-r*tau)*pnorm(d2))
}

bsput.theta <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  d2 <- d1 - sigma*sqrt(tau)
  return(-S*dnorm(d1)*sigma/(2*sqrt(tau)) + r*K*exp(-r*tau)*pnorm(-d2))
}

bscall.vega <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  return(S*sqrt(tau)*dnorm(d1))
}

bscall.rho <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  d2 <- d1 - sigma*sqrt(tau)
  return(K*tau*exp(-r*tau)*pnorm(d2))
}

bsput.rho <- function(S, K, q, tau, r, sigma) {
  d1 <- (log(S/K) + (r - q + 0.5*sigma^2)*tau)/(sigma*sqrt(tau))
  d2 <- d1 - sigma*sqrt(tau)
  return(-K*tau*exp(-r*tau)*pnorm(-d2))
}