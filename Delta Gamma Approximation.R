##############################################################################################
# Now write a function for delta-gamma approximation, called 'deltagamma.approx' that utilizes
# the function called 'bsput.delta' and 'bscall.gamma'. Remember that a call option has the
# the same gamma (or sensitivity to a 1-point move in the underlying).
# bsput.delta <- function(S, K, q, tau, r, sigma) 
# bscall.gamma <- function(S, K, q, tau, r, sigma)
##############################################################################################
DG.approx <- function(S, K, q, tau, r, sigma, adj_close, shift, delta, gamma){
  # This function uses 'analytical' delta
  # gamma.
  # INPUT:
  # S = current_price
  # K = strike_price
  # q = continuous dividend yield
  # tau = time to maturity (expressed in days/365)
  # r = risk-free rate
  # sigma = volatiltiy
  # adj_close = a column of daily adjusted close prices for a particular asset
  # shift = the column vector of results from the function called
  # 'norm.relative.shift' or 'absolute_shift'.
  # OUTPUT: 
  # A vector that contains the Delta-Gamma approximation for the value of the option
  
  vr <- c() # an empty vector
  for (i in 1:length(adj_close)){
    vr[i] <- delta*shift[i] + 0.5*gamma*((shift[i])^2)
  } 
  vr
}