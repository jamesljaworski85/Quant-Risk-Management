##############################################################################################
# Using Full Revaluation, calculate the price of the put option for AAPL using the previously
# defined formula called 'bsput.value'
# bsput.value <- function(S, K, q, tau, r, sigma)
# S = current_price
# K = strike_price
# q = continuous dividend yield
# tau = time to maturity (expressed in days/365)
# r = risk-free rate
# sigma = volatiltiy
# STEP 1
# calculate the put price by full revaluation
# IMPORTANT: make sure to run a 'for' loop on the function 'bsput.value', where 'i' iterates 
# over the 'shifted_current_price' at each previous time period
# STEP 2
# Then, write a for loop that calculates the change in option price
# STEP 3
# write a for loop that calculates the normalized relative shifts
##############################################################################################

# STEP 1

full.revaluation <- function(S,K,q,tau,r,sigma,shift_col=0,option_type){
  vr <- c() # an empty vector
  for (i in 1:length(aapl_data$Date)) {
    if (option_type == "call"){
      vr[i] <- bscall.value(S + shift_col[i],K,q,tau,r,sigma)
    } else if (option_type == "put"){
      vr[i] <- bsput.value(S + shift_col[i],K,q,tau,r,sigma)
    } else {
      print("please enter 'call' or 'put' for option_type")
      break
    }
  }
  vr
}
aapl_data$fullreval_putprice <- vr


# STEP 2
vr <- c() # an empty vector
for (i in 1:length(aapl_data$Date)) {
  current_putprice <- bsput.value(S = aapl_data$AAPL[1],K = 144,q = 0.02, tau = 1, r = 0.03, 
                                  sigma = 0.25)
  vr[i] <- bsput.value(S = aapl_data$shifted_current_price[i],K = 144,q = 0.02, tau = 1, r = 0.03, 
                       sigma = 0.25) - current_putprice
}
aapl_data$change_in_putprice <- vr

#STEP 3
norm.relative.shift <- function(returns) {
  vr <- c() # an empty vector
  current_price <- returns[1]
  for (i in 1:length(returns)) {
    vr[i] <- current_price * perc.shift(returns)[i]
  }
  vr
}
aapl_data$normal_relative_shift <- norm.relative.shift(aapl_data$AAPL)
spy_data$normal_relative_shift <- norm.relative.shift(spy_data$SPY)