##############################################################################################
# Now calculate percentage shifts, absolute shifts, log differences, shifted current prices that will be used 
# full revaluation. Referencing 'current' in variable names is referring to value as of 
# 1/4/2019 when the data was collected.
##############################################################################################
perc.shift <- function(adj_close) {  
  # INPUT: the function takes in a column of stock prices
  # OUTPUT: a vector of percentage shifts
  
  vr <- c() ### an empty vector 
  for (i in 1:length(adj_close)) {
    vr[i] <- (adj_close[i]-adj_close[i+1])/(adj_close[i+1])
  }
  vr
}
aapl_data$pct_shift <- perc.shift(aapl_data$AAPL)
spy_data$pct_shift <- perc.shift(spy_data$SPY)

absolute.shift <- function(adj_close) {
  # INPUT: the function takes in a column of adj_close
  # OUTPUT: a vector of percentage shifts
  vr <- c() ### an empty vector 
  for (i in 1:length(adj_close)) {
    vr[i] <- adj_close[i] - adj_close[i+1]
  }
  vr
}

aapl_data$absolute_shift <- absolute.shift(aapl_data$AAPL)
spy_data$absolute_shift <- absolute.shift(spy_data$SPY)


log.diff <- function(adj_close){
  # INPUT: the function takes in a column of adj_close
  
  vr <- c() ### an empty vector
  for (i in 1:length(adj_close)) {
    vr[i] <- log(adj_close[i]/adj_close[i+1])
  }
  vr
}
aapl_data$log_difference <- log.diff(aapl_data$AAPL)
spy_data$log_difference <- log.diff(spy_data$SPY)

shift.curr.price <- function(adj_close) {
  # INPUT: the function takes in a column of adj_close
  # OUTPUT: a vector of shifted current price, to be used for full revaluation
  
  vr <- c() # an empty vector
  current_price <- adj_close[1]
  for (i in 1:length(adj_close)) {
    vr[i] <- current_price*(1+perc.shift(adj_close)[i])
  }
  vr
}
aapl_data$shifted_current_price <- shift.curr.price(aapl_data$AAPL)
spy_data$shifted_current_price <- shift.curr.price(spy_data$SPY)