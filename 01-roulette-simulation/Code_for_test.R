#### Extra interests for earnings that based on each series. What if we are more focusing for the process of win and lose?
# The average earning for series should be:


average_earnings_series <- sum(one_series()-200)/1000


# how changing a parameter of the simulation (see table below) does or does not have an impact on average earnings for series?
# We can still try to change W from 300 to 500.


one_series_change <- function(
  B = 200
  , W = 500
  , L = 1000
  , M = 100
){
  
  # initial state
  state <- list(
    B = B
    , W = W
    , L = L
    , M = M
    , plays = 0
    , previous_wager = 0
    , previous_win = TRUE
  )
  
  # vector to store budget over series of plays
  budget <- rep(NA, L)
  
  # For loop of plays
  for(i in 1:L){
    new_state <- state %>% one_play
    budget[i] <- new_state$B
    if(new_state %>% stop_play){
      return(budget[1:i])
    }
    state <- new_state
  }
  budget    
}
a <- one_series_change()
average_earnings_change <- sum(a-200)/1000


# Here we show the graph for the previous average earnings with 1000 times repeatting. Then each of the previous average earning value has been plotted in one graph.


set_series<-c()
count <- 0
while(count<1000) {
  
  add <- sum(one_series()-200)/1000
  set_series <- c(set_series,add)
  count=count+1
}
plot(set_series)


# Here we show the graph for the new average earnings with 1000 times repeatting. Then each of the previous average earning value has been plotted in one graph.

set_changed<-c()
count <- 0
while(count<1000) {
  
  add_changed <- sum(one_series_change()-200)/1000
  set_changed <- c(set_changed,add_changed)
  count=count+1
}
plot(set_changed)


# From the two graphs, we can see that there have been some impacts on average earnings for series. Despite the overall value of average earning is larger when W = 500, when W = 300, there are two distinct horizontal significant related lines: one is around 0, and another is around 10. When W = 500, there is only one which is around 0.
