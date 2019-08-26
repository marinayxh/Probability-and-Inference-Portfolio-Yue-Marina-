#' A single play of the Martingale strategy
#' This type of system is based on the idea that you will double your bet after losing trades and—in theory—you will always cover your losses with winning bets that are double the amount of the losing bet.
#'
#' Takes a state list, spins the roulette wheel, returns the state list with updated values (for example, budget, plays, etc)
#' @param state A list with the following entries: 
#'   B              number, the budget
#'   W              number, the budget threshold for successfully stoping
#'   L              number, the maximum number of plays 
#'   M              number, the casino wager limit
#'   plays          integer, the number of plays executed
#'   previous_wager number, the wager in the previous play (0 at first play)
#'   previous_win   TRUE/FALSE, indicator if the previous play was a win (TRUE at first play)
#' @return The updated state list
library(dplyr)
library(ggplot2)
one_play <- function(state){
  
    # Wager
    proposed_wager <- ifelse(state$previous_win, 1, 2*state$previous_wager)
    wager <- min(proposed_wager, state$M, state$B)
    
    # Spin of the wheel
    red <- rbinom(1,1,18/38)
    
    # Update state
    state$plays <- state$plays + 1
    state$previous_wager <- wager
    if(red){
      # WIN
      state$B <- state$B + wager
      state$previous_win <- TRUE
    }else{
      # LOSE
      state$B <- state$B - wager
      state$previous_win <- FALSE
    }
  state
}


#' Stopping rule
#'
#' Takes the state list and determines if the gambler has to stop
#' @param state A list.  See one_play
#' @return TRUE/FALSE
stop_play <- function(state){
  if(state$B <= 0) return(TRUE)
  if(state$plays >= state$L) return(TRUE)
  if(state$B >= state$W) return(TRUE)
  FALSE
}


#' Play roulette to either bankruptcy, success, or play limits
#'
#' @param B number, the starting budget
#' @param W number, the budget threshold for successfully stoping
#' @param L number, the maximum number of plays 
#' @param M number, the casino wager limit
#' @return A vector of budget values calculated after each play.
one_series <- function(
    B = 200
  , W = 300
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

# helper function
get_last <- function(x) x[length(x)] 


# Simulation
walk_out_money <- rep(NA, 10000)
for(j in seq_along(walk_out_money)){
  walk_out_money[j] <- one_series(B = 200, W = 300, L = 1000, M = 100) %>% get_last
}

# Walk out money distribution
hist(walk_out_money, breaks = 100)

# Estimated probability of walking out with extra cash
mean(walk_out_money > 200)

# Estimated earnings
mean(walk_out_money - 200)

# Average earnings of a gambler that uses this strategy?
# By repeating the process, we could estimate the operating characteristics for simulating distribution. We sum the value of walk out money and divide maximum number of plays to receive the average earnings. 
average_earnings <- sum(one_series()-200)/1000

# provide a figure (or a series of figures) that show how the gamblers earnings (or losses) evolve over a series of wagers at the roulette wheel?
# The x-axis will be the wager number (or play number), the y-axis will be earnings.
# We could see that in one specific series, before the gambler stops, we can reach the winnings threshold in a long run. Some outliers do exist.
plot(one_series(), main=paste("Distribution of Earnings"), xlab="play number", ylab="earnings")

# how changing a parameter of the simulation (see table below) does or does not have an impact on average earnings?
# We can try to change W from 300 to 500.
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
set<-c()
count <- 0
while(count<1000) {

    add <- sum(one_series()-200)/1000
    set <- c(set,add)
    count=count+1
}
plot(set)
# Here we show the graph for the new average earnings with 1000 times repeatting. Then each of the previous average earning value has been plotted in one graph.
set_changed<-c()
count <- 0
while(count<1000) {

    add_changed <- sum(one_series_change()-200)/1000
    set_changed <- c(set_changed,add_changed)
    count=count+1
}
plot(set_changed)
# From the two graphs, we can see that there have been some impacts on average earnings. Despite the overall value of average earning is larger when W = 500, when W = 300, there are two distinct horizontal significant related lines: one is around 0, and another is around 10. When W = 500, there is only one which is around 0.

# how to estimate the average number of plays before stopping?
# We store the value of each time (total repeatting time is 100) the number of plays for a series in a list, and then calculate the average of the sum of these values. We receive a number of (about) 205. 
set_play<-c()
i <- 0
while(i < 100) {

    add_play <- length(one_series())
    set_play <- c(set_play,add_play)
    i=i+1
}
mean(set_play)

# Any limitation or source of uncertainty?
# We are not sure if a gambler have enough time to stay in this casino and win his money back.
# We are not sure if a gambler's heart is strong enough for losing money now, and wait for winning money back.
# A gambler may go bankrupt before striking a win.
# Table limit is sometimes different from casinos, and it can prevent large winning streaks.