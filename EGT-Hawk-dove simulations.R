
##Code written by: Imran Khan
## PhD student at Old Dominion University

##*****************Evolutionary game theory code for Hawk Dove game***************##
##********************************************************************************##
## Evolutionary Stable Strategy(ESS) in the hawk-dove game is the mixed strategy in which
## 1/3rd of the population is playing "dove" strategy and 2/3rd of the population is
## playing the "hawk" strategy.ESS is resistant to invasion by the alternative strategies
## in the population.

## When one-third of the players are "doves" in the population and two-third are the "hawks", 
## then the players of both types have the "same" fitness.

## The simulation of the following Hawk-Dove game theoretic model demonstrates that the 
## [Dove, Hawk] = [1/3, 2/3] is indeed ESS irrespective of the initial distribution of players.

## The graph shows how the strategy grows or shrinks over the multiple generations (or iterations).

## You can re-run the simulations by changing the distribution of dove and hawk players.


library(tidyverse)

num_generations <- 80000
num_players <- 10000
dove_players <- 1000
hawk_players <- 9000 

##*********Hawk-dove game payoffs************************##
##*******************************************************##
# The following matrix gives the payoffs for the row player when he plays against the column player.
# Hawk-dove game is symmetric in payoffs and strategies for the two players. 
payoffs <- matrix(c(3,1,5,0), nrow = 2, byrow = T) 
rownames(payoffs) <- c("D","H") ## D stands for Dove and H stands for Hawk
colnames(payoffs) <- c("D","H")

##*********Expected Payoffs***************************##
##****************************************************##
# The expected payoffs of the hawk and dove strategy is

Expected_payoffs_d <- ((dove_players/num_players)*payoffs[1,1]) +
                      ((hawk_players/num_players)*payoffs[1,2])


Expected_payoffs_h <- ((dove_players/num_players)*payoffs[2,1]) +
                      ((hawk_players/num_players)*payoffs[2,2])


## The expected payoff of the strategy determines the reproductive success of the strategy in the subsequent round.
## The expected payoff of the strategy gives the fitness of the strategy. Converting the
## fitness of each strategy to respective probabilities.These probabilities will determine
## how the strategies will grow or shrink in the population over the period of time.

fitness_d <- (Expected_payoffs_d/(Expected_payoffs_d + Expected_payoffs_h ))
fitness_h <- (Expected_payoffs_h/(Expected_payoffs_d + Expected_payoffs_h ))

df<- data.frame()

for ( i in seq_along(1:num_generations)){
  if(dove_players > 0 & hawk_players > 0){ ## For giving birth or dying, there must be atleast one agent
    if(fitness_d > runif(1,0,1)){
      dove_players = dove_players + 1
      hawk_players  = hawk_players - 1
    }
  }
  if(hawk_players > 0 &  dove_players > 0){
    if(fitness_h > runif(1,0,1)){
      hawk_players = hawk_players + 1
      dove_players = dove_players - 1
    }
  }
  
  Expected_payoffs_d <- ((dove_players/num_players)*payoffs[1,1]) +
                        ((hawk_players/num_players)*payoffs[1,2])
  
  
  Expected_payoffs_h <- ((dove_players/num_players)*payoffs[2,1]) +
                        ((hawk_players/num_players)*payoffs[2,2])
  
  fitness_d <- (Expected_payoffs_d/(Expected_payoffs_d + Expected_payoffs_h ))
  fitness_h <- (Expected_payoffs_h/(Expected_payoffs_d + Expected_payoffs_h ))
  
  cat("Dove players is", dove_players, " and hawk players is ", hawk_players,"\n")
  
  
  ## Collecting data in dataframe to visualizing the updating of frequencies of strategies
  new_row<- data.frame(i,dove_players,hawk_players)
  df <- rbind(df, new_row)
}


ggplot(data = df)+
  geom_smooth(mapping = aes(x = i, y = dove_players),se = F, color = "red")+
  geom_smooth(mapping = aes(x = i, y = hawk_players),se = F, color = "blue") +
  labs(
    x = "Number of generations",
    y = "Players",
    title = "Evolution of strategies in Hawk-dove game",
    caption = "Data:Simulations"
  )+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.50),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

















