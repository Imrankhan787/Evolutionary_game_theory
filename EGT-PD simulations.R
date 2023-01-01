
##Code written by: Imran Khan
## PhD student at Old Dominion University

##*****************Evolutionary game theory code for Prisoner's dilemma***********##
##********************************************************************************##
## Evolutionary Stable Strategy in the Prisoner's dilemma game is the "Defect" Strategy.ESS is resistant to 
## invasion by the alternative strategies in the population.

## All the players end up playing the "defect" strategy because "defect" strategy is the most successful one.
## Unsuccessful strategy dies out over multiple generations. On the other hand, successful strategy grows
## out over multiple generations.The cooperate strategy is unsuccessful one whereas the defect strategy is
## the successful one.

## The graph shows how the strategy grows or shrinks over the multiple generations (or iterations).

## You can re-run the simulations by changing the distribution of cooperate and defect players.

library(tidyverse)

num_generations <- 30000
num_players <- 10000
cooperate_players <- 9000
defect_players <- 1000

##*********Prisoner's dilemma payoff*********************##
##*******************************************************##
payoffs <- matrix(c(3,0,5,1), nrow = 2, byrow = T)
rownames(payoffs) <- c("C","D")
colnames(payoffs) <- c("C","D")

##*********Expected Payoffs***************************##
##****************************************************##
# The expected payoffs of the cooperate and defect strategy is

Expected_payoffs_c <- ((cooperate_players/num_players)*payoffs[1,1]) +
                      ((defect_players/num_players)*payoffs[1,2])


Expected_payoffs_d <- ((cooperate_players/num_players)*payoffs[2,1]) +
                      ((defect_players/num_players)*payoffs[2,2])


## The expected payoff of the strategy determines the reproductive success of the strategy in the subsequent period.
## The expected payoff of the strategy gives the fitness of the strategy. Converting the
## fitness of each strategy to respective probabilities.These probabilities will determine
## how the strategies will grow or shrink in the population over the period of time.

fitness_c <- (Expected_payoffs_c/(Expected_payoffs_c + Expected_payoffs_d ))
fitness_d <- (Expected_payoffs_d/(Expected_payoffs_c + Expected_payoffs_d ))

 df<- data.frame()
 
for ( i in seq_along(1:num_generations)){
  if(cooperate_players > 0 & defect_players > 0){ ## For giving birth or dying, there must be atleast one agent
    if(fitness_c > runif(1,0,1)){
      cooperate_players = cooperate_players + 1
      defect_players  = defect_players - 1
    }
  }
  if(defect_players > 0 & cooperate_players > 0){
    if(fitness_d > runif(1,0,1)){
      defect_players = defect_players + 1
      cooperate_players = cooperate_players - 1
    }
  }
  Expected_payoffs_c <- ((cooperate_players/num_players)*payoffs[1,1]) +
    ((defect_players/num_players)*payoffs[1,2])
  
  
  Expected_payoffs_d <- ((cooperate_players/num_players)*payoffs[2,1]) +
    ((defect_players/num_players)*payoffs[2,2])
  
  fitness_c <- (Expected_payoffs_c/(Expected_payoffs_c + Expected_payoffs_d ))
  fitness_d <- (Expected_payoffs_d/(Expected_payoffs_c + Expected_payoffs_d ))
  cat("Cooperate players is", cooperate_players, " and defect players is ", defect_players,"\n")
  
  
  ## Collecting data in dataframe to visualizing the updating of frequencies of strategies
  new_row<- data.frame(i,cooperate_players,defect_players)
  df <- rbind(df, new_row)
}

 
 ggplot(data = df)+
 geom_smooth(mapping = aes(x = i, y = cooperate_players),se = F, color = "red")+
 geom_smooth(mapping = aes(x = i, y = defect_players),se = F, color = "blue") +
   labs(
     x = "Number of generations",
     y = "Players",
     title = "Evolution of strategies in prisoner's Dilemma game",
     caption = "Data:Simulations"
   )+
   theme_bw()+
   theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.50),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
 
















