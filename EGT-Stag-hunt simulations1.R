

##Code written by: Imran Khan
## PhD student at Old Dominion University

##*****************Evolutionary game theory code for Stag-Hunt game **************##
##********************************************************************************##
## In evolutionary game theory, "the players interact with each other over time with their behaviors
## adjusting over time in response to the payoffs(fitness, profits, utilities etc.)that various choices
## have received historically" from the paper "Do people play Nash Equilibrium? Lessons from Evolutionary
## game theory". 

## We can also think of evolutionary game theory as the game in which all the players learn to
## play (or adopt) the most successful strategy in the game over multiple generations.The strategy that
## is most successful in the current generation is played by the larger fraction of the population in the
## next generation.Therefore, Unsuccessful strategy dies out over multiple generations. On the other hand, 
## successful strategy grows over multiple generations.

##*********************************Basin of attraction*************************************##
##*****************************************************************************************##
## Hunting stag (S,S) and hunting hare (H,H) are both Evolutionary Stable Strategies (ESS) in stag-hunt game.
## ESS is resistant to invasion by the alternative strategies in the population.The multiplicity of equilibria
## in Stag-hunt game raises an important question regarding the basin of attraction.The literature on evolutionary
## game theory suggests that hunting hare has the wide basin of attraction as compared to the hunting stag.

## Basin of attraction is the set of initial conditions that will evolve(lead) the dynamic system to a particular equilibrium.
## IF we have multiple equilibria in the game, then the question of which Nash equilibria will be played is determined 
## on the grounds of the "size" of "basin of attraction".
## If there is high strategic uncertainty in the multiple equilibria game, then the players play the equilibrium that has
## the wide basin of attraction.It is less risky to play for the Nash equilibrium with the larger size of basin of attraction.
## Although the (Stag, Stag)is a Pareto-efficient equilibrium, it has smaller basin of attraction.Through the simulations, we
## will try to find the minimum proportion of initial population that is required to play "stag" for the evolution of stag-stag
## equilibrium.

## The graph shows how the strategy grows or shrinks over the multiple generations (or iterations).

## You can re-run the simulations by changing the distribution of cooperate and defect players.

library(tidyverse)

num_generations <- 60000
num_players <- 10000
stag_players <- 7500  ##Try different proportion of initial population playing stag and hare strategies.
hare_players <- 2500

##*********Stag HUnt payoff matrix***********************##
##*******************************************************##
payoffs <- matrix(c(4,0,3,3), nrow = 2, byrow = T)
rownames(payoffs) <- c("S","H") # "S" stands for hunting Stag and "H" stands for hunting hare
colnames(payoffs) <- c("S","H")

##*********Expected Payoffs***************************##
##****************************************************##
# There are two strategies in the Stag-hunt game.
# The expected payoffs of hunting-stag and hunting-hare strategy is given below

# The expected payoff of any player hunting stag is given below
Expected_payoffs_s <- ((stag_players/num_players)*payoffs[1,1]) +
                     ((hare_players/num_players)*payoffs[1,2])

# The expected payoff of any player hunting hare is given below
Expected_payoffs_h <- ((stag_players/num_players)*payoffs[2,1]) +
                      ((hare_players/num_players)*payoffs[2,2])


## The expected payoff of the strategy determines the reproductive success of the strategy in the 
## subsequent period(generations).

## The expected payoff of the strategy gives the fitness of the strategy. Converting the
## fitness of each strategy to respective probabilities.These probabilities will determine
## how the strategies will grow or shrink in the population over the period of time.

fitness_s <- (Expected_payoffs_s/(Expected_payoffs_s + Expected_payoffs_h ))
fitness_h <- (Expected_payoffs_h/(Expected_payoffs_s + Expected_payoffs_h ))

df<- data.frame()

for ( i in seq_along(1:num_generations)){
  if(stag_players > 0 & hare_players > 0){ ## For giving birth or dying, there must be atleast one agent
    if(fitness_s > runif(1,0,1)){
      stag_players = stag_players + 1
      hare_players  = hare_players - 1
    }
  }
  if(hare_players > 0 & stag_players > 0){
    if(fitness_h > runif(1,0,1)){
      hare_players = hare_players + 1
      stag_players = stag_players - 1
    }
  }
  Expected_payoffs_c <- ((stag_players/num_players)*payoffs[1,1]) +
                        ((hare_players/num_players)*payoffs[1,2])
  
  
  Expected_payoffs_d <- ((stag_players/num_players)*payoffs[2,1]) +
                        ((hare_players/num_players)*payoffs[2,2])
   
  fitness_s <- (Expected_payoffs_c/(Expected_payoffs_c + Expected_payoffs_d ))
  fitness_h <- (Expected_payoffs_d/(Expected_payoffs_c + Expected_payoffs_d ))
  cat("Stag players is", stag_players, " and hare players is ", hare_players, "in generation ",  i,    "\n")
  
  
  ## Collecting data in dataframe to visualizing the updating of frequencies of strategies
  new_row<- data.frame(i,stag_players,hare_players)
  df <- rbind(df, new_row)
  if((stag_players==num_players)||(hare_players==num_players)){
    break
  }
}


ggplot(data = df)+
  geom_smooth(mapping = aes(x = i, y = stag_players),se = F, color = "red")+
  geom_smooth(mapping = aes(x = i, y = hare_players),se = F, color = "blue") +
  labs(
    x = "Number of generations",
    y = "Players",
    title = "Evolution of strategies in Stag-hunt game",
    caption = "Data:Simulations"
  )+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.50),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())












