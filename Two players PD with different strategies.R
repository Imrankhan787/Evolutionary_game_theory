
# The code written by : Imran Khan, PhD student, ODU
# Primary concentration : Modeling and Simulation.



##************Two players repeated Prisoner's dilemma game with different strategies***********************##
##*********************************************************************************************************##


payoff_matrix <- matrix(c(3,0,5,1), nrow = 2, byrow = 2)
rownames(payoff_matrix) <-c("C","D")
colnames(payoff_matrix) <- c("C","D")

strategies <- c("Always Cooperate","Always Defect", "Grim Trigger",
                "TFT", "Random", "Generous TFT")

##*********Strategies explanation********************##
##***************************************************##

#Always cooperate: always choosing the cooperative option, regardless of the actions of the other player

#Always defect: always choosing the non-cooperative option, regardless of the actions of the other player

#Random: This strategy involves choosing either the cooperative or non-cooperative option "randomly"

#Grim trigger: This strategy involves starting with cooperative behavior and then switching to non-cooperative behavior 
# forever if the other player defects.

#Generous tit for tat: This strategy is similar to the tit for tat strategy, but it occasionally cooperates even if the other player defects. 
#This can help to promote cooperation in situations where the other player is not using a tit for tat strategy


##**************************************************##
##**************************************************##

game_function <- function(player1_strategy = "Always Cooperate", 
                          player2_strategy = "Always Cooperate",
                          num_iterations = 1000){

# Create vectors to store the actions and payoffs for each player
player1_actions <- rep("C", num_iterations)
player2_actions <- rep("C", num_iterations)
player1_payoffs <- rep(0, num_iterations)
player2_payoffs <- rep(0, num_iterations)

# Set the initial action for player1 depending on the strategy 

if (player1_strategy == "Always Cooperate"){
  player1_action = "C"
}else if (player1_strategy == "Always Defect"){
  player1_action = "D"
}else if (player1_strategy == "Grim Trigger"){
  player1_action = "C"
}else if (player1_strategy == "Random"){
 player1_action = sample(c("C","D"),1)
}else if (player1_strategy =="TFT"){
  player1_action = "C"
}else if (player1_strategy == "Generous TFT"){
  player1_action = "C"
}else {
  cat("unable to understand ", player1_strategy, "\\n")
}

# Set the initial action for player2 depending on the strategy 

if (player2_strategy == "Always Cooperate"){
  player2_action = "C"
}else if (player2_strategy == "Always Defect"){
  player2_action = "D"
}else if (player2_strategy == "Grim Trigger"){
  player2_action = "C"
}else if (player2_strategy == "Random"){
  player2_action = sample(c("C","D"),1)
}else if (player2_strategy =="TFT"){
  player2_action = "C"
} else if (player2_strategy == "Generous TFT" ){
  player2_action = "C"
}else {
  cat("unable to understand ", player2_strategy, "\\n")
}



# Iterate through the number of iterations
for (i in 1:num_iterations) {
  
  # Store the actions of each player in the corresponding vector
  player1_actions[i] <- player1_action
  player2_actions[i] <- player2_action
  
  # Calculate the payoffs for each player based on their actions
  if (player1_action == "C" & player2_action == "C") {
    player1_payoffs[i] <- payoff_matrix[1,1]
    player2_payoffs[i] <- payoff_matrix[1,1]
  } else if (player1_action == "C" & player2_action == "D") {
    player1_payoffs[i] <- payoff_matrix[1,2]
    player2_payoffs[i] <- payoff_matrix[2,1]
  } else if (player1_action == "D" & player2_action == "C") {
    player1_payoffs[i] <- payoff_matrix[2,1]
    player2_payoffs[i] <- payoff_matrix[1,2]
  } else if (player1_action == "D" & player2_action == "D") {
    player1_payoffs[i] <- payoff_matrix[2,2]
    player2_payoffs[i] <- payoff_matrix[2,2]
  }
  
  # Update the action for player 1 for different strategies
  if (player1_strategy == "Always Cooperate"){
    player1_action = "C"
  }else if (player1_strategy == "Always Defect"){
    player1_action = "D"
  }else if (player1_strategy == "Grim Trigger"){
    if(any(player2_actions == "D")){
      player1_action = "D"
    }
  }else if (player1_strategy == "Random"){
    player1_action = sample(c("C","D"),1)
  }else if (player1_strategy =="TFT"){
    player1_action = player2_actions[i]
  } else if (player1_strategy == "Generous TFT"){
    if(player2_action =="C"){
      player1_action = "C"
    }else{
      player1_action <- sample(c("C","D"),size =1, prob = c(0.90, 0.10))
    }
  } else {
    cat("unable to understand ", player1_strategy, "\\n")
  }
  
  # Update the action for player 2 for different strategies
  if (player2_strategy == "Always Cooperate"){
    player2_action = "C"
  }else if (player2_strategy == "Always Defect"){
    player2_action = "D"
  }else if (player2_strategy == "Grim Trigger"){
    if(any(player1_actions == "D")){
      player2_action = "D"
    }
  }else if (player2_strategy == "Random"){
    player2_action = sample(c("C","D"),1)
  }else if (player2_strategy =="TFT"){
    player2_action = player1_actions[i]
  } else if (player2_strategy == "Generous TFT"){
    if(player1_action == "C"){
      player2_action = "C"
    }else{
      player2_action = sample(c("C","D"),size = 1, prob = c(0.90,0.10))
    }
  }else {
    cat("unable to understand ", player2_strategy, "\\n")
  }
}

# Calculate the total payoffs for each player
player1_total_payoff <- sum(player1_payoffs)
player2_total_payoff <- sum(player2_payoffs)

# Print the results
list(paste("Player 1 total payoff:", player1_total_payoff),
     paste("Player 2 total payoff:", player2_total_payoff))
                                                
}

##*****************************************************************##
##*****************************************************************##
##*****************************************************************##

#Call this function using different strategies for player 1 and player 2. You can choose from the following strategies
strategies <- c("Always Cooperate","Always Defect", "Grim Trigger",
                "TFT", "Random", "Generous TFT")

game_function(player1_strategy = "Generous TFT",
              player2_strategy = "Random",
              num_iterations = 1000)

