#################################################################################
##### ----- ##### ----- ##### ----- ###### ----- ##### ----- ##### ----- #####
### Date: April 9th, 2020
### Author: Peter Tea
### Purpose: Given posterior serve win probs, calculate prob win match, set scores.
# All code from Martin Ingram github. Originally written in Python but converted into R.


hold_serve_prob <- function(rally_win_prob){
  #Calculates the probability of holding serve.
  #  Args:
  #      vector rally_win_prob: The probability of winning a rally on serve.
  #  Returns:
  #      vector: The probability of holding serve.
  
  rally_lose_prob = 1 - rally_win_prob
  
  term_1 = (rally_win_prob)**4
  term_2 = ( 1 + 4*rally_lose_prob + 10*( (rally_lose_prob)**2 ) )
  
  first_summand = term_1 * term_2
  
  term_1 = 20 * ( (rally_win_prob * rally_lose_prob)**3 )
  term_2 = (rally_win_prob)**2
  term_3 = 1 / (1 - 2*rally_win_prob*rally_lose_prob)
  
  second_summand = term_1 * term_2 * term_3
  
  result <- first_summand + second_summand
  
  return(result)
  
}

###################################################################################

prob_reach_tiebreak_score <- function(i, j, win_serve_rally_prob_a,
                              win_serve_rally_prob_b){
  #"""The probability of reaching a given tiebreak score when player a serves
  #  first.
  #  Args:
  #      i (int): Score for player a.
  #      j (int): Score for player b.
  #      win_serve_rally_prob_a (vector): The probability that a wins a rally
  #          on their serve.
  #      win_serve_rally_prob_b (vector): The probability that b wins a rally
  #          on their serve.
  #  Returns:
  #      np.array: The probability of reaching tiebreak score [i, j].
  #  """
  
  
  # Helpful renamings
  
  lose_serve_rally_prob_a = 1 - win_serve_rally_prob_a
  lose_serve_rally_prob_b = 1 - win_serve_rally_prob_b
  
  a_served_last = (((i - 1 + j) %% 4 == 0) | ((i - 1 + j) %% 4 == 3))
  
  # Initial conditions:
  
  if (i == 0 & j == 0){
    return(1)
  }
  
  if (i < 0 | j < 0){
    return(0)
  }
    
  
  if (a_served_last){
    total = 0
    
    if (!(i == 7 & j <= 6)){
    total = total + prob_reach_tiebreak_score(
      i, j-1, win_serve_rally_prob_a, win_serve_rally_prob_b) * (
        lose_serve_rally_prob_a)
    }
    
    if (! (j == 7 & i <= 6)){
    total = total + (prob_reach_tiebreak_score(
      i-1, j, win_serve_rally_prob_a, win_serve_rally_prob_b) *
        win_serve_rally_prob_a)
    }
    
    return(total)
    
  } else {
    
    total = 0
    
    if (!(j == 7 & i <= 6)){
      total = total + (prob_reach_tiebreak_score(
        i-1, j, win_serve_rally_prob_a, win_serve_rally_prob_b) * 
          lose_serve_rally_prob_b)
      
    }
    
    if (!(i == 7 & j <= 6)){
      total = total + (prob_reach_tiebreak_score(
        i, j-1, win_serve_rally_prob_a, win_serve_rally_prob_b) *
          win_serve_rally_prob_b)
    }
    

  return(total) 
  }
}

##########################################################################
prob_win_tiebreak_a <- function(win_serve_rally_prob_a, win_serve_rally_prob_b){
  #"""Calculates the probability that a wins a tiebreak.
  #  Args:
  #      win_serve_rally_prob_a (np.array): The probability that player a wins
  #          a rally on their own serve.
  #      win_serve_rally_prob_b (np.array): The probability that player b wins
  #          a rally on their own serve.
  #  Returns:
  #      np.array: The probability a wins the tiebreak.
  #  """
  
  total = 0
  lose_serve_rally_prob_a = 1 - win_serve_rally_prob_a
  lose_serve_rally_prob_b = 1 - win_serve_rally_prob_b
  
  for(j in 0:5){
    total = total + prob_reach_tiebreak_score(7, j, win_serve_rally_prob_a,
                                       win_serve_rally_prob_b)
    

  }
  total = total + (prob_reach_tiebreak_score(6, 6, win_serve_rally_prob_a,
                                             win_serve_rally_prob_b) *
                     win_serve_rally_prob_a * lose_serve_rally_prob_b *
                     1/(1 - win_serve_rally_prob_a * win_serve_rally_prob_b -
                          lose_serve_rally_prob_a * lose_serve_rally_prob_b))
  
  return(total)

}
##########################################################################
prob_win_set_a <- function(win_serve_rally_prob_a, win_serve_rally_prob_b){
  #"""Calculates the probability that player a wins a set.
  #  Args:
  #      win_serve_rally_prob_a (np.array): The probability that player a wins
  #          a rally on their own serve.
  #      win_serve_rally_prob_b (np.array): The probability that player b wins
  #          a rally on their own serve.
  #  Returns:
  #      np.array: The probability that player a wins a set.
  #  """
  
  total = 0
  
  for(j in 0:4){
    total = total + prob_reach_set_score(6, j, win_serve_rally_prob_a,
                                  win_serve_rally_prob_b)

  }
  
  total = total + prob_reach_set_score(7, 5, win_serve_rally_prob_a,
                                       win_serve_rally_prob_b)
  
  total = total + prob_reach_set_score(7, 6, win_serve_rally_prob_a,
                                       win_serve_rally_prob_b)
  
  return(total)
}
##########################################################################
prob_reach_set_score <- function(i, j, win_serve_rally_prob_a, win_serve_rally_prob_b){
  
  #"""The probability of reaching a given set score when player A serves
  #  first.
  #  Args:
  #      i (int): Score for player a.
  #      j (int): Score for player b.
  #      win_serve_rally_prob_a (vector): The probability that a wins a rally
  #          on their serve.
  #      win_serve_rally_prob_b (vector): The probability that b wins a rally
  #          on their serve.
  #  Returns:
  #      vector: The probability of reaching set score [i, j].
  #  """
  
  #if ((j <= 6 & i <= 6) | (i == 7 & j <= 6) | (i <= 6 & j == 7)){
  #  print('Please provide a valid set score!')
  #  return(NULL)
  #} 
  
  
  hold_serve_prob_a = hold_serve_prob(win_serve_rally_prob_a)
  hold_serve_prob_b = hold_serve_prob(win_serve_rally_prob_b)
  
  # Helpful renamings
  lose_serve_prob_a = 1 - hold_serve_prob_a
  lose_serve_prob_b = 1 - hold_serve_prob_b
  
  a_served_last = ((i - 1 + j) %% 2 == 0)
  
  # Initial conditions
  if (i == 0 & j == 0){
    return(1)
  }
    
  
  if (i < 0 | j < 0){
    return(0)
  }
    
  
  # We have the tiebreak case:
  if (i == 6 & j == 7){
    return(prob_reach_set_score(
      i, j - 1, win_serve_rally_prob_a, win_serve_rally_prob_b) * (
        1 - prob_win_tiebreak_a(win_serve_rally_prob_a,
                                win_serve_rally_prob_b)))
    
    
  } else if (i == 7 & j == 6){
    return(prob_reach_set_score(
      i - 1, j, win_serve_rally_prob_a, win_serve_rally_prob_b) * (
        prob_win_tiebreak_a(win_serve_rally_prob_a,
                            win_serve_rally_prob_b)))
    
  }

  # We also have the 7-5 case:
  if (i == 7 & j == 5){
    return(prob_reach_set_score(i - 1, j, win_serve_rally_prob_a,
                                win_serve_rally_prob_b) * lose_serve_prob_b)
    
  } else if (i == 5 & j == 7){
    return(prob_reach_set_score(i, j - 1, win_serve_rally_prob_a,
                                win_serve_rally_prob_b) * hold_serve_prob_b)
    
    
  }

  # Two possibilities
  if (a_served_last){
    total = 0
    
    if (! (j == 6 & i <= 5)){
      total = total + (prob_reach_set_score(i-1, j, win_serve_rally_prob_a,
                                     win_serve_rally_prob_b) *
                  hold_serve_prob_a)
      
    }
    
    if (! (i == 6 & j <= 5)){
    
      total = total + (prob_reach_set_score(i, j-1, win_serve_rally_prob_a,
                                       win_serve_rally_prob_b) *
                    lose_serve_prob_a)  
      
    }
        
    return(total)
    
  } else{
    total = 0
  
    if (! (j == 6 & i <= 5)){
      total = total + (prob_reach_set_score(i-1, j, win_serve_rally_prob_a,
                                   win_serve_rally_prob_b) *
                lose_serve_prob_b)
    }
    
    if (! (i == 6 & j <= 5)){
      
      total = total + (prob_reach_set_score(i, j-1, win_serve_rally_prob_a,
                                   win_serve_rally_prob_b) *
                hold_serve_prob_b)
    }
    
   
  
    return(total) 
  }
}
##########################################################################
prob_win_match_a <- function(win_serve_rally_prob_a, win_serve_rally_prob_b,
                     best_of_five=FALSE){
  #"""Calculates the probability that player a wins the match.
  #  Args:
  #      win_serve_rally_prob_a (vector): The probability that player a wins
  #          a rally on their own serve.
  #      win_serve_rally_prob_b (vector): The probability that player b wins
  #          a rally on their own serve.
  #      best_of_five (Bool): Whether or not the match is in best-of-five
  #          format. If False, it is assumed to be best-of-three.
  #  Returns:
  #      vector: The probability that player a wins the match.
  #  """
  
  prob_a_win_set = prob_win_set_a(win_serve_rally_prob_a,
                                  win_serve_rally_prob_b)
  
  prob_b_win_set = prob_win_set_a(win_serve_rally_prob_b,
                                  win_serve_rally_prob_a)
  
  total = 0
  
  if (!best_of_five){
    total = total +  (prob_a_win_set)**2
    total = total +  2 * (prob_a_win_set**2) * prob_b_win_set
    
  } else{
    total = total +  ((prob_a_win_set**3) + 3*(prob_a_win_set**3)*
                         prob_b_win_set + 6*(prob_a_win_set**3)*
                         (prob_b_win_set**2))
    
  }
  
  return(total)
  
  
  
}
  





  

###########################################################################
# --> Plot the likely set scores b/n player A and player B

get_most_likely_set_scores <- function(serve_win_df, player1, player2){
  # posterior_serve_win_dat: data w/ 2 columns of serve win probs
  # player1: Name of player 1 (aesthetic)
  # player2: Name of player 2 (aesthetic)
  # tournament: Name of tournament (aesthetic)
  
  #player1 <- levels(serve_win_df$Player)[1]
  #player2 <- levels(serve_win_df$Player)[2]
  
  posterior_serve_win_dat <- cbind(serve_win_df %>%
                                     filter(Player == player1) %>%
                                            pull(p_spw), 
                                   serve_win_df %>% 
                                     filter(Player == player2) %>%
                                           pull(p_spw))
  
  poss_outcomes = list(c(6, 0), c(6, 1), c(6, 2), c(6, 3), c(6, 4), 
                       c(7, 5), c(7, 6))
  
  ## --> Assuming Player A serves 1st
  outcome_dict = vector()
  for (poss_outcome in poss_outcomes){
    outcome_dict =cbind(outcome_dict, round(prob_reach_set_score(
      poss_outcome[1], poss_outcome[2], posterior_serve_win_dat[,1], 
      posterior_serve_win_dat[,2]), 3))
    
    outcome_dict = cbind(outcome_dict, round(prob_reach_set_score(
      poss_outcome[2], poss_outcome[1], posterior_serve_win_dat[,1],
      posterior_serve_win_dat[,2]),3))
  }
  
  
  #dim(outcome_dict)
  ## --> Assuming Player B serves 1st
  outcome_dict2 = vector()
  for (poss_outcome in poss_outcomes){
    outcome_dict2 =cbind(outcome_dict2, round(prob_reach_set_score(
      rev(poss_outcome)[1], rev(poss_outcome)[2], posterior_serve_win_dat[,2], 
      posterior_serve_win_dat[,1]), 3))
    
    outcome_dict2 = cbind(outcome_dict2, round(prob_reach_set_score(
      rev(poss_outcome)[2], rev(poss_outcome)[1], posterior_serve_win_dat[,2],
      posterior_serve_win_dat[,1]),3))
  }
  
  
  likely_set_scores = data.frame(PlayerA_Score = c(6,0,6,1,6,2,6,3,6,4,7,5,7,6),
                                 PlayerB_Score = c(0,6,1,6,2,6,3,6,4,6,5,7,6,7),
                                 prob = (colMeans(outcome_dict) + colMeans(outcome_dict2))/2)
  
  likely_set_scores <- likely_set_scores %>%
    arrange(desc(prob)) %>%
    mutate(score = paste0('( ',PlayerA_Score, " - ", PlayerB_Score, ' )')) %>%
    mutate(score= forcats::fct_reorder(score, prob)) 
  
  #levels(likely_set_scores) <- likely_set_scores$score

  
  colnames(likely_set_scores) <- c(sub( x=paste0(player1, '_score'), " ", "_"),
                                   sub( x=paste0(player2, '_score'), " ", "_"),
                                   'prob',
                                   'score'
                                   )
  return(likely_set_scores)

  
}


get_most_likely_match_scores <- function(serve_win_df, player1, player2){
  # posterior_serve_win_dat: data w/ 2 columns of serve win probs
  # player1: Name of player 1 (aesthetic)
  # player2: Name of player 2 (aesthetic)
  # tournament: Name of tournament (aesthetic)
  
  #player1 <- levels(serve_win_df$Player)[1]
  #player2 <- levels(serve_win_df$Player)[2]
  
  posterior_serve_win_dat <- cbind(serve_win_df %>%
                                     filter(Player == player1) %>%
                                     pull(p_spw), 
                                   serve_win_df %>% 
                                     filter(Player == player2) %>%
                                     pull(p_spw))
  
  prob_p1_winning_a_set <- prob_win_set_a(posterior_serve_win_dat[,1],
                                       posterior_serve_win_dat[,2])
  prob_p2_winning_a_set <- 1- prob_p1_winning_a_set
  # -- Note: This should also be: prob_win_set_a(posterior_serve_win_dat[,2],posterior_serve_win_dat[,1])

  # -- If Best of 3 Sets:
  # -- Probability winning in 2 sets
  p1_in_2_sets <- prob_p1_winning_a_set**2
  p1_in_3_sets <- 2*(prob_p1_winning_a_set**2)*prob_p2_winning_a_set
  
  p2_in_2_sets <- prob_p2_winning_a_set**2
  p2_in_3_sets <- 2*(prob_p2_winning_a_set**2)*prob_p1_winning_a_set
  
  likely_match_scores = data.frame(p1_score = c(2,2,0,1),
                                   p2_score = c(0,1,2,2),
                                   prob = c(mean(p1_in_2_sets),
                                            mean(p1_in_3_sets),
                                            mean(p2_in_2_sets),
                                            mean(p2_in_3_sets))
                                   )
  likely_match_scores <- 
  likely_match_scores%>%
    arrange(desc(prob)) %>%
    mutate(score = paste(p1_score, "-", p2_score)) %>%
    mutate(score= forcats::fct_reorder(score, prob)) 
  
  colnames(likely_match_scores) <- c(sub( x=paste0(player1, '_score'), " ", "_"),
                                   sub( x=paste0(player2, '_score'), " ", "_"),
                                   'prob',
                                   'score'
  )
  
 return(likely_match_scores)
  
}


###########################################################################
# Test

#prob_reach_set_score(6, 4, 0.8, 0.8)

#prob_win_match_a(p,p,T)





 



 