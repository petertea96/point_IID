library(dplyr)

get_serve_win_posterior_dataframe <-function(match_data, 
                                             stan_results, 
                                             player_1_name = "Denis Shapovalov",
                                             player_2_name = "Frances Tiafoe",
                                             tournament_name = "canada masters",
                                             surface_name = "Hard"){
  
  # -- Get Player IDs
  # -- List all (full) player names in data as a factor
  all_player_names = unlist(list(match_data[, "server"], match_data[,'returner']))
  
  # -- Get player ID number (factor level) for the input player names
  player1_id = which(levels(all_player_names) == player_1_name)
  player2_id = which(levels(all_player_names) == player_2_name)
  
  # Get Tournament ID (i.e. factor level)
  tournament_id = which(levels(match_data[,'tournament']) == tournament_name)
  
  # Get Surface ID (i.e. factor level)
  surface_id = which(levels(match_data[,'surface']) == surface_name)
  
  
  # -- Get tournament intercept from stan results ----
  # -- First extract ALL tournament parameters
  tournament_est <- rstan::extract(stan_results)$t
  # -- Then, select the parameter corresponding to the tournament of interest!
  tournament_intercept <- tournament_est[,tournament_id]
  
  # --> Get player serve skill posteriors -----
  serve_est <- rstan::extract(stan_results)$prediction_s
  p1_serve_skill = serve_est[,player1_id]
  p2_serve_skill = serve_est[,player2_id]
  
  # --> Get player return skill posteriors -----
  return_est <- rstan::extract(stan_results)$prediction_r
  p1_return_skill = return_est[,player1_id]
  p2_return_skill = return_est[,player2_id]
  
  
  # --> Get player surface skills -----
  surface_est <- rstan::extract(stan_results)$surf
  p1_surface_skill = surface_est[,surface_id, player1_id]
  p2_surface_skill =  surface_est[,surface_id, player2_id]
  
  # --> Get global intercept -----
  posterior_intercept = rstan::extract(stan_results)$intercept
  
  
  # --> Obtain serve win posteriors
  # -- Note plogis() converts from logit to probability scale
  # i.e.: logit <- p1_serve_skill - p2_return_skill + p1_surface_skill - 
  #                p2_surface_skill + tournament_intercept + posterior_intercept
  #       prob = exp(logit) / (1 + exp(logit))

  p1_spw = plogis(p1_serve_skill - p2_return_skill
                  + p1_surface_skill
                  - p2_surface_skill
                  + tournament_intercept
                  + posterior_intercept)
  
  p2_spw = plogis(p2_serve_skill - p1_return_skill
                  + p2_surface_skill
                  - p1_surface_skill
                  + tournament_intercept
                  + posterior_intercept)
  
  dat <- cbind(p1_spw, p2_spw)
  colnames(dat) <- c("p1_spw", "p2_spw")
  
  # -- Convert data to wide format (easier for plotting)
  serve_win_df_wide <- reshape2::melt(dat) %>%
    mutate(Player = ifelse(Var2 == "p1_spw", player_1_name, player_2_name)) %>%
    select(-Var2) %>%
    rename(index = Var1,
           p_spw = value)
  serve_win_df_wide$Player <- as.factor(serve_win_df_wide$Player)
  
  return(serve_win_df_wide)
}


