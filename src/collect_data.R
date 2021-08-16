### Date: April 9th, 2020
### Author: Peter Tea
### Purpose: Scrape tennis data from Jeff Sackmann's repos

# We will look at the 2017, 2018 and 2019 tennis seasons' data

### Data cleaning and filtering steps

### For each tennis match, we extract the number of service points played and
### service points won for both players in a match. We also extract the name of 
### the tournament, date of play, and the court surface played on.

# --> Load libraries
library(dplyr)
library(lubridate)

##### ----- ##### ----- ##### ----- ###### ----- ##### ----- ##### ----- #####

scrape_tennis_data <- function(year, atp=TRUE){
  ### year: year we want to scrape
  ### atp: Do we want atp data (TRUE) or wta (FALSE)
  
  # --> Get url
  if(atp){
    url_dat <- paste("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_",
                     year,".csv", sep = "")
  } else {
    url_dat <- paste("https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_",
                     year,".csv", sep = "")
  }
  
  # --> read data
  dat <- read.csv(url_dat, stringsAsFactors = FALSE)
  
  # --> select important variables and filter to remove Davis Cup
  complete_dat <- dat %>%
    # --> Remove Davis Cup data (not a legit tournament IMO)
    dplyr::filter(!grepl(pattern = "Davis Cup", x = tourney_name)) %>%
    dplyr::filter(w_1stWon + w_2ndWon > 0) %>%
    dplyr::filter(l_1stWon + l_2ndWon > 0) %>%
    dplyr::filter(surface != "None" | surface != "none") %>%
    
    dplyr::select(l_1stWon, l_2ndWon, l_svpt, loser_name, w_1stWon, w_2ndWon, 
                  w_svpt,winner_name, tourney_date, surface, tourney_name)
  
  complete_dat$year <- year
  return( complete_dat[complete.cases(complete_dat),] ) 
}

##### ----- ##### ----- ##### ----- ###### ----- ##### ----- ##### ----- #####

get_periods <- function(tourney_date, month_split = 4){
  # month_split: how many months should be in a period?
  library(lubridate)
  
  # --> Set period length to 2
  period_temp = as.Date(as.character(tourney_date), format="%Y%m%d")
  
  # Calculates the difference in months between two dates
  start_date = min(period_temp)
  temp = (year(period_temp) - year(start_date))*12 + month(period_temp) - month(start_date) 
  
  # --> aggregate 0 - 2 months as p =1; 3 - 5 as p = 2; etc
  return((temp %/% month_split) + 1)
}


collect_relevant_data <- function(years_of_interest, atp, month_split=4, filter_step = FALSE){
  # --> Initialize list
  datalist = list()
  
  for(index in 1:length(years_of_interest)){
    datalist[[index]] <- scrape_tennis_data(year = years_of_interest[index], 
                                            atp = atp)
  }
  
  #--> Same thing as rbind(data1, data2, etc) of the list
  big_data <- dplyr::bind_rows(datalist) 
  #dim(big_data); dim(big_data[complete.cases(big_data),])
  
  # --> Shape into dataset where each row is a match's service game
  # --> Note: There will be 2 rows for each match 
  
  
  
  ##### ----- ##### ----- ##### ----- ###### ----- #####
  # --> Collect serve points played, serve points won, tournament, surface
  ##### ----- ##### ----- ##### ----- ###### ----- #####
  #--> Losers first...
  loser_spw_won <- big_data$l_1stWon + big_data$l_2ndWon
  loser_sp_total <- big_data$l_svpt
  losers <- big_data$loser_name
  #losers_id = big_data$loser_id
  
  #--> Winners now
  winner_spw_won <- big_data$w_1stWon + big_data$w_2ndWon
  winner_sp_total <- big_data$w_svpt
  winners <- big_data$winner_name
  #winners_id = big_data$winner_id
  
  #--> Get surface and tournament IDs
  surface <- big_data$surface
  tournament <- big_data$tourney_name
  
  #--> get periods
  period <- get_periods(big_data$tourney_date, month_split)
  year = big_data$year
  
  ##### ----- ##### ----- ##### ----- ###### ----- #####
  # --> Combine into one dataframe:
  ##### ----- ##### ----- ##### ----- ###### ----- #####
  winner_frame = data.frame(server = winners, returner = losers, 
                            sp_won = winner_spw_won, sp_total = winner_sp_total,
                            period = period, surface = surface, tournament = tournament, 
                            year = year,
                            stringsAsFactors = TRUE)
  
  loser_frame = data.frame(server = losers, returner = winners, 
                           sp_won = loser_spw_won, sp_total = loser_sp_total,
                           period = period, surface = surface, tournament = tournament,
                           year = year,
                           stringsAsFactors = TRUE)
  
  relevant_data = rbind(winner_frame, loser_frame)
  
  
  
  # Should we remove players who did not play many games?
  # if(filter_step){
  #   
  #   winner_frame = data.frame(server = winners, returner = losers, 
  #                             sp_won = winner_spw_won, sp_total = winner_sp_total,
  #                             period = period, surface = surface, tournament = tournament, 
  #                             stringsAsFactors = FALSE)
  #   
  #   loser_frame = data.frame(server = losers, returner = winners, 
  #                            sp_won = loser_spw_won, sp_total = loser_sp_total,
  #                            period = period, surface = surface, tournament = tournament,
  #                            stringsAsFactors = FALSE)
  #   
  #   relevant_data = rbind(winner_frame, loser_frame)
  #   
  #   relevant_data = relevant_data[complete.cases(relevant_data),]
  #   
  #   
  #   serve_dat_to_keep <- relevant_data %>%
  #     group_by(server) %>%
  #     summarise(n=n()) %>%
  #     filter(n >= 5)
  #   
  #   relevant_data <- relevant_data %>%
  #     dplyr::filter( (server %in% serve_dat_to_keep$server)) %>%
  #     dplyr::filter( (returner %in% serve_dat_to_keep$server)) 
  #   
  #   
  #   # Convert character columns into factor
  #   relevant_data[sapply(relevant_data, is.character)] <- lapply(relevant_data[sapply(relevant_data,
  #                                                                                     is.character)], as.factor)
  #   
  # }
  
  
  return(relevant_data)
}



# -- Function to format data to fit a STAN Model
format_stan_data <- function(relevant_data){
  
  ##################################################################
  # --> Extract data components to feed into stan model:
  ##################################################################
  surface_ids = as.numeric(as.factor(relevant_data[,'surface']))
  
  #--> Sanity check (ensure we get 1,2,3)
  #sort(unique(surface_ids))
  
  tournament_ids = as.numeric(as.factor(relevant_data[,'tournament']))
  #sort(unique(tournament_ids))
  
  players <- as.numeric(as.factor(unlist(list(relevant_data[, "server"], relevant_data[,'returner']))))
  #sort(unique(players))
  
  
  middle_num <- length(players)/2
  server_ids = players[1:middle_num]
  returner_ids = players[(middle_num+1):length(players)]
  
  spw = relevant_data[,'sp_won']
  spt = relevant_data[,'sp_total']
  
  period = relevant_data[,'period']
  
  num_periods = length(unique(period))
  num_surfaces = length(levels(as.factor(relevant_data[, "surface"])))
  
  
  stan_data <- list(
    num_matches = nrow(relevant_data),
    num_players = length(unique(players)),
    s_id = server_ids,
    r_id = returner_ids,
    spw = spw,
    spt = spt,
    period = period,
    num_periods = num_periods,
    surface = surface_ids,
    num_surfaces = num_surfaces,
    tournament = tournament_ids,
    num_tournaments = length(unique(tournament_ids))
  )
  
  return(stan_data)
  
  
}



