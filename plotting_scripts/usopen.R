# -- Produce your plots
library(dplyr)
library(rstan)
library(ggplot2)
library(patchwork)
source('src/summarise_posterior_data.R')
source('src/winning_prob.R')

atp_stan_results <- readRDS("./model/advi_atp_model.RDS")
atp_data <- readRDS(file = "./data/atp_data.rds")

#levels(atp_data$server) %>% sort()
#levels(atp_data$tournament)

p1_first <- 'Novak'
p1_last <- 'Djokovic'
player1 <- paste(p1_first, p1_last)

p2_first <- 'Daniil'
p2_last <- 'Medvedev'
player2 <- paste(p2_first, p2_last)

my_tournament <- 'us open'
my_surface <- 'Hard'

serve_win_df <- get_serve_win_posterior_dataframe(match_data = atp_data, 
                                                  stan_results = atp_stan_results, 
                                                  player_1_name = player1,
                                                  player_2_name = player2,
                                                  tournament_name = my_tournament,
                                                  surface_name = my_surface)

plot_background_col <- '#F5F5DC' # "#DBF5F0" is teal
panel_background_col <- '#F5F5DC'
# -- Predict Match -----
is_best_of_5 <- TRUE
match_win_probs <- prob_win_match_a(serve_win_df %>% 
                                      filter(Player == player1) %>%
                                      pull(p_spw), 
                                    serve_win_df %>% 
                                      filter(Player == player2) %>%
                                      pull(p_spw),
                                    best_of_five =is_best_of_5)


# -- Plot Most Likely Set Scores ----
most_likely_set_scores_df <- get_most_likely_set_scores(serve_win_df = serve_win_df,
                                                        player1 = player1,
                                                        player2 = player2)



most_likely_set_scores_df$score <- as.character(most_likely_set_scores_df$score)

remain_prob <-
  most_likely_set_scores_df %>%
  slice(10:nrow(most_likely_set_scores_df)) %>%
  #filter(prob < 0.05) %>%
  pull(prob) %>% sum()

most_likely_set_scores_df_plot <- most_likely_set_scores_df %>%
  slice(1:9) 
#filter(prob > 0.05)


## -- Code broke; this bit somehow works
## -- Idea: Re-configure the factor levels in between of the data processing steps
ordered_levels <- most_likely_set_scores_df_plot$score[order(most_likely_set_scores_df_plot$prob)]

most_likely_set_scores_df_plot[nrow(most_likely_set_scores_df_plot)+1, ] <- list(0,0,remain_prob, 'Other') 

most_likely_set_scores_df_plot$score <- factor(most_likely_set_scores_df_plot$score,
                                               levels = c('Other',ordered_levels))


# -- Most likely Match line scores -----
most_likely_match_scores_df <- get_most_likely_match_scores(serve_win_df = serve_win_df,
                                                            player1 = player1,
                                                            player2 = player2,
                                                            best_of_five = is_best_of_5 
                                                            )
most_likely_match_scores_df
most_likely_match_scores_df$match_label <- c('Djokovic in 3',
                                             'Djokovic in 4',
                                             'Djokovic in 5',
                                             'Other in 5',
                                             'Other in 4',
                                             'Other in 3')
most_likely_match_scores_df <- most_likely_match_scores_df %>%
  mutate(match_label= forcats::fct_reorder(match_label, prob)) 



# -- Generate your plots here ----

# -- Plot Prob(Win Point on Serve)
win_serve_plot <- 
  ggplot(serve_win_df, 
         aes(x = p_spw*100, fill =Player)) +
  scale_fill_manual(values = c('#ffce42', '#02075d'), labels = c('Other', 'Djokovic') ) + 
  geom_density(aes(y=..density..), alpha = 0.8)  +
  ggtitle("Winning a Service point") + 
  #xlab("Prob(Win Serve Point)") + ylab("Posterior Density") +
  xlab("Probability") + ylab("") +
  theme_bw() +
  theme(panel.background = element_rect(fill = panel_background_col, # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1),
                                face = "bold",
                                hjust = 0.5,
                                family = 'Tahoma'),
        legend.position = c(0.16, 0.86),
        #legend.position = 'top',
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.2, "cm"),
        legend.background = element_rect(fill = "#F5F5DC"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 11, family = 'Tahoma'),
        axis.text.x = element_text(colour = "black", 
                                   face = "bold",
                                   size = 10,
                                   family = 'Tahoma'),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = plot_background_col))

win_serve_plot
# -- Plot Prob(Player 1 Beats Player 2) ----
#match_win_title <- paste0('Prob. ', p1_last, ' Wins Match')

label_win <- data.frame(
  x = mean( match_win_probs*100) - 2.5, 
  y =  0.01,
  label =  paste(round(mean( match_win_probs*100), 1), '%')
)

match_win_title <- 'Djokovic Winning Match'
match_win_plot <- 
  data_frame(val = match_win_probs*100) %>%
  ggplot(., aes(val)) + 
  geom_density(alpha = 0.8, fill = "#02075d")  +
  ggtitle(match_win_title)  +
  xlab('Probability') + ylab('') +
  geom_vline(xintercept = mean( match_win_probs*100),
             colour="black", linetype = "longdash"
  ) +
  geom_label(data = label_win, 
             aes(x = x, y = y,label = label),
             fontface =2,
             fill=plot_background_col) +
  theme_bw() +
  theme(panel.background = element_rect(fill = panel_background_col, # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1),
                                face = "bold", 
                                hjust = 0.5,
                                family = 'Tahoma'),
        legend.position = "right",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.text.x = element_text(colour = "black", 
                                   face = "bold",
                                   size = 10,
                                   family = 'Tahoma'),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(face = "bold", 
                                  size = 11,
                                  family = 'Tahoma'),
        plot.background = element_rect(fill = plot_background_col))

match_win_plot

set_scores_plot <- 
  most_likely_set_scores_df_plot %>%
  #filter(prob > 0.015) %>%
  ggplot(aes(x = score, y = 100*prob)) +
  geom_bar(stat = "identity", color="white", fill = "#02075d", width = 0.8) +
  #geom_text(aes(x = score, y = 100*prob + 0.9,
  #              label = paste(100*round(prob,3), "%", sep = ""))) +
  geom_label(aes(x = score, y = 100*prob + 0.9,
                 label = paste(100*round(prob,2), "%", sep = "")),
             fill=plot_background_col, 
             fontface = "bold",
             size=2.5,
             label.size = 0.1, 
             nudge_y = -1.5,
             nudge_x = 0) +
  coord_flip() + 
  xlab("") +
  theme_bw() + 
  ylab("Probability") + 
  ggtitle("Predicted Set Scores") + 
  theme(panel.background = element_rect(fill = panel_background_col, # background colour
                                        #light green: ##DBF5E8
                                        # light yellow: #F8FCCB
                                        colour = "black", # border colour
                                        size = 0.8, linetype = "solid"),
        plot.title=element_text(size = rel(1),
                                face = "bold", 
                                hjust = 0.5,
                                family = 'Tahoma'),
        axis.text.y = element_text(face="bold", 
                                   color="black", 
                                   size=10,
                                   family = 'Tahoma'),
        axis.line = element_line(colour = "black", 
                                 size = 0.8, linetype = "solid"),
        axis.title.y=element_blank(),
        axis.title.x = element_text(colour = "black",
                                    face = "bold",
                                    size = 11,
                                    family = 'Tahoma'),
        axis.text.x = element_text(colour = "black",
                                   face = "bold",
                                   size = 10,
                                   family = 'Tahoma'),
        plot.background = element_rect(fill = plot_background_col))

set_scores_plot

match_scores_plot <- 
  most_likely_match_scores_df %>%
  ggplot(aes(x = match_label, y = 100*prob)) +
  geom_bar(stat = "identity", color="white", fill = "#02075d", width = 0.8)  +
  geom_label(aes(x = match_label, y = 100*prob + 0.9,
                 label = paste(100*round(prob,2), "%", sep = "")),
             fill=plot_background_col, 
             fontface = "bold",
             size=3,
             label.size = 0.2, 
             nudge_y = -3.5,
             nudge_x = 0) +
  coord_flip() + 
  xlab("") +
  theme_bw() + 
  ylab("Probability") + 
  ggtitle("Predicted Match Result") + 
  theme(panel.background = element_rect(fill = panel_background_col, # background colour
                                        #light green: ##DBF5E8
                                        # light yellow: #F8FCCB
                                        colour = "black", # border colour
                                        size = 0.8, linetype = "solid"),
        plot.title=element_text(size = rel(1),
                                face = "bold", hjust = 0.5,
                                family = 'Tahoma'),
        axis.text.y = element_text(face="bold",
                                   color="black", 
                                   size=10,
                                   family = 'Tahoma'),
        axis.line = element_line(colour = "black", 
                                 size = 0.8, linetype = "solid"),
        axis.title.y=element_blank(),
        axis.title.x = element_text(colour = "black", 
                                    face = "bold",
                                    size = 11,
                                    family = 'Tahoma'),
        axis.text.x = element_text(colour = "black", 
                                   face = "bold",
                                   size = 10,
                                   family = 'Tahoma'),
        plot.background = element_rect(fill = plot_background_col))

match_scores_plot

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

tournament_logo <- get_png("./img/usopen.png")

# --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Make Final Plot ----
# --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
#my_title = paste(p1_last ,'vs.', p2_last, sep = ' ')
my_title = 'Djokovic Chasing a Calander Grand Slam*'

( (match_scores_plot + match_win_plot) / (set_scores_plot + win_serve_plot)) + 
  inset_element(p = tournament_logo,
                left = 0, right = 0.25, bottom = 1.85, top = 2.25, align_to = 'full') +
  theme_void()+
  #left = 1.5, right = 0.25,
  inset_element(p = tournament_logo,
                left = 0.75, right = 1, bottom = 1.85, top = 2.25, align_to = 'full') +
  theme_void() +
  plot_annotation(title = my_title,
                  subtitle = '*First time since Rod Laver in 1969 (ATP)\n US Open Championship Match 2021',
                  caption = 'Model: @xenophar; Data: @tennisabstract',
                  theme = theme( plot.title=element_text(size = rel(1.7),
                                                         face = "bold", 
                                                         hjust = 0.5,
                                                         family = 'Tahoma'),
                                 plot.subtitle = element_text(size = rel(1),
                                                              hjust = 0.5,
                                                              family = 'Tahoma',
                                                              face = 'italic'),
                                 plot.caption = element_text(face = "italic",
                                                             family = 'Tahoma'),
                                 
                                 plot.background = element_rect(fill = plot_background_col)) 
  )

ggsave('./plots/usopen_djokovic.jpg',
       width=7, height=7,
       dpi = 300)

