# -- Barty vs Teichman
# -- Produce your plots
library(dplyr)
library(rstan)
library(ggplot2)
library(patchwork)
source('src/summarise_posterior_data.R')
source('src/winning_prob.R')

wta_stan_results <- readRDS("./model/advi_wta_model.RDS")
wta_data <- readRDS(file = "./data/wta_data.rds")

#levels(wta_data$server)
#levels(wta_data$tournament)

p1_first <- 'Ashleigh'
p1_last <- 'Barty'
player1 <- paste(p1_first, p1_last)
p2_first <- 'Jil Belen'
p2_last <- 'Teichmann'

player2 <- paste(p2_first, p2_last)
serve_win_df <- get_serve_win_posterior_dataframe(match_data = wta_data, 
                                                  stan_results = wta_stan_results, 
                                                  player_1_name = player1,
                                                  player_2_name = player2,
                                                  tournament_name = "Cincinnati",
                                                  surface_name = "Hard")

# -- Predict Match -----
match_win_probs <- prob_win_match_a(serve_win_df %>% 
                                      filter(Player == player1) %>%
                                      pull(p_spw), 
                                    serve_win_df %>% 
                                      filter(Player == player2) %>%
                                      pull(p_spw),
                                    best_of_five = FALSE)


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
                                                            player2 = player2)
most_likely_match_scores_df

# -- Update these labels manually -----
most_likely_match_scores_df$match_label <- c('Barty in 2',
                                             'Barty in 3',
                                             'Teichmann in 2',
                                             'Teichmann in 3')
most_likely_match_scores_df <- most_likely_match_scores_df %>%
  mutate(match_label= forcats::fct_reorder(match_label, prob)) 



# -- Generate your plots here ----

# -- Plot Prob(Win Point on Serve)
win_serve_plot <- 
  ggplot(serve_win_df, 
         aes(x = p_spw*100, fill =Player)) +
  scale_fill_manual(values = c('#E67C7C', '#aaf0d1'),  labels = c('Barty', 'Teichmann')) + 
  geom_density(aes(y=..density..), alpha = 0.6)  +
  ggtitle("Prob. Winning a Serve Point") + 
  #xlab("Prob(Win Serve Point)") + ylab("Posterior Density") +
  xlab("Probability") + ylab("") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#F5F5DC", # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1),
                                face = "bold",
                                hjust = 0.5,
                                family = 'Tahoma'),
        legend.position = c(0.5, 0.86),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.125, "cm"),
        legend.background = element_rect(fill = "#F5F5DC"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 11),
        axis.text.x = element_text(colour = "black", 
                                   face = "bold",
                                   size = 10,
                                   family = 'Tahoma'),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#FFCCBC"))

# -- Plot Prob(Player 1 Beats Player 2) ----
match_win_title <- paste0('Prob. ', p1_last, ' Wins Match')
match_win_plot <- 
  data_frame(val = match_win_probs*100) %>%
  ggplot(., aes(val)) + 
  geom_density(alpha = 0.6, fill = "#E67C7C")  +
  geom_vline(xintercept = mean( match_win_probs*100),
             colour="black", linetype = "longdash"
  )+
  ggtitle(match_win_title)  +
  xlab('Probability') + ylab('') +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#F5F5DC", # background colour
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
        plot.background = element_rect(fill = "#FFCCBC"))

set_scores_plot <- 
  most_likely_set_scores_df_plot %>%
  filter(prob > 0.015) %>%
  ggplot(aes(x = score, y = 100*prob)) +
  geom_bar(stat = "identity", color="#9C7B60", fill = "#E67C7C", width = 0.8) +
  #geom_text(aes(x = score, y = 100*prob + 0.9,
  #              label = paste(100*round(prob,3), "%", sep = ""))) +
  geom_label(aes(x = score, y = 100*prob + 0.9,
                 label = paste(100*round(prob,2), "%", sep = "")),
             fill="#FFCCBC", 
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
  theme(panel.background = element_rect(fill = "#F5F5DC", # background colour
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
                                    size = 12,
                                    family = 'Tahoma'),
        axis.text.x = element_text(colour = "black",
                                   face = "bold",
                                   size = 10,
                                   family = 'Tahoma'),
        plot.background = element_rect(fill = "#FFCCBC"))

my_title = 'Barty vs. Teichmann'

match_scores_plot <- 
  most_likely_match_scores_df %>%
  ggplot(aes(x = match_label, y = 100*prob)) +
  geom_bar(stat = "identity", color="#9C7B60", fill = "#E67C7C", width = 0.8)  +
  geom_label(aes(x = match_label, y = 100*prob + 0.9,
                 label = paste(100*round(prob,2), "%", sep = "")),
             fill="#FFCCBC", 
             fontface = "bold",
             size=3.5,
             label.size = 0.2, 
             nudge_y = -3.5,
             nudge_x = 0) +
  coord_flip() + 
  xlab("") +
  theme_bw() + 
  ylab("Probability") + 
  ggtitle("Predicted Final Score Line") + 
  theme(panel.background = element_rect(fill = "#F5F5DC", # background colour
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
                                    size = 12,
                                    family = 'Tahoma'),
        axis.text.x = element_text(colour = "black", 
                                   face = "bold",
                                   size = 10,
                                   family = 'Tahoma'),
        plot.background = element_rect(fill = "#FFCCBC"))


get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

tournament_logo <- get_png("./img/cincinatti.png")

#tournament_logo <- png::readPNG("./img/cincinatti.png")



( (match_scores_plot + match_win_plot) / (set_scores_plot + win_serve_plot)) + 
  inset_element(p = tournament_logo,
                left = 0, right = 0.25, bottom = 2, top = 2.25, align_to = 'full') +
  theme_void()+
  inset_element(p = tournament_logo,
                left = 1.5, right = 0.25, bottom = 2, top = 2.25, align_to = 'full') +
  theme_void() +
  plot_annotation(title = my_title,
                  subtitle = 'WTA Championship Match Cincinnati 2021',
                  caption = 'Model: @xenophar\nData: @tennisabstract',
                  theme = theme( plot.title=element_text(size = rel(1.5),
                                                         face = "bold", 
                                                         hjust = 0.5,
                                                         family = 'Tahoma'),
                                 plot.subtitle = element_text(size = rel(1.25),
                                                              hjust = 0.5,
                                                              family = 'Tahoma'),
                                 plot.caption = element_text(face = "italic",
                                                             family = 'Tahoma'),
                                 
                                 plot.background = element_rect(fill = "#FFCCBC")) 
  )

ggsave('./plots/barty_teichmann.jpg',
      width=6.5, height=6,
      dpi = 300)

