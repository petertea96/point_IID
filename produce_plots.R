# -- Produce your plots
library(dplyr)
library(rstan)
library(ggplot2)
library(patchwork)
source('src/summarise_posterior_data.R')
source('src/winning_prob.R')


#atp_stan_results <- readRDS("./model/advi_atp_model.RDS")
atp_stan_results <- readRDS("./model/atp_model.RDS")
atp_data <- readRDS(file = "./data/atp_data.rds")


# -- Predict Prob(Win Point on Serve) -----
player1 <- "Daniil Medvedev"
player2 <- "Reilly Opelka"
serve_win_df <- get_serve_win_posterior_dataframe(match_data = atp_data, 
                                                  stan_results = atp_stan_results, 
                                                  player_1_name = player1,
                                                  player_2_name = player2,
                                                  tournament_name = "Canada Masters",
                                                  surface_name = "Hard")


# -- Plot Prob(Win Point on Serve)
win_serve_plot <- 
ggplot(serve_win_df, 
       aes(x = p_spw*100, fill =Player)) +
    geom_density(aes(y=..density..), alpha = 0.6)  +
    ggtitle("Prob(Win Serve Point)") + 
    #xlab("Prob(Win Serve Point)") + ylab("Posterior Density") +
    xlab("Probability") + ylab("") +
    theme_classic() +
    theme(panel.background = element_rect(fill = "#F5F5DC", # background colour
                                          colour = "black", # border colour
                                          size = 0.5, linetype = "solid"),
          plot.title=element_text(size = rel(1),
                                  face = "bold", hjust = 0.5),
          legend.position = c(0.51, 0.87),
          legend.title = element_blank(),
          legend.text = element_text(size = 7),
          legend.key.size = unit(0.125, "cm"),
          legend.background = element_rect(fill = "#F5F5DC"),
          legend.key = element_rect(fill = "gray90"),
          axis.title = element_text(face = "bold", size = 11),
          axis.text.x = element_text(colour = "black", face = "bold",
                                     size = 10),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "#DBF5F0")) 
  

# -- Predict Match -----
match_win_probs <- prob_win_match_a(serve_win_df %>% 
                                      filter(Player == player1) %>%
                                      pull(p_spw), 
                                    serve_win_df %>% 
                                      filter(Player == player2) %>%
                                      pull(p_spw),
                                    best_of_five = FALSE)


# -- Plot Prob(Medvedev Beats Opelka) ----
match_win_plot <- 
data_frame(val = match_win_probs*100) %>%
  ggplot(., aes(val)) + 
  geom_density(alpha = 0.6, fill = "#aaf0d1")  +
  ggtitle('Prob(Medvedev Beats Opelka)')  +
  #xlab("Prob(Medvedev Beats Opelka)") + ylab("Posterior Density") +
  xlab('Probability') + ylab('') +
  theme_bw() +
  #scale_x_continuous(breaks=c(0.2,0.4, 0.6, 0.8, 0.9, 1)) +
  theme(panel.background = element_rect(fill = "#F5F5DC", # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1),
                                face = "bold", hjust = 0.5),
        legend.position = "right",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.text.x = element_text(colour = "black", face = "bold",
                                   size = 10),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(face = "bold", size = 11),
        plot.background = element_rect(fill = "#DBF5F0"))


# -- Plot Most Likely Set Scores ----
most_likely_set_scores_df <- get_most_likely_set_scores(serve_win_df = serve_win_df)

set_scores_plot <- 
most_likely_set_scores_df %>%
  filter(prob > 0.01) %>%
  ggplot(aes(x = score, y = 100*prob)) +
  geom_bar(stat = "identity", color="#00a86b", fill = "#9ED9CCFF", width = 0.8) +
  #geom_text(aes(x = score, y = 100*prob + 0.9,
  #              label = paste(100*round(prob,3), "%", sep = ""))) +
  geom_label(aes(x = score, y = 100*prob + 0.9,
                 label = paste(100*round(prob,2), "%", sep = "")),
             fill="#DBF5F0", 
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
                                face = "bold", hjust = 0.5),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 0.8, linetype = "solid"),
        axis.title.y=element_blank(),
        axis.title.x = element_text(colour = "black", face = "bold",
                                    size = 12),
        axis.text.x = element_text(colour = "black", face = "bold",
                                    size = 10),
        plot.background = element_rect(fill = "#DBF5F0"))

# -- Most likely Match line scores -----
most_likely_match_scores_df <- get_most_likely_match_scores(serve_win_df = serve_win_df)

most_likely_match_scores_df$match_label <- c('Medvedev in 2',
                                             'Medvedev in 3',
                                             'Opelka in 2',
                                             'Opelka in 3')
most_likely_match_scores_df <- most_likely_match_scores_df %>%
mutate(match_label= forcats::fct_reorder(match_label, prob)) 

match_scores_plot <- 
most_likely_match_scores_df %>%
  ggplot(aes(x = match_label, y = 100*prob)) +
  geom_bar(stat = "identity", color="#00a86b", fill = "#9ED9CCFF", width = 0.8) +
  geom_label(aes(x = match_label, y = 100*prob + 0.9,
                 label = paste(100*round(prob,2), "%", sep = "")),
             fill="#DBF5F0", 
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
                                face = "bold", hjust = 0.5),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 0.8, linetype = "solid"),
        axis.title.y=element_blank(),
        axis.title.x = element_text(colour = "black", face = "bold",
                                    size = 12),
        axis.text.x = element_text(colour = "black", face = "bold",
                                   size = 10),
        plot.background = element_rect(fill = "#DBF5F0",
                                       colour = "#DBF5F0",size = 0))


my_title = emo::ji_glue('Medvedev (:russia:) Heavily Favoured against Opelka (:america:)')
my_title = 'Medvedev Heavily Favoured against Opelka'


( (match_scores_plot + match_win_plot) / (set_scores_plot + win_serve_plot)) + 
  plot_annotation(title = my_title,
                  subtitle = 'National Bank Open Masters 1000',
                  caption = 'Model: @xenophar\nData: @tennisabstract',
                  theme = theme( plot.title=element_text(size = rel(1.5),
                                                         face = "bold", hjust = 0.5),
                                 plot.subtitle = element_text(size = rel(1.25),
                                                              hjust = 0.5),
                                 plot.caption = element_text(face = "italic"),
                                 
                                 plot.background = element_rect(fill = "#DBF5F0"))
  )


ggsave('./plots/medvedev_opelka.jpg',
       width=6.5, height=6,
       dpi = 300)


# -- Serve Skills -----
serve_to_plot <- get_skill_to_plot(stan_results = atp_stan_results, 
                                   relevant_data = atp_data,
                                   skill = "serve")

plot_skill(serve_to_plot, skill = "serve", year = "(2017-2021)")

# --> Return skills -----
return_to_plot <- get_skill_to_plot(stan_results = atp_stan_results, 
                                    relevant_data = atp_data,
                                    skill = "return")
plot_skill(return_to_plot, skill = "return", year = "(2017-2021)")




