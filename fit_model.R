# -- Fit STAN Model
library(dplyr)
library(rstan)

source('/Users/petertea/tennis_analytics/projects/point_IID/src/collect_data.R')
atp_data <- readRDS(file = "/Users/petertea/tennis_analytics/projects/point_IID/data/atp_data.rds")


# -- Convert to STAN FORMAT -----
atp_stan_data <- format_stan_data(relevant_data = atp_data)

rstan_options(auto_write = FALSE)
options(mc.cores = 4)
#model <- stan_model('./model/stan_model.stan')
#atp_model_fit <- sampling(model, data=atp_stan_data)
#saveRDS(atp_model_fit, file = "./model/atp_model.RDS")

# -- Hacky way of fitting the STAN model ----
library(cmdstanr)
library(data.table)

stan.model <- cmdstan_model('/Users/petertea/tennis_analytics/projects/point_IID/model/stan_model.stan')

# -- ADVI version the fastest MCMC to fit
fit_atp_model <- stan.model$variational(
  data = atp_stan_data,
  tol_rel_obj = 0.001
)

# -- Convert object types to stanfit
atp.stanfit <- rstan::read_stan_csv(fit_atp_model$output_files())
saveRDS(atp.stanfit, file = "/Users/petertea/tennis_analytics/projects/point_IID/model/advi_atp_model.RDS")



# -- Repeat for WTA ----
wta_data <- readRDS(file = "/Users/petertea/tennis_analytics/projects/point_IID/data/wta_data.rds")
wta_stan_data <- format_stan_data(relevant_data = wta_data)

fit_wta_model <- stan.model$variational(
  data = wta_stan_data,
  tol_rel_obj = 0.001
)

wta.stanfit <- rstan::read_stan_csv(fit_wta_model$output_files())
saveRDS(wta.stanfit, file = "/Users/petertea/tennis_analytics/projects/point_IID/model/advi_wta_model.RDS")

