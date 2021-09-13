# -- Save Match Data

# --> Load libraries
library(dplyr)
source("~/tennis_analytics/projects/point_IID/src/collect_data.R")

atp_data <- collect_relevant_data(2018:2021, atp= TRUE)
wta_data <- collect_relevant_data(2018:2021, atp= FALSE)
table(wta_data$year)

# -- Need to do extensive checks on how tournaments (and other variables!) are spelled
atp_data$tournament <- lapply(atp_data$tournament, as.character)
atp_data$tournament <- trimws(x = tolower(atp_data$tournament), which = 'both')
atp_data$tournament <- as.factor(atp_data$tournament)
#levels(atp_data$tournament)

wta_data$tournament <- lapply(wta_data$tournament, as.character)
wta_data$tournament <- trimws(x = tolower(wta_data$tournament), which = 'both')
wta_data$tournament <- as.factor(wta_data$tournament)
#levels(wta_data$tournament)
levels(wta_data$server) %>% sort()

# Save an object to a file
saveRDS(atp_data, file = "~/tennis_analytics/projects/point_IID/data/atp_data.rds")
saveRDS(wta_data, file = "~/tennis_analytics/projects/point_IID/data/wta_data.rds")
# Restore the object
#readRDS(file = "~/tennis_analytics/projects/point_IID/data/wta_data.rds")
