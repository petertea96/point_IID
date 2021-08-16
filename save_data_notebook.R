# -- Save Match Data

# --> Load libraries
library(dplyr)
source("~/tennis_analytics/projects/point_IID/src/collect_data.R")

atp_data <- collect_relevant_data(2017:2021, atp= TRUE)
wta_data <- collect_relevant_data(2017:2021, atp= FALSE)
table(wta_data$year)

# Save an object to a file
saveRDS(atp_data, file = "~/tennis_analytics/projects/point_IID/data/atp_data.rds")
saveRDS(wta_data, file = "~/tennis_analytics/projects/point_IID/data/wta_data.rds")
# Restore the object
readRDS(file = "~/tennis_analytics/projects/point_IID/data/wta_data.rds")
