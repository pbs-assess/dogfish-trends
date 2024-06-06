library(dplyr)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())

source("analysis/999-prep-overall-trawl.R")
gridll <- readRDS("output/PredictionGridCentres_IPHCcoast_regarea.rds")
gridll$depth_m <- gridll$bot_depth

glimpse(gridll)

g <- ggplot(grid, aes(UTM.lon, UTM.lat)) +
  geom_tile(colour = "grey70", fill = "white") +
  coord_fixed()
ggsave("figs/grids.png", width = 5, height = 5)
