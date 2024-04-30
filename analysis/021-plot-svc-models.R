library(sdmTMB)
library(dplyr)
library(ggplot2)
source("analysis/999-prep-overall-trawl.R")
grid <- mutate(grid, X = UTM.lon, Y = UTM.lat)
fit <- readRDS("output/fit-trawl-svc-lognormal-mix.rds")
b1 <- tidy(fit)
b2 <- tidy(fit, model = 2)
z1 <- b1$estimate[b1$term == "year_scaled"]
z2 <- b2$estimate[b1$term == "year_scaled"]

# pick any year:
p <- predict(fit, newdata = filter(grid, year == max(grid$year)))
p$svc <- z1 + z2 + p$zeta_s_year_scaled1 + p$zeta_s_year_scaled2
p |>
  ggplot(aes(UTM.lon, UTM.lat, fill = exp(svc))) +
  geom_raster() +
  scale_fill_gradient2(trans = "log10") +
  labs(fill = "Spatially\nvarying\ntrend\n\n(Proportion change\nby decade)") +
  coord_equal()
ggsave("figs/svc-trawl.png", width = 6, height = 6)
