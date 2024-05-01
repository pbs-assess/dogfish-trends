library(sdmTMB)
library(dplyr)
library(ggplot2)
library(sf)
source("analysis/999-prep-overall-trawl.R")
source("analysis/999-rotate.R")
grid <- mutate(grid, X = UTM.lon, Y = UTM.lat)

sf::sf_use_s2(FALSE)
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
coast <- sf::st_crop(
  map_data,
  c(xmin = -175, ymin = 20, xmax = -115, ymax = 70)
)
coast_proj <- sf::st_transform(coast, crs = 32612)

# make them in the right format
coast_proj4 <-
  st_cast(
    coast_proj,
    "POLYGON"
  )
# View(st_geometry(coast_proj4))
# plot(st_geometry(coast_proj4))
# plot(st_geometry(coast_proj4)[[1]])

# final2 <- st_sf(st_sfc())
# for (i in 1:dim(coast_proj4)[1]) {
#   rotate_coast3 <- splitrotatepolygon(
#     coast_proj4,
#     30, # for coast
#     mean(prediction_region$data$UTM.lon)*1000, # FIXME
#     mean(prediction_region$data$UTM.lat)*1000 # FIXME
#   )
#   final2 <- rbind(final2, rotate_coast3)
# }


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
