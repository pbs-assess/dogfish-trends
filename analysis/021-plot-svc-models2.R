library(sdmTMB)
library(dplyr)
library(ggplot2)
library(sf)
source("analysis/999-prep-overall-trawl.R")
source("analysis/999-rotate.R")
# devtools::install_github("seananderson/ggsidekick")
grid <- mutate(grid, X = UTM.lon, Y = UTM.lat)
pred_year = 2003

# absolute decline map

# coastwide absolute declines map -----------------------------------------
grid <- grid |>
  dplyr::select(-c(year, year_scaled)) |>
  distinct(.keep_all = TRUE)
grid$FID <- seq(1, nrow(grid), 1)
years <- c(2003, 2023) #<- we assume it's linear so just assume linear change between the years
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
unique(grid$year)

dat_coast <- filter(dat, survey_name %in%
  c("syn bc", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "GOA")) |>
  filter(year %in% c(2003, 2023))
year_scaled <- unique(dat_coast$year_scaled)

grid <- grid |> mutate(year_scaled = ifelse(year == 2003, year_scaled[1], year_scaled[2]))

fit <- readRDS("output/fit-trawl-svc-lognormal-mix.rds") #<- bring in svc model
p <- predict(fit, newdata = grid)
p$combined_intercept <- p$est_non_rf1 + p$omega_s1 + p$est_non_rf2 + p$omega_s2

p <- p |>
  mutate(expest = exp(p$est1 + p$est2)) |>
  group_by(FID) |>
  mutate(x = expest[which(year == 2003)], y = expest[which(year == 2023)]) |>
  mutate(est_diff = y - x ) |>
  filter(year == pred_year)

ggplot(p, aes(UTM.lon, UTM.lat, colour = (est_diff))) +
  geom_tile() +
  colorspace::scale_colour_continuous_divergingx(
    palette = "RdBu", mid = 0)

ggplot(p, aes(UTM.lon, UTM.lat, colour = exp(combined_intercept))) +
  geom_tile() +
  scale_colour_viridis_c(trans = "log10")


# coastwide maturity classes absolute declines map -----------------------------------------

fits <- readRDS("output/fit-trawl-svc-maturity.rds")
lapply(fits, \(x) x$family)

grab_svc_pred_LD <- function(fit) {
  cat("-")
  p <- predict(fit, newdata = filter(grid, year %in% c(2003, 2023)))
  if (sdmTMB:::is_delta(fit)) {
    p <- p |> mutate(expest = exp(est1 + est2)) |>
      group_by(FID) |>
      mutate(y = expest[which(year == 2023)],  x = expest[which(year == 2003)]) |>
      mutate(est_diff = y-x) |>
      filter(year == 2003)

    p$combined_intercept <- p$est_non_rf1 + p$omega_s1 + p$est_non_rf2 + p$omega_s2

  } else {
    p <- p |>
      mutate(expest = exp(est)) |>
      mutate(y = expest[which(year == 2023)],  x = expest[which(year == 2003)]) |>
      mutate(est_diff = y-x) |>
      filter(year == 2003)

    p$combined_intercept <- p$est_non_rf1 + p$omega_s1 + p$est_non_rf2 + p$omega_s2
  }
  p
}

preds <- lapply(fits, grab_svc_pred_LD)
p <- c(list(p), preds) # add in the coast pred data
names(p) <- c("Combined", names(preds))


# rotate and shift coastline: ----------------------------------------

apply_rotation_df <- function(dd) {
  dd$X <- NULL
  dd$Y <- NULL
  dd <- sdmTMB::add_utm_columns(dd,
                                ll_names = c("longitude", "latitude"), utm_crs = 32612, units = "m"
  )
  rotate_coords(
    dd$X, dd$Y, 25,
    c(
      mean(dd$X),
      mean(dd$Y)
    )
  ) |>
    rename(rotated_x = x, rotated_y = y) |>
    bind_cols(dd)
}

pr <- lapply(p, apply_rotation_df)
# NOTE: the ordering happens here! not in factor levels
# right to left
pr2 <- c(
  "Combined" = list(pr$Combined),
  "mf" = list(pr$mf),
  "mm" = list(pr$mm),
  "maturingf" = list(pr$maturingf),
  "maturingm" = list(pr$maturingm),
  "imm" = list(pr$imm)
)
prsl <- lapply(seq_along(pr), \(i) {
  d <- pr2[[i]]
  d$rotated_x <- d$rotated_x - 750000 * (i - 1)
  d
})
names(prsl) <- names(pr2)

source("analysis/999-colours-etc.R")
prs <- bind_rows(prsl, .id = "group")
prs <- add_maturity_group_clean_column(prs)
lvls <- c("Combined", rev(levels(prs$group_clean)))

prs$group_clean <- as.character(prs$group_clean)
prs$group_clean <- if_else(is.na(prs$group_clean), "Combined", prs$group_clean)

lvls <- gsub("Mature ", "Mature\n", lvls)
lvls <- gsub("Maturing ", "Maturing\n", lvls)
prs$group_clean <- gsub("Mature ", "Mature\n", prs$group_clean)
prs$group_clean <- gsub("Maturing ", "Maturing\n", prs$group_clean)

prs$group_clean <- factor(prs$group_clean, levels = lvls)
pal <- rev(RColorBrewer::brewer.pal(3, name = "RdBu"))
mids <- group_by(prs, group_clean) |> summarise(mean_x = mean(rotated_x))

# saveRDS(prs, "output/svc-spatial-data.rds")

# rotate sf coast ---------------------------------

library(sf)
sf::sf_use_s2(FALSE)
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
coast <- sf::st_crop(
  map_data,
  c(xmin = -175, ymin = 25, xmax = -95, ymax = 70)
)
plot(st_geometry(coast))
coast_proj <- sf::st_transform(coast, crs = 32612)
# make them in the right format
coast_proj4 <-
  st_cast(
    coast_proj,
    "POLYGON"
  )
# plot(st_geometry(coast_proj4))

rotated_coast <- list()
for (i in seq_len(dim(coast_proj4)[1])) {
  rotated_coast[[i]] <- splitrotatepolygon(
    coast_proj4,
    25,
    mean(pr[[1]]$X),
    mean(pr[[1]]$Y)
  )
}
rotated_coast <- do.call(rbind, rotated_coast)


# main plot ---------------------------------------------------------------

LIMS <- c(-12, 4) #<- set up limits to contract colour scheme and make visualizations better
LAB <- "Est. biomass change\n('05-'23)"

#scale ok?
ggplot(prs, aes((est_diff))) + geom_histogram() + facet_wrap(~group_clean) + scale_x_log10()

library(ggsidekick)
g1 <- prs |>
  #filter(year == 2023) |>
  #mutate(svc = ifelse(exp(svc) >= LIMS[2], log(LIMS[2] - 1e-6), svc)) |>
  #mutate(svc = ifelse(exp(svc) <= LIMS[1], log(LIMS[1] + 1e-6), svc)) |>
  ggplot(aes(rotated_x, rotated_y, fill = (est_diff ), colour = (est_diff ))) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.5),
    legend.title = element_text(colour = "grey30", size = 10),
    plot.background = element_rect(fill = "white", linewidth = 0)
  ) +
  geom_sf(data = rotated_coast, inherit.aes = FALSE, fill = "grey70", colour = "grey40") +
  geom_tile(width = 3000, height = 3000) +
  colorspace::scale_colour_continuous_divergingx(
    palette = "RdBu", mid = 0,
    trans = "fourth_root_power",
    limits = LIMS
  ) +
  colorspace::scale_fill_continuous_divergingx(
    palette = "RdBu", mid = 0,
    trans = "fourth_root_power",
    limits = LIMS
  ) +
  labs(fill = LAB, colour = LAB) +
  coord_sf(
    xlim = c(min(prs$rotated_x), 1546730 - 1800000),
    ylim = range(prs$rotated_y) + c(0, 100000)
  ) +
  geom_text(
    data = mids, mapping = aes(x = mean_x - 1000000, label = group_clean),
    y = 8500000, inherit.aes = FALSE, colour = "grey30", vjust = 1, hjust = 0
  )
g1

LABS2 <- "Relative\nbiomass\ndensity"

range01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

prs3 <- prs |>
  group_by(group_clean) |>
  # mutate(combined_intercept = combined_intercept - mean(combined_intercept)) |>
  mutate(LWR = quantile(combined_intercept, probs = c(0.005))) |>
  mutate(UPR = quantile(combined_intercept, probs = c(0.995))) |>
  mutate(combined_intercept = ifelse(combined_intercept >= UPR, UPR - 1e-6, combined_intercept)) |>
  mutate(combined_intercept = ifelse(combined_intercept <= LWR, LWR + 1e-6, combined_intercept)) |>
  mutate(combined_intercept = range01(exp(combined_intercept))) |>
  ungroup()
# LIMS_INT <- quantile(prs3$combined_intercept, probs = c(0.005, 0.995))

g2 <- prs3 |>
  ggplot(aes(rotated_x, rotated_y, fill = combined_intercept, colour = combined_intercept)) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.5),
    legend.title = element_text(colour = "grey30", size = 10),
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  geom_sf(data = rotated_coast, inherit.aes = FALSE, fill = "grey70", colour = "grey40") +
  geom_tile(width = 3000, height = 3000) +
  scale_colour_viridis_c(option = "D", trans = "sqrt") +
  scale_fill_viridis_c(option = "D", trans = "sqrt") +
  labs(fill = LABS2, colour = LABS2) +
  coord_sf(
    xlim = c(min(prs$rotated_x), 1546730 - 1800000),
    ylim = range(prs$rotated_y) + c(0, 100000)
  ) +
  geom_text(
    data = mids, mapping = aes(x = mean_x - 1000000, label = group_clean),
    y = 8500000, inherit.aes = FALSE, colour = "grey30", vjust = 1, hjust = 0
  )
g2
g <- cowplot::plot_grid(g2, g1, nrow = 2, align = "v", labels = c("(a)", "(b)"), label_fontface = "plain", label_colour = "grey30", label_size = 12)

ggsave("figs/svc-trawl-stacked-abs.pdf", width = 6.1, height = 10)
ggsave("figs/svc-trawl-stacked-abs.png", width = 6.1, height = 10)


# plot with location names for SOM ----------------------------------------
p <- prs |> filter(group == "Combined")
p <- p |> dplyr::select(-c("UTM.lon", "UTM.lat", "X", "Y"))
p2 <- sdmTMB::add_utm_columns(p,
                              ll_names = c("longitude", "latitude"), utm_crs = 32612, units = "m"
)

map_data <- rnaturalearth::ne_states(returnclass = "sf")
coast <- sf::st_crop(
  map_data,
  c(xmin = -180, ymin = 25, xmax = -115, ymax = 70)
)
plot(st_geometry(coast))

coast_proj <- sf::st_transform(coast, crs = 32612)

p2 |>
  ggplot(aes(X, Y, fill = (est_diff ), colour = (est_diff ))) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.5),
    legend.title = element_text(colour = "grey30", size = 10),
    plot.background = element_rect(fill = "white", linewidth = 0)
  ) +
  geom_sf(data = coast_proj, inherit.aes = FALSE, fill = "grey70", colour = "grey40") +
  geom_tile(width = 3000, height = 3000) +
  colorspace::scale_colour_continuous_divergingx(
    palette = "RdBu", mid = 0,
    trans = "fourth_root_power",
    limits = LIMS
  ) +
  colorspace::scale_fill_continuous_divergingx(
    palette = "RdBu", mid = 0,
    trans = "fourth_root_power",
    limits = LIMS
  ) +
  labs(fill = LAB, colour = LAB)

ggsave("figs/location-map.png", width = 7, height = 5.5)

