library(sdmTMB)
library(dplyr)
library(ggplot2)
library(sf)
source("analysis/999-prep-overall-trawl.R")
source("analysis/999-rotate.R")
grid <- mutate(grid, X = UTM.lon, Y = UTM.lat)

# #trying this to see if the map of absolute decline is more intuitive
# #needs cleaning and not included in manuscript for now, rough

# coastwide absolute declines map -----------------------------------------
grid <- grid |>
  dplyr::select(-c(year, year_scaled)) |>
  distinct(.keep_all = TRUE)
grid$FID <- seq(1, nrow(grid), 1)
#years <- seq(min(dat$year), max(dat$year), 1) #<- first and last years
years <- c(2005, 2023) #<- we assume it's linear so just assume linear change between the years
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))

dat_coast <- filter(dat, survey_name %in%
  c("syn bc", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "GOA")) |>
  filter(year >= 2005)

dat_coast |>
  dplyr::select(year, year_scaled) |>
  distinct(.keep_all = TRUE) #<- get the year scaled values that match the data

grid <- grid |> mutate(year_scaled = ifelse(year == 2005, -0.5, 1.3))

fit <- readRDS("output/fit-trawl-svc-lognormal-mix.rds") #<- bring in svc model
p <- predict(fit, newdata = grid)
glimpse(p)
#p$expest <- rowSums(p[c("est2", "est_non_rf2", "est_rf2", "est1", "est_non_rf1", "est_rf1")]) # sum the model 1 and 2 est including random and non random effects
p$expest <- rowSums(p[c("est1", "est2")]) # sum the model 1 and 2 est including random and non random effects
#p$expest <- p$est1 * p$est2 # should they be multiplied since they are in log space?

p <- p |>
  group_by(FID) |>
  mutate(est_diff = expest[which(year == 2023)] - expest[which(year == 2005)])

# p <- p |>
#   group_by(FID) |>
#   mutate(est_diff = expest[which(year == 2023)]/expest[which(year == 2005)])

ggplot(p, aes(UTM.lon, UTM.lat, colour = exp(expest))) +
  geom_tile() +
  scale_colour_viridis_c(trans = "log10")

ggplot(p, aes(UTM.lon, UTM.lat, colour = exp(est_diff))) +
  geom_tile() +
  #scale_colour_viridis_c(trans = "log10") +
  colorspace::scale_colour_continuous_divergingx(
    palette = "RdBu", mid = 0, trans = "log10")

ggplot(p, aes(UTM.lon, UTM.lat, colour = exp(est_diff))) +
  geom_tile() +
  #scale_colour_viridis_c(trans = "log10") +
  colorspace::scale_colour_continuous_divergingx(
    palette = "RdBu", mid = 0)


# coastwide maturity classes absolute declines map -----------------------------------------

fits <- readRDS("output/fit-trawl-svc-maturity.rds")
lapply(fits, \(x) x$family)

grab_svc_pred_LD <- function(fit) {
  cat("-")
  p <- predict(fit, newdata = filter(grid, year %in% c(2005, 2023)))
  if (sdmTMB:::is_delta(fit)) {
    p <- p |> mutate(estsum = (est1) + (est2)) #+ (est_non_rf1) + (est_non_rf2) + (est_rf1) + (est_rf2))
    p <- p |>
      group_by(FID) |>
      mutate(est_diff = exp(estsum[which(year == 2023)] - estsum[which(year == 2005)])) |>
      filter(year == 2005)
  } else {
    p <- p |>
      mutate(estsum = (est) )# |> + (est_non_rf) + (est_rf)) # |>
      #dplyr::select(c(longitude, latitude, UTM.lon, UTM.lat, estsum, FID, year, year))
    p <- p |>
      group_by(FID) |>
      mutate(est_diff = exp(estsum[which(year == 2023)] - estsum[which(year == 2005)])) |>
      filter(year == 2005)
  }
  p
}

preds <- lapply(fits, grab_svc_pred_LD)
x <- preds$imm
p <- c(list(p), preds) # add in the coast pred data
names(p) <- c("Combined", names(preds))

# ###end of determining change in biomass by grid cell


# fit <- readRDS("output/fit-trawl-svc-lognormal-mix.rds")
# b1 <- tidy(fit, model = 1)
# b2 <- tidy(fit, model = 2)
# z1 <- b1$estimate[b1$term == "year_scaled"]
# z2 <- b2$estimate[b1$term == "year_scaled"]
#
# # pick any year:
# p <- predict(fit, newdata = filter(grid, year == min(fit$data$year)))
# p$svc <- z1 + z2 + p$zeta_s_year_scaled1 + p$zeta_s_year_scaled2
# # p$combined_intercept <- p$est_non_rf1 + p$omega_s1 + p$est_non_rf2 + p$omega_s2
# p_start <- predict(fit, newdata = filter(grid, year == min(fit$data$year)))
# p$combined_intercept <- p_start$est1 + p_start$est2
#
# # instead of intercept at scaled year = 0 (i.e., 2010), take prediction at an
# # early time step
#
# # mat_trend_fits <- readRDS("output/fit-trawl-by-maturity-poisson-link-gengamma.rds")
# # mm <- lapply(mat_trend_fits, \(x) x$data$lengthgroup[1]) |> unlist()
# # names(mat_trend_fits) <- mm
# #
# fit_trend <- readRDS("output/fit-trawl-coast-lognormal-mix-poisson-link-30-45.rds")
#
# # p_start <- predict(fit_trend, newdata = filter(grid, year == min(fit$data$year))) # 2005 or 2006
# # p$combined_intercept <- p_start$est1 + p_start$est2
#
# # maturity svc fits -------------------------------------------------------
#
# fits <- readRDS("output/fit-trawl-svc-maturity.rds")
#
# lapply(fits, \(x) x$family)
#
# grab_svc_pred <- function(fit) {
#   cat("-")
#   p <- predict(fit, newdata = filter(grid, year == max(grid$year)))
#   if (sdmTMB:::is_delta(fit)) {
#     b1 <- tidy(fit, model = 1)
#     b2 <- tidy(fit, model = 2)
#     z1 <- b1$estimate[b1$term == "year_scaled"]
#     z2 <- b2$estimate[b1$term == "year_scaled"]
#     p$svc <- z1 + z2 + p$zeta_s_year_scaled1 + p$zeta_s_year_scaled2
#     # p$combined_intercept <- p$est_non_rf1 + p$omega_s1 + p$est_non_rf2 + p$omega_s2
#     p_start <- predict(fit, newdata = filter(grid, year == min(fit$data$year)))
#     p$combined_intercept <- p_start$est1 + p_start$est2
#   } else {
#     b1 <- tidy(fit)
#     z1 <- b1$estimate[b1$term == "year_scaled"]
#     p$svc <- z1 + p$zeta_s_year_scaled
#     # p$combined_intercept <- p$est_non_rf + p$omega_s
#     p_start <- predict(fit, newdata = filter(grid, year == min(fit$data$year)))
#     p$combined_intercept <- p_start$est
#   }
#   p
# }
# preds <- lapply(fits, grab_svc_pred)
#
# p <- c(list(p), preds)
# names(p) <- c("Combined", names(preds))

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

LIMS <- c(-0.5, 2) #<- set up limits to contract colour scheme and make visualizations better
LIMS <- c(0.05, 2) #<- set up limits to contract colour scheme and make visualizations better
LIMS <- c(0.0001, 1000) #<-
#LAB <- "Proportion\nbiomass change\nper decade"
LAB <- "Est. biomass change\n('05-'23)"

# main plot ---------------------------------------------------------------

test <- prs |> filter(group == "Combined") |> filter(year == 2005)
ggplot(test, aes(UTM.lon, UTM.lat, colour = (est_diff))) +
  geom_tile() +
  #scale_colour_viridis_c(trans = "log10") +
  colorspace::scale_colour_continuous_divergingx(
    palette = "RdBu", mid = 0)# , trans = "sqrt",
    #limits = LIMS)
unique(prs$group)

#g <-
  prs |>
    filter(year == 2005) |>
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
    #trans = "log10",
    limits = LIMS
  ) +
  colorspace::scale_fill_continuous_divergingx(
    palette = "RdBu", mid = 0,
    #trans = "log10",
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

ggsave("figs/svc-trawl-absolute.pdf", width = 7, height = 5.5)
ggsave("figs/svc-trawl-absolute.png", width = 7, height = 5.5)



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
  ggplot(aes(X, Y, fill = exp(est_diff ), colour = exp(est_diff ))) +
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
    trans = "log10",
    limits = LIMS
  ) +
  colorspace::scale_fill_continuous_divergingx(
    palette = "RdBu", mid = 0,
    trans = "log10",
    limits = LIMS
  ) +
   labs(fill = LAB, colour = LAB)
  # coord_sf(
  #   xlim = c(min(p$rotated_x), 1546730 - 1800000),
  #   ylim = range(p$rotated_y) + c(0, 100000)
  # )
    # ) +
  # geom_text(
  #   data = mids, mapping = aes(x = mean_x - 1000000, label = group_clean),
  #   y = 8500000, inherit.aes = FALSE, colour = "grey30", vjust = 1, hjust = 0
  # )

ggsave("figs/location-map.png", width = 7, height = 5.5)
#ggsave("figs/svc-trawl-absolute.png", width = 7, height = 5.5)
