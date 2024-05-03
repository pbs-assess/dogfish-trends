library(sdmTMB)
library(dplyr)
library(ggplot2)
library(sf)
source("analysis/999-prep-overall-trawl.R")
source("analysis/999-rotate.R")
grid <- mutate(grid, X = UTM.lon, Y = UTM.lat)

fit <- readRDS("output/fit-trawl-svc-lognormal-mix.rds")
b1 <- tidy(fit, model = 1)
b2 <- tidy(fit, model = 2)
z1 <- b1$estimate[b1$term == "year_scaled"]
z2 <- b2$estimate[b1$term == "year_scaled"]

# pick any year:
p <- predict(fit, newdata = filter(grid, year == max(grid$year)))
p$svc <- z1 + z2 + p$zeta_s_year_scaled1 + p$zeta_s_year_scaled2
p$combined_intercept <- p$est_non_rf1 + p$omega_s1 + p$est_non_rf2 + p$omega_s2

# instead of intercept at scaled year = 0 (i.e., 2010), take prediction at an
# early time step

p_start <- predict(fit, newdata = filter(grid, year == min(fit$data$year)))
p$combined_intercept <- p_start$est1 + p_start$est2

# maturity svc fits -------------------------------------------------------

fits <- readRDS("output/fit-trawl-svc-maturity.rds")

lapply(fits, \(x) x$family)

grab_svc_pred <- function(fit) {
  p <- predict(fit, newdata = filter(grid, year == max(grid$year)))
  if (sdmTMB:::is_delta(fit)) {
    b1 <- tidy(fit, model = 1)
    b2 <- tidy(fit, model = 2)
    z1 <- b1$estimate[b1$term == "year_scaled"]
    z2 <- b2$estimate[b1$term == "year_scaled"]
    p$svc <- z1 + z2 + p$zeta_s_year_scaled1 + p$zeta_s_year_scaled2
    # p$combined_intercept <- p$est_non_rf1 + p$omega_s1 + p$est_non_rf2 + p$omega_s2
    p_start <- predict(fit, newdata = filter(grid, year == min(fit$data$year)))
    p$combined_intercept <- p_start$est1 + p_start$est2

  } else {
    b1 <- tidy(fit)
    z1 <- b1$estimate[b1$term == "year_scaled"]
    p$svc <- z1 + p$zeta_s_year_scaled
    # p$combined_intercept <- p$est_non_rf + p$omega_s
    p_start <- predict(fit, newdata = filter(grid, year == min(fit$data$year)))
    p$combined_intercept <- p_start$est
  }
  p
}
preds <- lapply(fits, grab_svc_pred)

p <- c(list(p), preds)
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

GAP <- 750000 # shift between panels in UTM m
prsl <- lapply(seq_along(pr), \(i) {
  d <- pr2[[i]]
  d$rotated_x <- d$rotated_x - GAP * (i - 1)
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
# 'mids' is for placing the maturity labels at the top:
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

LIMS <- c(0.05, 2)
LAB <- "Proportion\nbiomass change\nper decade"

ggplot(prs, aes(exp(combined_intercept))) + geom_histogram() + facet_wrap(~group_clean) + scale_x_log10()

prs2 <- prs |>
  mutate(combined_intercept = ifelse(group == "Combined",
    combined_intercept + log(1000), combined_intercept))

# scale right?
ggplot(prs2, aes(exp(combined_intercept))) + geom_histogram() + facet_wrap(~group_clean) + scale_x_log10()



# main plot ---------------------------------------------------------------

g1 <- prs2 |>
  mutate(svc = ifelse(exp(svc) >= LIMS[2], log(LIMS[2] - 1e-6), svc)) |>
  mutate(svc = ifelse(exp(svc) <= LIMS[1], log(LIMS[1] + 1e-6), svc)) |>
  ggplot(aes(rotated_x, rotated_y, fill = exp(svc), colour = exp(svc))) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.5),
    legend.title = element_text(colour = "grey30", size = 10),
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  geom_sf(data = rotated_coast, inherit.aes = FALSE, fill = "grey70", colour = "grey40") +
  geom_tile(width = 3000, height = 3000) +
  colorspace::scale_colour_continuous_divergingx(
    palette = "RdBu", mid = 0, trans = "log10",
    limits = LIMS
  ) +
  colorspace::scale_fill_continuous_divergingx(
    palette = "RdBu", mid = 0, trans = "log10",
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

LABS2 <- "Relative\nbiomass\ndensity"

range01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

prs3 <- prs2 |>
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

g <- cowplot::plot_grid(g2, g1, nrow = 2, align = "v", labels = c("(a)", "(b)"), label_fontface = "plain", label_colour = "grey30", label_size = 12)

ggsave("figs/svc-trawl-stacked-start.pdf", width = 6.1, height = 10)
ggsave("figs/svc-trawl-stacked-start.png", width = 6.1, height = 10)
