library(dplyr)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())
source("data-prep/00-set-crs.R")
source("analysis/999-colours-etc.R")
set_starting_year <- 1983
set_starting_year_iphc <- 1995
# map -----------------------------------------------------------------------

source("analysis/999-prep-overall-trawl.R")
sf::sf_use_s2(FALSE)
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
coast <- sf::st_crop(
  map_data,
  c(xmin = -175, ymin = 20, xmax = -115, ymax = 70)
)

coast_proj <- sf::st_transform(coast, crs = 32612)
df <- sdmTMB::add_utm_columns(dat, units = "m", utm_crs = 32612) |>
  filter(!survey_name %in% c("msa bc")) |>
  mutate(survey_name = gsub("syn bc", "BC", survey_name)) |>
  mutate(survey_name = gsub("Triennial", "NWFSC", survey_name)) |>
  mutate(survey_name = gsub("NWFSC.Combo.pass1", "NWFSC", survey_name)) |>
  mutate(survey_name = gsub("NWFSC.Combo.pass2", "NWFSC", survey_name))

pnw <- ggplot() +
  geom_point(data = filter(df, year %in% c(2003, 2004, 2006, 2007)), aes(X, Y,
    col = survey_name), size = 0.02) +
  geom_sf(data = coast_proj, colour = "grey70", fill = "grey90") +
  xlim(range(df$X) + c(-300000, 10000)) +
  ylim(range(df$Y) + c(-10000, 1000000)) +
  theme_void() +
  guides(fill = "none", colour = "none") +
  scale_colour_manual(values = cols_region2) +
  annotate("text", x = min(df$X) - 100000 + 90000, y = max(df$Y) + 1000000, label = "(b)")

# prep HS subregional model
ind_hs <- readRDS("output/trawl-coast-indexes.rds") |> filter(subregion == "Hecate (subregion)")

fit_syn <- readRDS("output/fit-trawl-by-region-lognormal-poisson-link-NW-mix.rds")[["BC"]]$fit

nd <- readRDS("output/prediction_grid_hs.rds") |>
  add_utm_columns(units = "km", utm_crs = coast_crs) %>%
  rename("UTM.lon" = "X", "UTM.lat" = "Y") |>
  mutate(
    UTM.lon = round(UTM.lon, 3),
    X = UTM.lon, Y = UTM.lat,
    depth_m  = bot_depth,
    year = 2003L
  )
p <- predict(fit_syn, newdata = nd, return_tmb_object = TRUE)
hs_2003_from_syn <- get_index(p, bias_correct = TRUE, area = nd$area_km)

ind_syn <- readRDS("output/trawl-coast-indexes.rds") |>
  filter(year == 2003 & region == "British Columbia" &
           model == "Region-specific" & subregion == "Region-specific")

hs_syn_ratio <- hs_2003_from_syn$est/ind_syn$est

# trawl index ---------------------------------------------------------------

ind <- readRDS("output/trawl-coast-indexes.rds")



ind <- ind |> #filter(subregion != "Hecate (subregion)") |>
  group_by(region, subregion, model) |>
  mutate(geo_mean = exp(mean(log_est[year >= 2003]))) |>
  mutate(
    est = est / geo_mean,
    lwr = lwr / geo_mean,
    upr = upr / geo_mean
  )

## if you want to scale by proportion of BC represented by HS
# ind[ind$subregion== "Hecate (subregion)",]$est <- ind[ind$subregion== "Hecate (subregion)",]$est * hs_syn_ratio
# ind[ind$subregion== "Hecate (subregion)",]$lwr <- ind[ind$subregion== "Hecate (subregion)",]$lwr * hs_syn_ratio
# ind[ind$subregion== "Hecate (subregion)",]$upr <- ind[ind$subregion== "Hecate (subregion)",]$upr * hs_syn_ratio

ind <- ind |>
  mutate(region = factor(region,
    levels = c("Coastwide", "Gulf of Alaska", "British Columbia", "US West Coast")))

glmdf <- ind |> filter(year >= 2005, model == "Combined") |>
  group_by(region) |>
  group_split() |>
  purrr::map_dfr(\(x) {
    x$decade <- x$year / 10
    m <- glm(est ~ decade, data = x, family = Gamma(link = "log"))
    nd <- data.frame(year = seq(min(x$year), max(x$year)))
    nd$decade = nd$year / 10
    p <- predict(m, newdata = nd, type = "response")
    ret <- data.frame(nd, glm_pred = p)
    row.names(ret) <- NULL
    ret$region <- x$region[1]
    ret$slope <- coef(m)[[2]]
    ci <- confint(m)
    ret$lwr <- ci[2, 1]
    ret$upr <- ci[2, 2]
    ret
  })
saveRDS(glmdf, file = "output/glmdf-overall-trends.rds")

lab_pos <- ind |> group_by(region) |>
  summarise(max_y = max(upr)) |>
  mutate(region_lab = paste0("(", letters[4:7], ") ", region))

theme_set(ggsidekick::theme_sleek())
gg_trawl <- filter(ind, model == "Combined") |>
  ggplot(aes(year, est, group = model, colour = region, ymin = lwr, ymax = upr)) +
  facet_wrap(~region, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = cols_region3) +
  geom_pointrange(data = filter(ind, model != "Combined", subregion == "Hecate (subregion)"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey70", alpha = 0.6) +
  geom_pointrange(data = filter(ind, model != "Combined", subregion != "Hecate (subregion)"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey30", alpha = 0.6) +
  geom_pointrange(
    size = 0.2, pch = 21) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, xlim = c(set_starting_year-1, 2024)) +
  geom_line(aes(x = year, y = glm_pred), inherit.aes = FALSE, data = glmdf, lwd = .9, colour = "grey35") +
  theme(legend.position.inside = c(0.25, 0.86), legend.position = "inside") +
  guides(colour = "none") +
  labs(x = "Year", y = "Trawl survey biomass index", colour = "Model") +
  geom_text(data = lab_pos, mapping = aes(y = max_y * 0.9, label = region_lab), x = 2022,
    inherit.aes = FALSE, vjust = 0.5, hjust = 1, size = 3) +
  theme(strip.text.x = element_blank(), strip.background.x = element_blank(), panel.spacing.y = unit(-0.1, "lines"))+
  theme(axis.title.x.bottom = element_text(size = 9))
gg_trawl

# IPHC index ----------------------------------------------------------------

ind_ll <- readRDS("output/indexes-iphc-nb2-coastwide.rds")
ind_ll$region <- gsub("^Coast$", "Coastwide", ind_ll$region)
ind_ll <- ind_ll |>
  mutate(region = factor(region,
    levels = c("Coastwide", "Gulf of Alaska", "British Columbia", "US West Coast")))
ind_ll <- mutate(ind_ll, est = est / 1000, lwr = lwr / 1000, upr = upr / 1000)

ind_ll <- filter(ind_ll, !(region == "US West Coast" & year %in% c(1998, 2000, 2020, 2021, 2022)))

glmdf_ll <- ind_ll |> #filter(year >= 2006) |>
  group_by(region) |>
  group_split() |>
  purrr::map_dfr(\(x) {
    x$decade <- x$year / 10
    m <- mgcv::gam(est ~ s(decade, k = 6), data = x, family = Gamma(link = "log"))
    nd <- data.frame(year = seq(min(x$year), max(x$year), length.out = 200))
    nd$decade = nd$year / 10
    p <- predict(m, newdata = nd)
    ret <- data.frame(nd, glm_pred = exp(p))
    row.names(ret) <- NULL
    ret$region <- x$region[1]
    ret$slope <- coef(m)[[2]]
    ret
  })

lab_pos <- ind_ll |> group_by(region) |>
  summarise(max_y = max(upr)) |>
  mutate(region_lab = paste0("(", letters[8:11], ") ", region))

gg_iphc <-
  ind_ll |>
  ggplot(aes(year, est, colour = region)) +
  scale_colour_manual(values = cols_region3) +
  facet_wrap(~region, scales = "free_y", ncol = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.2, pch = 21) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, xlim = c(set_starting_year_iphc, 2024)) +
  geom_line(aes(x = year, y = glm_pred), inherit.aes = FALSE, data = glmdf_ll, lwd = .9, colour = "grey35") +
  geom_text(data = lab_pos, mapping = aes(y = max_y * 0.9, label = region_lab), x = 2022,
    inherit.aes = FALSE, vjust = 0.5, hjust = 1, size = 3) +
  theme(legend.position.inside = c(0.25, 0.86), legend.position = "inside") +
  guides(colour = "none") +
  labs(x = "Year", y = "IPHC longline survey abundance index", colour = "Model") +
  theme(strip.text.x = element_blank(), strip.background.x = element_blank(), panel.spacing.y = unit(-0.1, "lines"))+
  theme(axis.title.x.bottom = element_text(size = 9))
gg_iphc

g_coefs <- glmdf |> select(region, slope, lwr, upr) |> distinct() |>
  mutate(slope = exp(slope), lwr = exp(lwr), upr = exp(upr)) |>
  mutate(region = forcats::fct_rev(region)) |>
  ggplot(aes(slope, 1, xmin = lwr, xmax = upr, colour = region)) +
  geom_vline(xintercept = 1, lty = 2, colour = "grey70") +
  geom_pointrange(position = position_dodge(width = 0.3), pch = 21) +
  scale_colour_manual(values = cols_region, guide = guide_legend(reverse = TRUE)) +
  ggsidekick::theme_sleek() +
  # scale_x_log10(breaks = c(0.3, 0.5, 0.7, 1)) +
  theme(axis.title.y.left = element_blank()) +
  xlab("Proportional change per decade") +
  labs(colour = "Region") +
  guides(colour = "none") +
  coord_cartesian(xlim = c(0.3, 1.01)) +
  theme(axis.text.y.left = element_blank(), axis.ticks.y.left = element_blank()) +
  theme(axis.title.x.bottom = element_text(size = 9)) +
  annotate("text", x = 0.325, y = 1.1, label = "(c)")

g_trend_panels <- cowplot::plot_grid(gg_trawl, gg_iphc, ncol = 2L, align = "h")

#img <- magick::image_read("figs/Spiny_dogfish.jpg")
img <- magick::image_read("Spiny Dogfish Sketch_3D Paint.jpg")
(dog_image <- magick::image_ggplot(img) +
  # theme(axis.text = element_text()) +
  annotate("text",
           x = 355, y = 1510,
           # x = 115, y = 410, colour = "white",
           label = "(a)"
  ))
g_left_panels <- cowplot::plot_grid(dog_image, pnw, g_coefs, ncol = 1L, rel_heights = c(1.4, 3, 1))

g <- cowplot::plot_grid(g_left_panels, g_trend_panels, rel_widths = c(1.2, 3), ncol = 2L, align = "h")
print(g)

ggsave("figs/overall-survey-trends2.pdf", width = 6.7, height = 5.4)
ggsave("figs/overall-survey-trends2.png", width = 6.7, height = 5.4)
