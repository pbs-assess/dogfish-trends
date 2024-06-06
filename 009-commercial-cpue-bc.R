#Pull commercial data
#Create index


#params
sf_use_s2(FALSE)
Coastalcrs <- 32609
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(
  map_data,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)
bc_coast_proj <- sf::st_transform(bc_coast, crs = 32609)

library(gfdata)
library(tidyverse)
library(sf)
library(ggplot2)
library(PBSmapping)
library(here)
library(tidyr)
library(purrr)
library(sdmTMB)
library(gfplot)

# load cleaned data and make mesh  ------------------------------------------------------
dat <- readRDS("output/wrangled-cpue.rds")
dat$depth <- as.numeric(as.character(dat$depth))
dat$latitude <- as.numeric(as.character(dat$latitude))
dat <- add_utm_columns(dat, c("longitude", "latitude"))
dat$log_depth <- log(dat$best_depth)
dat$vessel <- as.factor(dat$vessel_registration_number)
dat$month <- factor(dat$month)
dat <- filter(dat, year <= 2023)
filter(dat, hours_fished > 2000) # 8762.05!
dat <- filter(dat, hours_fished < 2000)
dat$depth_scaled <- (dat$log_depth - mean(dat$log_depth)) / sd(dat$log_depth)

# mesh --------------------------------------------------------------------
mesh <- sdmTMB::make_mesh(dat, c("X", "Y"), cutoff = 30)
plot(mesh$mesh)
points(dat$X, dat$Y)

# grid --------------------------------------------------------------------
grid <- gfplot::synoptic_grid
grid$log_depth <- log(grid$depth)
grid$survey_domain_year <- NULL
gg <- replicate_df(grid, "year", time_values = sort(unique(dat$year)))
gg$utm_zone <- NULL
gg$cell_area <- NULL
gg$survey_series_name <- NULL
#gg$month <- factor(6, levels = levels(dat$month))
gg$month <- "06"
gg$vessel <- 310988
#gg$vessel <- NA #wy does sean has this as NA
gg$depth_scaled <- (gg$log_depth - mean(dat$log_depth)) / sd(dat$log_depth)
unique(dat$month)

# model delta lognormal fit -----------------------------------------------
hist(dat$spp_catch)
hist(log(dat$spp_catch + 1))
hist(filter(dat, spp_catch > 0) |> pull(spp_catch))
hist(filter(dat, spp_catch > 0) |> pull(spp_catch) |> log())

fit_sp <- sdmTMB(
  spp_catch ~ 0 + as.factor(year) +
    depth_scaled + I(depth_scaled^2) +
    (1 | vessel) + as.factor(month),
  family = delta_lognormal(),
  mesh = mesh,
  offset = log(dat$hours_fished),
  spatial = "on",
  spatiotemporal = "off",
  data = dat,
  time = "year",
  anisotropy = TRUE,
  # priors = sdmTMBpriors(
  #   matern_s = pc_matern(range_gt = 40, sigma_lt = 3)
  # ),
  predict_args = list(newdata = gg, re_form_iid = NA),
  index_args = list(area = rep(4, nrow(gg))),
  do_index = TRUE,
  silent = FALSE
)
saveRDS(fit_sp, "output/fit_cpue_dl.rds")
#fit_sp <- readRDS("data-generated/fit_cpue_dl.rds")

# heavier than lognormal!? see histogram above and QQ plots below
fit_dgm <- update(
  fit_sp,
  spatiotemporal = "IID",
  anisotropy = FALSE,
  family = delta_lognormal_mix(),
  control = sdmTMBcontrol(
    start = list(logit_p_mix = qlogis(0.01)),
    map = list(logit_p_mix = factor(NA))
  )
)
saveRDS(fit_dgm, "output/fit_dgm.rds")

sanity(fit_dgm)
sanity(fit_sp)
fit_sp
plot_anisotropy(fit_sp)
plot_anisotropy(fit_dgm)


# # lognormal spatiotemporal?
# tictoc::tic()
# fit_spt <- update(
#   fit_sp,
#   anisotropy = FALSE, # convergence issues if TRUE
#   spatiotemporal = "iid"
# )
# tictoc::toc()
# #saveRDS(fit_spt, "data-generated/fit_spt.rds")
# sanity(fit_sp)
# sanity(fit_spt) #this has issues
# fit_spt




# get index ---------------------------------------------------------------
fit_sp <- readRDS("output/fit_cpue_dl.rds")
fit_dgm <- readRDS("output/fit_dgm.rds")
AIC(fit_sp, fit_dgm)

sanity(fit_dgm)
sanity(fit_sp)
fit_sp
plot_anisotropy(fit_sp)
plot_anisotropy(fit_dgm)

ind_sp <- get_index(fit_sp, bias_correct = TRUE)
ggplot(ind_sp, aes(year, ymin = lwr, ymax = upr, y = est)) +
  geom_pointrange()
ind_dgm <- get_index(fit_dgm, bias_correct = TRUE)
ggplot(ind_dgm, aes(year, ymin = lwr, ymax = upr, y = est)) +
  geom_pointrange() + theme_classic()

x <- ggplot(ind_dgm, aes(year, ymin = lwr, ymax = upr, y = est)) +
  geom_pointrange()
x + geom_pointrange(data = ind_sp, aes(year, ymin = lwr, ymax = upr, y = est), col = "red")

ind_sp |>
  mutate(
    upr = upr / exp(mean(log(est))),
    lwr = lwr / exp(mean(log(est))),
    est = est / exp(mean(log(est)))
  ) |>
  ggplot(aes(year, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ylab("Standardized commercial trawl CPUE") +
  xlab("Year") +
  coord_cartesian(
    expand = FALSE,
    ylim = c(0, 3),
    xlim = c(range(ind_sp$year) + c(-0.5, 0.5))
  ) +
  scale_x_continuous(breaks = seq(1996, 2024, 2))
ggsave("Figures/cpue-index-spatiotemporal.png", width = 6, height = 4)


# # predict -----------------------------------------------------------------
# p <- predict(fit_sp, newdata = gg, re_form_iid = NA)
# saveRDS(p, "output/prediction_sp.rds")
#
# p <- predict(fit_dgm, newdata = gg, re_form_iid = NA)
# saveRDS(p, "output/prediction_dgm.rds")
#
# #source(here::here("analysis/functions/make_sdmTMB_delta_maps.R"))



