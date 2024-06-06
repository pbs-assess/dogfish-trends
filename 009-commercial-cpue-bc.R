#Changes in distribution?
#Pull commercial data
#Look for changes in COG depth and lat/lon
#also correlate with environmental variables

sf_use_s2(FALSE)
# libraries ---------------------------------------------------------------
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
#remotes::install_github("pbs-assess/gfplot", force = TRUE)





# Create CRS for NWFSC, GOA, BC, and Coastal ---------------------------------------

Coastalcrs <- 32609

# Create map objects ------------------------------------------------------
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

# BC
bc_coast <- st_crop(
  map_data,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)
bc_coast_proj <- sf::st_transform(bc_coast, crs = 26909)


# pull commercial data -------------------------------------------

#landings and discards
# catch <- get_catch(species = "044")
# catch <- catch |> drop_na(lon)
# glimpse(catch)
# ggplot(catch, aes(lon, lat, colour = discarded_kg)) + geom_point()
# saveRDS(catch, "outputs/get_catch_dogfish.rds")

#get catch and retain trips with no catches
#extracts catch and effort data since 1996

#run this if it needs updating
# cpuetl <- gfdata::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)
# cpuetl <- cpuetl |> filter(species_code == "044")
# cpuetl <- cpuetl|>
#   mutate(date2 = as.Date(best_date, format = "%Y-%m-%d H:M:S")) |>
#   mutate(dmy = lubridate::ymd(date2)) |>
#   mutate(year = lubridate::year(dmy)) |>
#   mutate(month = lubridate::month(dmy)) |>
#   mutate(julian = lubridate::yday(dmy))
# saveRDS(cpuetl, "data-raw/cpue_bottomtrawl.rds")
cpuetl <- readRDS("data-raw/cpue_bottomtrawl.rds")


# remove points but include offshore points ---------------------------------------------------------
cpuetl |>
  ggplot() +
  geom_point(aes(longitude, latitude, size = catch_kg, colour = catch_kg)) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "sqrt")

#load shp of bc's waters, clip everything outside of this
shelf <- st_read("data-raw", "Shelf_polygon_noSOG")
shelf2 <- sf::st_transform(shelf, crs = "EPSG:32609")
shelf3 <- st_buffer(shelf2, dist = 25000) #25km
plot(st_geometry(shelf2))
plot(st_geometry(shelf3), add = TRUE)

cpuetl2 <- cpuetl |>
  mutate(longitude2 = longitude, latitude2 = latitude) |>
  drop_na(longitude, latitude)
cpuetl2 <- sdmTMB::add_utm_columns(cpuetl2,
                                   ll_names = c("longitude2", "latitude2"),
                                   utm_names = c("UTM.lon", "UTM.lat"),
                                   utm_crs = 32609)
cpuetl2 <- cpuetl2 |>
  mutate(UTM.lon.m = UTM.lon * 1000,
         UTM.lat.m = UTM.lat * 1000)
cpuetl_sp <- st_as_sf(cpuetl2,
                      coords = c("UTM.lon.m", "UTM.lat.m"),
                      crs = "EPSG:32609")

plot(st_geometry(shelf3))
plot(st_geometry(cpuetl_sp), add = TRUE)

cpuetl3 <- st_intersection(cpuetl_sp, st_geometry(shelf3)) %>%
  st_drop_geometry() #|>
cpuetl3 <- cpuetl3 |>
  mutate(UTM.lat.m = UTM.lat*1000, UTM.lon.m = UTM.lon*1000)
saveRDS(cpuetl3, "data-raw/cpuetl_incoffshore.rds")



# get fleet seans def -----------------------------------------------------


#get fleet information. See Sean's for parameter definition
dat <-
  gfplot::tidy_cpue_index(
    cpuetl,
    species_common = tolower("SPINY DOGFISH"),
    gear = "bottom trawl",
    #alt_year_start_date = "02-21", #what is this?
    use_alt_year = FALSE,
    year_range = c(1996, 2024),
    lat_range = c(48, Inf),
    min_positive_tows = 100,
    min_positive_trips = 5,
    min_yrs_with_trips = 5,
    depth_band_width = 1,
    area_grep_pattern = c("^5A|^5B|^5C|^5D|^5E|^3C|^3D"),
    depth_bin_quantiles = c(0.001, 0.999),
    min_bin_prop = 0.001,
    lat_band_width = 0.02#,
    #return_raw_data = TRUE
  )

#create matching columns to get longitude back into dat database
dat$fishing_event_id <- sub('.+-(.+)', '\\1', dat$fishing_event_id)
dat$fishing_event_id_unique <-
  paste0(dat$year, "-",
         dat$trip_id, "-",
         dat$fishing_event_id)
cpuetl$fishing_event_id_unique <-
  paste0(cpuetl$year, "-",
         cpuetl$trip_id, "-",
         cpuetl$fishing_event_id)
glimpse(cpuetl)
glimpse(dat)
cpuetl$month <- as.factor(cpuetl$month)
cpuetl <- cpuetl |> dplyr::select(c(fishing_event_id_unique,dmy, julian,longitude))
test <- left_join(dat, cpuetl, by = c("fishing_event_id_unique" = "fishing_event_id_unique"))
saveRDS(test, "data-raw/cpue_bottomtrawl_fleet.rds")

# cpuell <- get_cpue_index(gear = "hook and line")
# cpuell <- cpuell |> filter(species_code == "044")
# saveRDS(cpuell, "data-raw/cpue_ll.rds")
#
# cpuemd <- get_cpue_index(gear = "midwater trawl")
# cpuemd <- cpuemd |> filter(species_code == "044")
# saveRDS(cpuemd, "data-raw/cpue_midwater.rds")

# summary plots -----------------------------------------------------------
cpue_offshore <- readRDS("data-raw/cpuetl_incoffshore.rds")
fleet <- readRDS("data-raw/cpue_bottomtrawl_fleet.rds")
fleet$latitude <- as.numeric(as.character(fleet$latitude))
fleet$depth <- as.numeric(as.character(fleet$depth))

#map of offshore points and sean's fleet definition
ggplot() +
  geom_point(data = cpue_offshore,
             aes(longitude, latitude), colour = ("red"),
             alpha = 0.5) +
  geom_point(data = fleet,
             aes(longitude, latitude), colour = ("black"),
             alpha = 0.5) +
  #facet_wrap(~year) +
  #scale_colour_viridis_c(trans = "log") +
  #scale_colour_viridis_c() +
  theme_classic()
ggsave("Figures/Summary_fleet_versus_offshore.jpg", width = 10, height = 10)

ggplot() +
  geom_point(data = cpue_offshore,
             aes(longitude, latitude), colour = ("red"),
             alpha = 0.5) +
  geom_point(data = fleet,
             aes(longitude, latitude), colour = ("black"),
             alpha = 0.5) +
  xlim(c(-128, -125)) +
  ylim(c(48, 50)) +
  theme_classic() +
  facet_wrap(~year)
ggsave("Figures/Summary_fleet_versus_offshore_zoom.jpg", width = 10, height = 10)


fleet |>
  ggplot() +
  geom_point(aes(longitude, latitude, size = (cpue), colour = (cpue)),
             alpha = 0.5) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "log") +
  #scale_colour_viridis_c() +
  theme_classic()
ggsave("Figures/Summary_fleet.jpg", width = 10, height = 10)

cpue_offshore |>
  ggplot() +
  geom_point(aes(longitude, latitude, size = catch_kg, colour = catch_kg))

fleet |>
  group_by(year) |>
  summarize(sum_cpue = sum(cpue)) |>
  ggplot() +
  geom_point(aes(year, sum_cpue))

cpue_offshore |>
  group_by(year) |>
  summarize(sum_weight = sum(catch_kg)) |>
  filter(sum_weight > 0) |>
  ggplot() +
  geom_point(aes(year, sum_weight))

#remove some of the extremely shallow depths and deep depths
range(fleet$depth)
fleet$depth <- as.numeric(as.character(fleet$depth))
fleet |>
  ggplot() +
  geom_point(aes(year, (depth), size = cpue, colour = cpue)) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic()
ggsave("Figures/Summary_depth.jpg", width = 5, height = 4)

#get rid of 2024
fleet |>
  ggplot() +
  geom_point(aes(year, julian, size = cpue, colour = cpue))

cpue_offshore |>
  ggplot() +
  geom_point(aes(year, julian, size = catch_kg, colour = catch_kg))


# outlier depths? -----------------------------------------------
#cpue_offshore <- readRDS("data-raw/cpuetl_incoffshore.rds")

#get rid of extreme shallow and deep depths
#none to get rid of
fleet |>
  group_by(year) |>
  summarize(min = min(depth), max = max(depth))

fleet |>
  ggplot() +
  geom_point(aes(year, depth, colour = cpue, size = cpue)) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic()

# erroneous julian dates? --------------------------------------------
#none to get rid of
fleet |>
  group_by(year) |>
  summarize(min = min(julian), max = max(julian))
fleet |>
  #filter(cpue > 1000) |>
  ggplot() +
  geom_point(aes(year, (julian), size = cpue, colour = cpue)) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic()
ggsave("Figures/Summary_julian.jpg", width = 5, height = 4)




# load cleaned data and make mesh  ------------------------------------------------------
dat <- readRDS("data-raw/cpue_bottomtrawl_fleet.rds")
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
mesh <- sdmTMB::make_mesh(dat, c("X", "Y"), cutoff = 30)
plot(mesh$mesh)
points(dat$X, dat$Y)


# make grid ---------------------------------------------------------------
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
saveRDS(fit_sp, "data-generated/fit_cpue_dl.rds")
fit_sp <- readRDS("data-generated/fit_cpue_dl.rds")

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
saveRDS(fit_dgm, "data-generated/fit_dgm.rds")
sanity(fit_dgm)
sanity(fit_sp)
fit_sp
plot_anisotropy(fit_sp)
plot_anisotropy(fit_dgm)


# lognormal spatiotemporal?
tictoc::tic()
fit_spt <- update(
  fit_sp,
  anisotropy = FALSE, # convergence issues if TRUE
  spatiotemporal = "iid"
)
tictoc::toc()
saveRDS(fit_spt, "data-generated/fit_spt.rds")
sanity(fit_sp)
sanity(fit_spt) #this has issues
fit_spt

fit_dgm <- readRDS("data-generated/fit_dgm.rds")
AIC(fit_dgm)
AIC(fit_sp, fit_spt)
AIC(fit_sp, fit_dgm)


# get index ---------------------------------------------------------------
fit_dgm <- readRDS("data-generated/fit_dgm.rds")

ind_sp <- get_index(fit_sp, bias_correct = TRUE)
ggplot(ind_sp, aes(year, ymin = lwr, ymax = upr, y = est)) +
  geom_pointrange()
ind_dgm <- get_index(fit_dgm, bias_correct = TRUE)
ggplot(ind_dgm, aes(year, ymin = lwr, ymax = upr, y = est)) +
  geom_pointrange() + theme_classic()
ggsave("Figures/Index_dlognormalmixture.jpg", height = 4, width = 5)

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

#ggsave("figs/cpue/cpue-index-spatiotemporal.png", width = 6, height = 4)


# predict -----------------------------------------------------------------
p <- predict(fit_sp, newdata = gg, re_form_iid = NA)
saveRDS(p, "data-generated/prediction_sp.rds")

p <- predict(fit_dgm, newdata = gg, re_form_iid = NA)
saveRDS(p, "data-generated/prediction_dgm.rds")

#source(here::here("analysis/functions/make_sdmTMB_delta_maps.R"))



# COG ---------------------------------------------------------------------
#p <- predict(fit_sp, newdata = gg, re_form_iid = NA, return_tmb_object = TRUE)
p <- predict(fit_dgm, newdata = gg, re_form_iid = NA, return_tmb_object = TRUE)

cog <- get_cog(
  p,
  level = 0.95,
  format = c("wide"),
  area = 4,
  silent = TRUE
)

glimpse(cog)
#saveRDS(cog, "data-generated/cog_sp.rds")
#cog <- readRDS("data-generated/cog_sp.rds")
saveRDS(cog, "data-generated/cog_dgm.rds")
cog <- readRDS("data-generated/cog_dgm.rds")

ggplot(
  data = cog,
  aes(year, est_x )
) +
  geom_point() +
  geom_line() +
  theme_classic() +
  #geom_path(aes(colour = as.numeric(year), lwd = 1)) +
  geom_ribbon(aes(ymin = lwr_x, ymax = upr_x ), alpha = 0.4, col = "grey80")
ggsave("Figures/COGcomm_lon.jpg", width = 5, height = 4)
#ggsave("Figures/COGcomm_lon_noribbon.jpg", width = 5, height = 4)

# latitude
ggplot(
  data = cog,
  aes(year, est_y )
) +
  geom_point() +
  geom_line() +
  theme_classic() +
  #geom_path(aes(colour = as.numeric(year), lwd = 1)) +
  geom_ribbon(aes(ymin = lwr_y, ymax = upr_y ), alpha = 0.4, colour = "grey80")
ggsave("Figures/COGcomm_lat.jpg", width = 5, height = 4)
#ggsave("Figures/COGcomm_lat_noribbon.jpg", width = 5, height = 4)


# BC
bc_coast <- st_crop(
  map_data,
  c(xmin = -130, ymin = 50.4, xmax = -127, ymax = 51)
)
bc_coast_proj <- sf::st_transform(bc_coast, crs = 32609)


ggplot() +
  geom_sf(data =  bc_coast_proj) +
  geom_point(data = cog,
             aes(est_x*1000, est_y*1000, colour = year)
  ) +
  #geom_point() +
  theme_classic() #+
geom_path(aes(colour = as.numeric(year)), lwd = 1) +
  #geom_ribbon(aes(ymin = lwr_y, ymax = upr_y), alpha = 0.4)


  ggplot(data = cog,
         aes(est_x, est_y, colour = (year))) +
  #geom_sf(data =  bc_coast_proj) +
  geom_point() +
  theme_classic() +
  geom_text(aes(label = (year)))
#geom_line(aes(est_x, est_y, colour = (year)))
#geom_ribbon(aes(ymin = lwr_y, ymax = upr_y), alpha = 0.4)
ggsave("Figures/COGcomm_cog.jpg", width = 5, height = 4)


# model depth varying  -----------------------------------------------
hist(dat$spp_catch)
hist(log(dat$spp_catch + 1))
hist(filter(dat, spp_catch > 0) |> pull(spp_catch))
hist(filter(dat, spp_catch > 0) |> pull(spp_catch) |> log())

fit_tv <- update(fit_dgm,
                 # spp_catch ~ 0 + as.factor(year) +
                 #   depth_scaled + I(depth_scaled^2) +
                 #   (1 | vessel) + as.factor(month),
                 time_varying = ~ 0 +  depth_scaled + I(depth_scaled^2),
                 time_varying_type = "rw0")
saveRDS(fit_tv, file = "data-generated/hbll-out-sdmTMB_tv.rds")
sanity(fit_tv)
AIC(fit_tv, fit_dgm)


# get index depth varying ---------------------------------------------------------------
fit_tv <- readRDS("data-generated/hbll-out-sdmTMB_tv.rds")

ind_sp <- get_index(fit_tv, bias_correct = TRUE)
ggplot(ind_sp, aes(year, ymin = lwr, ymax = upr, y = est)) +
  geom_pointrange()
##ggsave("Figures/Index_dlognormalmixture.jpg", height = 4, width = 5)

# ind_sp |>
#   mutate(
#     upr = upr / exp(mean(log(est))),
#     lwr = lwr / exp(mean(log(est))),
#     est = est / exp(mean(log(est)))
#   ) |>
#   ggplot(aes(year, est)) +
#   geom_pointrange(aes(ymin = lwr, ymax = upr)) +
#   ylab("Standardized commercial trawl CPUE") +
#   xlab("Year") +
#   coord_cartesian(
#     expand = FALSE,
#     ylim = c(0, 3),
#     xlim = c(range(ind_sp$year) + c(-0.5, 0.5))
#   ) +
#   scale_x_continuous(breaks = seq(1996, 2024, 2))

#ggsave("figs/cpue/cpue-index-spatiotemporal.png", width = 6, height = 4)

# predict depth varying -----------------------------------------------------------------
p <- predict(fit_tv, newdata = gg, re_form_iid = NA)
saveRDS(p, "data-generated/prediction_cpue_tv.rds")
p <- readRDS("data-generated/prediction_cpue_tv.rds")
fit_tv
dat$depth

nd <- expand.grid(
  depth_m =
    seq(min(log(dat$depth)), max(dat$depth)),
  length.out = 500)
year <- c(unique(dat$year))
nd <- purrr::map_dfr(year, function(.x) {
  dplyr::mutate(nd, year = .x)
})
nd$offset <- 0

# run prediction
# create database of depth
p_maxdepth <- p %>%
  group_by(year) %>%
  summarize(max_est = max(est2),
            xint = depth[est2 == max_est])

ggplot(
  p, aes((depth), exp(est2)), group = as.factor(year)) +
  geom_line(size = 1, aes(group = as.factor(year),
                          colour = as.factor(year))) +
  # geom_vline(
  #   data = p_maxdepth,
  #   aes(xintercept = (xint), group = (year),
  #       colour = year), alpha = 0.4, size = 1
  # ) +
  theme_classic() +
  labs(colour = "Year", size = 20) +
  scale_x_continuous(name = "Depth (m)")+
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
    strip.text = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 18),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  scale_colour_viridis_c(option = "mako", name = "Year") #+
ggsave("Figures/depth_cpue.jpg", width = 5, height = 5)

ggplot(
  p_maxdepth,
  aes(year, (xint) * -1)
) +
  geom_point(aes(colour = year), size = 2) +
  geom_line(aes(colour = year)) +
  theme_classic() +
  scale_colour_viridis_c(option = "mako") +
  xlab(label = "Year") +
  ylab(label = "Depth (m)") +
  guides(fill = "none", colour = "none", label = "Title") +
  theme(#axis.title = element_text(size = 25),
    strip.text = element_blank(),
    # axis.ticks = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 15)
  ) +
  labs(colour = "Year", size = 20)
ggsave("Figures/depth_cpue_lineplot.jpg", width = 5, height = 5)


