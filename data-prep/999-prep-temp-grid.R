library(inlabru)
library(sdmTMB)
library(ggplot2)

# add temp to grid --------------------------------------------------------
#One "simple" way to do this is to use sdmTMB to project the observed bottom temperature to the grid cell level. Then use that for the density-weighted mean temperature.
#First, we generated predictions of gridded bottom temperature data using observations from the trawl surveys in our analysis. We fit in situ bottom temperature measurements from each of the trawl surveys as the response variable with penalized regression splines on depth and calendar day and spatiotemporal variability (spatiotemporal fields allow mean bottom temperature to be slightly different in each year and to vary in a non-linear pattern over time). Spatiotemporal fields were modeled as an autoregressive AR(1) process, allowing warm and cool locations to persist across time steps


source("analysis/999-prep-overall-trawl.R")

dat |> group_by(survey_name) |> drop_na(bottom_temp_c) |> summarize(min = min(bottom_temp_c), max = max(bottom_temp_c))
dat <- dat |> drop_na(bottom_temp_c)
range(dat$bottom_temp_c)
dat <- dat |> mutate(julian_std = julian - 182) #scaled julian to july 1st 2023
dat |> group_by(survey_name) |> drop_na(julian_std) |> summarize(min = min(julian_std), max = max(julian_std))
dat <- dat |> drop_na(bottom_temp_c)

table(dat$survey_name, dat$year)
table(dat$region, dat$year)

domain <- fmesher::fm_nonconvex_hull_inla(
  as.matrix(dat[, c("UTM.lon", "UTM.lat")]),
  concave = -0.02, convex = -0.02, resolution = c(200, 200)
)

# same as coastwide
min_edge <- 30
max_edge <- 45

#min_edge <- 60
#max_edge <- 90

mesh3 <- fmesher::fm_mesh_2d_inla(
  loc = as.matrix(dat[, c("UTM.lon", "UTM.lat")]),
  boundary = domain,
  max.edge = c(max_edge, 1000),
  offset = c(10, 300), #
  cutoff = min_edge
)

mesh <- make_mesh(dat, c("UTM.lon", "UTM.lat"), mesh = mesh3)
mesh$mesh$n

ggplot() +
  geom_point(
    data = dat, aes(UTM.lon, UTM.lat),
    # colour = "blue",
    size = 0.5, alpha = 0.2, pch = 21
  ) +
  inlabru::gg(mesh$mesh) +
  xlab("UTM (km)") +
  ylab("UTM (km)") +
  coord_fixed()

ggplot() +
  geom_point(
    data = dat, aes(UTM.lon, UTM.lat,
    colour = bottom_temp_c),
    size = 0.5, alpha = 0.2, pch = 21
  ) +
  inlabru::gg(mesh$mesh) +
  xlab("UTM (km)") +
  ylab("UTM (km)") +
  coord_fixed()

f <- bottom_temp_c ~ poly(logbot_depth, 2) + poly(julian_std,2) # <- use this
set_family <- tweedie()
set_spatial <- "on"

fit <- sdmTMB(
  formula = f,
  data = dat,
  time = "year",
  #offset = "offset_km2",
  mesh = mesh,
  spatial = set_spatial,
  spatiotemporal = "ar1",
  silent = FALSE,
  share_range = FALSE,
  extra_time = seq(min(dat$year), max(dat$year)),
  family = set_family,
)
sanity(fit)
tidy(fit, "ran_pars")
saveRDS(fit, "output/fit-temp-model.rds")


fit <- run_extra_optimization(fit)


# if (FALSE) {
#   set.seed(1)
#   s <- simulate(fit, 300L, type = "mle-mvn")
#   dh <- dharma_residuals(s, fit, return_DHARMa = TRUE)
#   plot(dh)
#   DHARMa::testResiduals(dh)
# }

nd <- grid
nd <- nd |> dplyr::select(-c(year, year_scaled)) |> distinct(.keep_all = TRUE)
#nd <- filter(nd, region %in% dat$region)
unique(dat$survey_name)
nd$julian_std <- mean(dat$julian_std)
year <- c(unique(dat$year))
nd <- purrr::map_dfr(year, function(.x) {
  dplyr::mutate(nd, year = .x)
})
p <- predict(fit, newdata = nd, return_tmb_object = TRUE)

range(exp(p$data$est))
saveRDS(p, "output/pred-coastal-temp-grid.rds")

unique(p$data$region)
ggplot(p$data, aes(longitude, latitude, colour = exp(est))) + geom_point() +
  facet_wrap(~year)

