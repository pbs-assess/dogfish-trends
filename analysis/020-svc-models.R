library(dplyr)
library(ggplot2)
library(sdmTMB)
source("analysis/999-prep-overall-trawl.R")

dat_coast <- filter(dat, survey_name %in%
    c("syn bc", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "GOA")) |>
  filter(year >= 2006)

# coast SVC trawl model ------------------------------------------------

domain <- fmesher::fm_nonconvex_hull_inla(
  as.matrix(dat_coast[, c("UTM.lon", "UTM.lat")]),
  concave = -0.07, convex = -0.05, resolution = c(200, 200)
)
mesh3 <- fmesher::fm_mesh_2d_inla(
  boundary = domain,
  max.edge = c(150, 2000),
  offset = c(150, 300),
  cutoff = 50
)
mesh <- make_mesh(dat_coast, c("UTM.lon", "UTM.lat"), mesh = mesh3)
plot(mesh)
mesh$mesh$n

mean(dat_coast$year_scaled)
sd(dat_coast$year_scaled)

fit <- sdmTMB(
  formula = catch_weight_t ~ 1 + year_scaled + poly(log(depth_m), 2),
  spatial_varying = ~ 0 + year_scaled, #<
  data = dat_coast,
  time = "year",
  offset = "offset_km2",
  mesh = mesh,
  spatial = "on",
  spatiotemporal = "off",
  family = delta_lognormal_mix(type = "poisson-link"),
  control = sdmTMBcontrol(
    start = list(logit_p_mix = qlogis(0.01)),
    map = list(logit_p_mix = factor(NA))
  ),
  silent = FALSE,
  share_range = FALSE,
  priors = sdmTMBpriors(
    matern_s = pc_matern(range_gt = 250, sigma_lt = 2),
    matern_st = pc_matern(range_gt = 250, sigma_lt = 2)
  ),
)
fit
sanity(fit)
rm(dat, mesh3, domain)
saveRDS(fit, file = "output/fit-trawl-svc-lognormal-mix.rds")
fit <- readRDS("output/fit-trawl-svc-lognormal-mix.rds")

set.seed(1)
s <- simulate(fit, 300L, type = "mle-mvn")
dh <- dharma_residuals(s, fit, return_DHARMa = TRUE)
DHARMa::plotQQunif(dh)

# SVC SD collapses:
if (FALSE) {
  fit1 <- update(fit, spatiotemporal = "iid")
  saveRDS(fit1, file = "output/fit-trawl-svc-iid-lognormal-mix.rds")
  fit1 <- readRDS("output/fit-trawl-svc-iid-lognormal-mix.rds")
  sanity(fit1)
  fit1
  set.seed(1)
  s1 <- simulate(fit1, 300L, type = "mle-mvn")
  dh1 <- dharma_residuals(s1, fit1, return_DHARMa = TRUE)
  DHARMa::plotQQunif(dh1)

  AIC(fit, fit1)
}

if (FALSE) {
  fit2 <- update(fit, spatiotemporal = "rw")
  fit2
  # SVC SD collapses
}

# Trawl by maturity group ----------------------------------------------

source("analysis/999-prep-overall-trawl.R")
rm(dat) # only keep 'grid'
source("analysis/999-prep-maturity-split-data.R")
d <- prep_maturity_split_data() |> filter(year >= 2006)
d$year_scaled <- (d$year - 2010) / 10

fit_maturity_group_svc <- function(dd) {
  cat(unique(dd$lengthgroup), "\n")
  this_mesh <- make_mesh(dd, c("X", "Y"), mesh = mesh$mesh)
  fit <- sdmTMB(
    formula = catch_weight_t ~ 1 + year_scaled + poly(log(depth_m), 2),
    spatial_varying = ~ 0 + year_scaled, #<
    data = dd,
    time = "year",
    offset = "offset_km",
    mesh = this_mesh,
    spatial = "on",
    spatiotemporal = "off",
    # family = delta_lognormal_mix(type = "poisson-link"),
    family = delta_lognormal(type = "poisson-link"),
    # control = sdmTMBcontrol(
    #   start = list(logit_p_mix = qlogis(0.01)),
    #   map = list(logit_p_mix = factor(NA))
    # ),
    silent = FALSE,
    share_range = FALSE,
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = 250, sigma_lt = 2),
      matern_st = pc_matern(range_gt = 250, sigma_lt = 2)
    ),
  )
}

ret <- split(d, d$lengthgroup) |> lapply(ds, fit_maturity_group_svc)
saveRDS(ret, file = "output/fit-trawl-svc-maturity-lognormal-mix.rds")

# IPHC -----------------------------------------------------------------

# not linear??
