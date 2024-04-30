library(dplyr)
library(ggplot2)
library(sdmTMB)
theme_set(ggsidekick::theme_sleek())

source("analysis/01-prep-overall-trawl.R")

dat_coast <- filter(dat, survey_name %in%
    c("syn bc", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "GOA")) |>
  filter(year >= 2003)
dat_coast$survey_name <- factor(dat_coast$survey_name,
  levels = c("GOA", "syn bc", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2"))
levels(dat_coast$survey_name)
table(dat_coast$survey_name)

# coast trawl model ---------------------------------------------------------

domain <- fmesher::fm_nonconvex_hull_inla(
  as.matrix(dat_coast[, c("UTM.lon", "UTM.lat")]),
  concave = -0.07, convex = -0.05, resolution = c(200, 200)
)
plot(domain)
mesh3 <- fmesher::fm_mesh_2d_inla(
  boundary = domain,
  max.edge = c(150, 2000),
  offset = c(150, 300),
  cutoff = 50
)
mesh <- make_mesh(dat_coast, c("UTM.lon", "UTM.lat"), mesh = mesh3)
plot(mesh)
mesh$mesh$n

CUTOFF <- 100 # meters depth

dd <- list(
  filter(dat_coast, depth_m < CUTOFF),
  filter(dat_coast, depth_m >= CUTOFF)
)

fits <- lapply(dd, \(x) {
  .mesh <- make_mesh(x, c("UTM.lon", "UTM.lat"), mesh = mesh$mesh)
  fit <- sdmTMB(
    formula = catch_weight_t ~ 1 + poly(log(depth_m), 2),
    data = x,
    time = "year",
    offset = "offset_km2",
    mesh = .mesh,
    spatial = "on",
    spatiotemporal = "rw",
    family = delta_lognormal_mix(type = "poisson-link"),
    control = sdmTMBcontrol(
      start = list(logit_p_mix = qlogis(0.01)),
      map = list(logit_p_mix = factor(NA))
    ),
    silent = FALSE,
    share_range = FALSE,
    extra_time = seq(min(x$year), max(x$year)),
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = 250, sigma_lt = 2),
      matern_st = pc_matern(range_gt = 250, sigma_lt = 2)
    ),
  )
})
fits
purrr::walk(fits, sanity)

saveRDS(fits, file = "output/fit-by-depth-poisson-link.rds")

indexes <- lapply(seq_along(fits), \(i) {
  cat(i, "\n")
  if (i == 1) {
    nd <- dplyr::filter(grid, depth_m < CUTOFF)
    id <- "shallow"
  } else {
    nd <- dplyr::filter(grid, depth_m >= CUTOFF)
    id <- "deep"
  }
  nd <- filter(nd, year %in% fits[[i]]$data$year)
  pred <- predict(fits[[i]], newdata = nd, return_tmb_object = TRUE)
  ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
  ind$group <- id
  gc()
  ind
})
saveRDS(indexes, file = "output/indexes-by-depth-poisson-link.rds")

bind_rows(indexes) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = group)) +
  geom_pointrange() +
  facet_wrap(~group)

# try with the main model ---------------------------

fit <- readRDS("output/fit-trawl-coast-lognormal-mix.rds")
fit

tofit <- expand.grid(region = unique(grid$region), depth = c("shallow", "deep"))


