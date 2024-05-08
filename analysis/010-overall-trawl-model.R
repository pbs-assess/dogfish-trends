library(dplyr)
library(ggplot2)
library(sdmTMB)
theme_set(ggsidekick::theme_sleek())

source("analysis/999-prep-overall-trawl.R")

dat_coast <- filter(dat, survey_name %in%
    c("syn bc", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "GOA")) |>
  filter(year >= 2003)
dat_coast$survey_name <- factor(dat_coast$survey_name,
  levels = c("GOA", "syn bc", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2"))
levels(dat_coast$survey_name)
table(dat_coast$survey_name)

# coast trawl model ---------------------------------------------------------

# custom mesh:
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
ggplot() + inlabru::gg(mesh$mesh) +
  geom_point(data = dat_coast, aes(UTM.lon, UTM.lat), size = 0.5, alpha = 0.07, pch = 21) +
  xlab("UTM (km)") + ylab("UTM (km)") + coord_fixed()
ggsave("figs/trawl-model-mesh.pdf", width = 6, height = 6)
ggsave("figs/trawl-model-mesh.png", width = 6, height = 6)

mesh$mesh$n

tictoc::tic()
fit1 <- sdmTMB(
  formula = catch_weight_t ~ 1 + poly(log(depth_m), 2),
  data = dat_coast,
  time = "year",
  offset = "offset_km2",
  mesh = mesh,
  spatial = "on",
  spatiotemporal = "rw",
  family = delta_lognormal_mix(),
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
tictoc::toc()
fit1
sanity(fit1)
rm(dat, mesh3, domain)
saveRDS(fit1, file = "output/fit-trawl-coast-lognormal-mix.rds")
fit1 <- readRDS("output/fit-trawl-coast-lognormal-mix.rds")

p1 <- get_pars(fit1)
plogis(p1$logit_p_mix)
1 + exp(p1$log_ratio_mix)
fit1$sd_report

set.seed(1)
s1 <- simulate(fit1, 500L, type = "mle-mvn")

dh1 <- dharma_residuals(s1, fit1, return_DHARMa = TRUE)
plot(dh1)
DHARMa::plotQQunif(dh1, testDispersion = FALSE)


tictoc::tic()
fit4 <- update(fit1, family = delta_lognormal_mix(type = "poisson-link"))
tictoc::toc()

saveRDS(fit4, file = "output/fit-trawl-coast-lognormal-mix-poisson-link.rds")
fit4 <- readRDS("output/fit-trawl-coast-lognormal-mix-poisson-link.rds")

AIC(fit1, fit4)

set.seed(123)
s4 <- simulate(fit4, 400L, type = "mle-mvn")

r <- dharma_residuals(s4, fit4)
ggplot(r, aes(expected, observed)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(x = "Expected", y = "Observed") +
  coord_equal(expand = FALSE, xlim = c(0, 1), ylim = c(0, 1))
ggsave("figs/qq-trawl-main.png", width = 4, height = 4)

dh4 <- dharma_residuals(s4, fit4, return_DHARMa = TRUE)
DHARMa::plotQQunif(dh4, testOutliers = FALSE, testDispersion = FALSE)

if (FALSE) {
  fit2 <- update(fit4, family = delta_lognormal())
  fit2
  sanity(fit2)
  AIC(fit1, fit2)

  set.seed(1)
  s2 <- simulate(fit2, 300L, type = "mle-mvn")
  dh2 <- dharma_residuals(s2, fit2, return_DHARMa = TRUE)
  DHARMa::testResiduals(dh2)
}

if (FALSE) {
  fit3 <- update(
    fit1,
    family = delta_gengamma()
  )
  fit3
  sanity(fit3)
  AIC(fit1, fit3)

  set.seed(1)
  s3 <- simulate(fit3, 300L, type = "mle-mvn")
  dh3 <- dharma_residuals(s3, fit3, return_DHARMa = TRUE)
  DHARMa::testResiduals(dh3)
}

# compare to including survey catchability effects:

dat_coast$survey_name <- factor(dat_coast$survey_name,
  levels = c("syn bc", "GOA", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2"))

fitq <- sdmTMB(
  formula = catch_weight_t ~ 1 + poly(log(depth_m), 2) + factor(survey_name),
  data = dat_coast,
  time = "year",
  offset = "offset_km2",
  mesh = mesh,
  spatial = "on",
  spatiotemporal = "rw",
  family = delta_lognormal_mix(),
  control = sdmTMBcontrol(
    start = list(logit_p_mix = qlogis(0.01)),
    map = list(logit_p_mix = factor(NA))
  ),
  silent = FALSE,
  share_range = FALSE,
  # do_fit = F,
  priors = sdmTMBpriors(
    matern_s = pc_matern(range_gt = 250, sigma_lt = 2),
    matern_st = pc_matern(range_gt = 250, sigma_lt = 2),
    b = normal(c(NA, NA, NA, 0, 0, 0), c(NA, NA, NA, 1, 1, 1))
  ),
)
AIC(fit4, fitq)
sanity(fitq)
fitq
fitq$sd_report

grid <- filter(grid, year %in% dat_coast$year)
grid$survey_name <- factor("syn bc", levels = levels(dat_coast$survey_name))

# chunk years to keep memory down:
chunk_years <- function(x, chunks) {
  ny <- length(x)
  split(x, rep(seq_len(chunks), each = ceiling(ny/chunks))[seq_along(x)])
}
yrs <- unique(dat_coast$year)
yy <- chunk_years(yrs, 2)
yy

fit <- fit4 # !!

index_l <- lapply(yy, \(y) {
  cat(y, "\n")
  nd <- dplyr::filter(grid, year %in% y)
  pred <- predict(fit, newdata = nd, return_tmb_object = TRUE)
  ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
  gc()
  ind
})
index <- do.call(rbind, index_l)

index_l <- lapply(yy, \(y) {
  cat(y, "\n")
  nd <- dplyr::filter(grid, year %in% y)
  pred <- predict(fit4, newdata = nd, return_tmb_object = TRUE)
  ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
  gc()
  ind
})
indexq <- do.call(rbind, index_l)

# apply coast model to regions ----------------------------------------------

# Alaska fairly hard on memory, but still OK ~max 20 GB
regions <- unique(grid$region)

gc()

do_expanions <- function(model) {
  lapply(regions, \(r) {
    cat(r, "\n")
    nd <- dplyr::filter(grid, region %in% r)
    pred <- predict(model, newdata = nd, return_tmb_object = TRUE)
    ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
    ind$region <- r
    gc()
    ind
  })
}
index_reg_l <- do_expanions(fit)
index_reg_lq <- do_expanions(fitq)
index_reg <- do.call(rbind, index_reg_l)
index_regq <- do.call(rbind, index_reg_lq)

bind_rows(
  mutate(index, model = "No catchability effects", region = "Coastwide"),
  mutate(indexq, model = "With catchability effects", region = "Coastwide"),
  mutate(index_reg, model = "No catchability effects"),
  mutate(index_regq, model = "With catchability effects")
) |>
  mutate(region = gsub("NWFSC", "US West Coast", region)) |>
  mutate(region = factor(region, levels = c("Coastwide", "GOA", "BC", "US West Coast"))) |>
  group_by(model, region) |>
  mutate(geo_mean = exp(mean(log_est[year >= 2003]))) |>
  mutate(
    est = est / geo_mean,
    lwr = lwr / geo_mean,
    upr = upr / geo_mean
  ) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = model, fill = model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  xlab("Year") +
  ylab("Standardized relative biomass") +
  labs(colour = "Model", fill = "Model") +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, xlim = range(index$year) + c(-0.5, 0.5)) +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~region, ncol = 1)
ggsave("figs/index-trawl-main-by-region-catchability-effects.pdf", width = 5.5, height = 10)

ind <- bind_rows(mutate(index, region = "Coast"), index_reg)

ind$region <- factor(ind$region, levels = c("Coast", "GOA", "BC", "NWFSC"))

geomean <- exp(mean(log(index$est)))
mutate(ind,
  est = est / geomean,
  lwr = lwr / geomean,
  upr = upr / geomean
) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
  facet_wrap(~region, scales = "free_y", ncol = 1) +
  geom_ribbon(fill = "grey90") +
  geom_line() +
  coord_cartesian(ylim = c(0, NA))

coast_index <- ind

# iterate over regions with individual fits ---------------------------------

source("analysis/999-prep-overall-trawl.R")
table(dat$survey_name)
dat$region <- ""
dat$region[dat$survey_name %in%
    c("NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "NWFSC.Slope", "Triennial")] <- "NWFSC"
dat$region[dat$survey_name %in% c("GOA")] <- "GOA"
dat$region[dat$survey_name %in% c("syn bc")] <- "BC"
dat$region[dat$survey_name %in% c("msa bc")] <- "BC"
table(dat$region)

table(dat$survey_name, dat$year)
table(dat$region, dat$year)

dat <- filter(dat, survey_name != "msa bc") # doesn't fit well, too big of an extrapolation

dat |> filter(region == "NWFSC", year < 2007) |>
  ggplot(aes(UTM.lon, UTM.lat, colour = survey_name)) + geom_point() +
  facet_grid(survey_name~year)

group_by(dat, survey_name) |>
  summarise(min_depth = min(depth_m), max_depth = max(depth_m))

# function to fit each:
fit_trawl_region <- function(dd) {
  print(unique(dd$region))
  domain <- fmesher::fm_nonconvex_hull_inla(
    as.matrix(dd[, c("UTM.lon", "UTM.lat")]),
    concave = -0.07, convex = -0.05, resolution = c(200, 200)
  )
  nwfsc <- any(grepl("NWFSC", dd$survey_name, ignore.case = TRUE))
  bc <- any(grepl("bc", dd$survey_name, ignore.case = TRUE))
  goa <- any(grepl("goa", dd$survey_name, ignore.case = TRUE))

  # pre 1998, there are 3 year gaps... and it's a lot to ask the
  # RW field to stitch together
  # 1998 onwards there is the NWFSC Slope survey every year to help
  # i.e., 1998 onewards has a survey every year
  if (nwfsc) dd <- filter(dd, year >= 1995)

  if (nwfsc || goa) {
    mesh3 <- fmesher::fm_mesh_2d_inla(
      boundary = domain,
      max.edge = c(100, 2000),
      offset = c(100, 250),
      cutoff = 40
    )
  } else if (bc) {
    mesh3 <- fmesher::fm_mesh_2d_inla(
      boundary = domain,
      max.edge = c(50, 2000),
      offset = c(50, 100),
      cutoff = 25
    )
    plot(mesh3)
    mesh3$n
  } else {
    stop("No region found")
  }
  mesh <- make_mesh(dd, c("UTM.lon", "UTM.lat"), mesh = mesh3)
  plot(mesh)
  mesh$mesh$n

  if (nwfsc) {
    dd$survey_name <- factor(dd$survey_name,
      levels = c("NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "NWFSC.Slope", "Triennial"))
  }
  if (length(unique(dd$survey_name)) > 1) {
    f <- catch_weight_t ~ survey_name + poly(log(depth_m), 2)
  } else {
    f <- catch_weight_t ~ poly(log(depth_m), 2)
  }
  table(dd$survey_name, dd$year)

  fit <- sdmTMB(
    formula = f,
    data = dd,
    time = "year",
    offset = "offset_km2",
    mesh = mesh,
    spatial = "on",
    spatiotemporal = "rw",
    silent = FALSE,
    share_range = FALSE,
    extra_time = seq(min(dd$year), max(dd$year)),
    family = delta_lognormal_mix(type = "poisson-link"),
    control = sdmTMBcontrol(
      start = list(logit_p_mix = qlogis(0.01)),
      map = list(logit_p_mix = factor(NA))
    ),
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = 200, sigma_lt = 2),
      matern_st = pc_matern(range_gt = 200, sigma_lt = 2)
    ),
  )
  fit
  sanity(fit)

  if (FALSE) {
    set.seed(1)
    s <- simulate(fit, 300L, type = "mle-mvn")
    dh <- dharma_residuals(s, fit, return_DHARMa = TRUE)
    plot(dh)
    DHARMa::testResiduals(dh)
  }

  nd <- grid
  nd <- filter(nd, region %in% dd$region)
  nd <- filter(nd, year %in% unique(dd$year))
  if (length(unique(dd$survey_name)) > 1) {
    nd$survey_name <-
      factor(levels(dd$survey_name)[1],
        levels = levels(dd$survey_name))
  }
  p <- predict(fit, newdata = nd, return_tmb_object = TRUE)
  ind <- get_index(p, bias_correct = TRUE, area = nd$area_km)
  list(index = ind, fit = fit, pred = p)
}

out <- split(dat, dat$region) |> lapply(fit_trawl_region)

ind <- purrr::map_dfr(out, \(x) {
  ii <- x$ind
  ii$region <- x$pred$data$region[1]
  ii
})

out$BC$fit
out$GOA$fit
out$NWFSC$fit
rm(fit, fit1, dat_coast)
saveRDS(out, file = "output/fit-trawl-by-region-lognormal-mix-poisson-link.rds")
out <- readRDS("output/fit-trawl-by-region-lognormal-mix-poisson-link.rds")

# depth effect --------------------------------------------------------------


aa <- bind_rows(
  mutate(ind, model = "Region-specific"),
  mutate(coast_index, model = "Combined")
)
aa$region <- gsub("Coast", "Coastwide", aa$region)
aa$region <- gsub("GOA", "Gulf of Alaska", aa$region)
aa$region <- gsub("BC", "British Columbia", aa$region)
aa$region <- gsub("NWFSC", "US West Coast", aa$region)

saveRDS(aa, "output/trawl-coast-indexes.rds")
