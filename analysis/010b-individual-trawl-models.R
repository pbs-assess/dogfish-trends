library(sdmTMB)
library(dplyr)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())

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
saveRDS(out, file = "output/fit-trawl-by-region-lognormal-mix-poisson-link.rds")
# out <- readRDS("output/fit-trawl-by-region-lognormal-mix-poisson-link.rds")

coast_index <- readRDS("output/coast-index-trawl.rds")
aa <- bind_rows(
  mutate(ind, model = "Region-specific"),
  mutate(coast_index, model = "Combined")
)
aa$region <- gsub("Coast", "Coastwide", aa$region)
aa$region <- gsub("GOA", "Gulf of Alaska", aa$region)
aa$region <- gsub("BC", "British Columbia", aa$region)
aa$region <- gsub("NWFSC", "US West Coast", aa$region)

saveRDS(aa, "output/trawl-coast-indexes.rds")
