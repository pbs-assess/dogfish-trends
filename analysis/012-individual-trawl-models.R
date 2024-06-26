library(sdmTMB)
library(dplyr)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())

# iterate over regions with individual fits ---------------------------------

source("analysis/999-prep-overall-trawl.R")

# but MSA in 2003 classified as syn BC
filter(dat, survey_name %in% c("syn bc", "msa bc"), year %in% c(2002, 2003, 2004)) |>
  ggplot() + geom_point(aes(longitude, latitude,
                            shape = survey_name,
                            colour = survey_abbrev)) +
  facet_grid(~year)

# leaving 2003 HS MSA classified as syn for the overall model since there's only one year of it and it's catchability isn't sig different from syn surveys when they are modelled together
# separating it here to make two separate models
dat <- dat |> mutate(
  survey_name = ifelse(survey_abbrev == "HS MSA", "msa bc", survey_name)
)

table(dat$survey_name)

dat$julian_c <- dat$julian - 172 # centered on summer solstice
dat$region <- ""
dat$region[dat$survey_name %in%
    c("NWFSC.Combo.pass1", "NWFSC.Combo.pass2",
      "AFSC.Slope", "NWFSC.Slope", "Triennial")] <- "NWFSC"
dat$region[dat$survey_name %in% c("GOA")] <- "GOA"
dat$region[dat$survey_name %in% c("syn bc")] <- "BC"
dat$region[dat$survey_name %in% c("msa bc")] <- "BC"
# dat$region[dat$survey_name %in% c("msa bc")] <- "HS" # use this for HS by its self
table(dat$region)

table(dat$survey_name, dat$year)
table(dat$region, dat$year)

dat |> filter(region == "NWFSC", year < 2003) |>
  ggplot(aes(UTM.lon, UTM.lat,
             colour = log(catch_weight_t), size = catch_weight_t)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_grid(survey_name~year)

dat |> filter(region == "NWFSC", year < 2007) |>
  ggplot() +
  geom_histogram(aes((depth_m))) +
  # geom_histogram(aes(log(catch_weight_t))) +
  # geom_histogram(aes(julian)) +
  # geom_histogram(aes(offset_km2)) +
  facet_wrap(~survey_name, scales = "free_y")

group_by(dat, survey_name) |>
  summarise(min_depth = min(depth_m), max_depth = max(depth_m))

ggplot(dat) +
  facet_wrap(~region) +
  geom_histogram(aes(log(catch_weight_t)))

# function to fit each:
fit_trawl_region <- function(dd) {
  print(unique(dd$region))

  nwfsc <- any(grepl("NWFSC", dd$survey_name, ignore.case = TRUE))
  bc <- any(grepl("BC", dd$region, ignore.case = TRUE))
  goa <- any(grepl("goa", dd$survey_name, ignore.case = TRUE))
  hs <- any(grepl("HS", dd$region, ignore.case = TRUE))
  # pre 1998, there are 3 year gaps... and it's a lot to ask the
  # RW field to stitch together
  # 1998 onwards there is the NWFSC Slope survey every year to help
  # i.e., 1998 onewards has a survey every year
  # if (nwfsc) dd <- filter(dd, year >= 1995)
  # if (nwfsc) dd <- filter(dd, year >= 1989)

  domain <- fmesher::fm_nonconvex_hull_inla(
    as.matrix(dd[, c("UTM.lon", "UTM.lat")]),
    # concave = -0.07, convex = -0.05, resolution = c(200, 200)
    concave = -0.02, convex = -0.02, resolution = c(200, 200)
  )

  # same as coastwide
  min_edge <- 30
  # max_edge <- 55 # phi was estimating with gradient 0.002
  max_edge <- 45 # better convergence

  if (nwfsc) {
  # #   # # 35 +/- 20%
  # #   # min_edge <- 28
  # #   # max_edge <- 42
  # #   # # 38 +/- 20% #min est range of 150/4
    min_edge <- 30
    max_edge <- 45
  # #   # 45 +/- 20% #min est range of 135/3
  # #   min_edge <- 36
  # #   max_edge <- 54
  }

  if (bc) {
    # #20 +/- 20%
    min_edge <- 16
    max_edge <- 24
    # 25 +/- 20% #min est range of 95/4
    # min_edge <- 20
    # max_edge <- 30
    # # # 30 +/- 20% #min est range of 95/3
    # min_edge <- 24
    # max_edge <- 36
  }

  mesh3 <- fmesher::fm_mesh_2d_inla(
    loc = as.matrix(dd[,c("UTM.lon","UTM.lat")]),
    boundary = domain,
    max.edge = c(max_edge, 1000),
    # offset first value: does nothing with domain (vs. data)
    # offset second value: should be ~= range to avoid edge effects
    offset = c(10, 300), #
    cutoff = min_edge
  )

  if(hs){
    # much smaller domain needs different params
    domain <- fmesher::fm_nonconvex_hull_inla(
      as.matrix(dd[, c("UTM.lon", "UTM.lat")]),
      concave = -0.07, convex = -0.05, resolution = c(200, 200)
    )

    min_edge <- 12
    max_edge <- 16

    mesh3 <- fmesher::fm_mesh_2d_inla(
      loc = as.matrix(dd[,c("UTM.lon","UTM.lat")]),
      boundary = domain,
      max.edge = c(max_edge, 100),
      offset = c(10, 100),
      cutoff = min_edge
    )
  }

  mesh <- make_mesh(dd, c("UTM.lon", "UTM.lat"), mesh = mesh3)
  mesh$mesh$n

  ggplot() +
    geom_point(data = dd, aes(UTM.lon, UTM.lat),
               # colour = "blue",
               size = 0.5, alpha = 0.2, pch = 21) +
    inlabru::gg(mesh$mesh) +
    xlab("UTM (km)") + ylab("UTM (km)") + coord_fixed()

  ggsave(paste0("figs/trawl-model-mesh-", unique(dd$region),
                "-",min_edge, "-", max_edge, ".pdf"), width = 6, height = 6)
  ggsave(paste0("figs/trawl-model-mesh-", unique(dd$region),
                "-",min_edge, "-", max_edge, ".png"), width = 6, height = 6)

  if (nwfsc) {
    # dd$survey_name <- factor(dd$survey_name,
    #   levels = c("NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "NWFSC.Slope", "Triennial"))

    dd <- dd |> mutate(survey_name = ifelse(
      survey_name %in% c("NWFSC.Combo.pass1", "NWFSC.Combo.pass2"),
      "NWFSC.Combo", survey_name))

    dd$survey_name <- factor(dd$survey_name,
                             levels = c("NWFSC.Combo", "AFSC.Slope", "NWFSC.Slope", "Triennial"))
  }


  if (length(unique(dd$survey_name)) > 1) {
    # f <- catch_weight_t ~ survey_name + poly(log(depth_m), 2) + poly(julian_c, 2)
    # f <- catch_weight_t ~ survey_name + s(depth_m, julian_c)
    f <- catch_weight_t ~ survey_name + poly(log(depth_m), 2)*poly(julian_c, 2)
  } else {
    f <- catch_weight_t ~ poly(log(depth_m), 2) + poly(julian_c, 2)
  }
  table(dd$survey_name, dd$year)

  # set_family <- delta_lognormal_mix(type = "poisson-link")
  set_family <- delta_lognormal(type = "poisson-link")
  # if(bc) set_family <- delta_lognormal_mix(type = "poisson-link") # doesn't fit with fine mesh
  if(nwfsc) set_family <- delta_lognormal_mix(type = "poisson-link")

  set_spatial <- "on"
  if(hs) set_spatial <- "off"
  if(hs) f <- catch_weight_t ~ poly(log(depth_m), 2)

  if (bc) {
    # dd$survey_name <- factor(dd$survey_name,
    #   levels = c("syn bc", "msa bc"), labels = c("BC Synoptic", "MSA"))

    # can't estimate catchability or date effects for BC
    f <- catch_weight_t ~ poly(log(depth_m), 2)

    # needed if hs and bc combined
    set_spatial <- "off"
    dd$survey_name <- factor("BC Synoptic/MSA")
  }

  fit <- sdmTMB(
    formula = f,
    data = dd,
    time = "year",
    offset = "offset_km2",
    mesh = mesh,
    spatial = set_spatial,
    spatiotemporal = "rw",
    silent = FALSE,
    share_range = FALSE,
    extra_time = seq(min(dd$year), max(dd$year)),
    family = set_family,
    control = sdmTMBcontrol(
      start = list(logit_p_mix = qlogis(0.01)),
      map = list(logit_p_mix = factor(NA))
    ),
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = max_edge*3, sigma_lt = 2),
      matern_st = pc_matern(range_gt = max_edge*3, sigma_lt = 2)
    ),
  )
  sanity(fit)


  if(set_spatial != "off"){
  b1 <- tidy(fit, "ran_pars", model = 1)
  sigO1 <- b1$estimate[b1$term == "sigma_O"]
  sigO1se <- b1$std.error[b1$term == "sigma_O"]

  if (sigO1 < 0.01 || sigO1se / sigO1 > 3) { # sometimes collapses here
    fit <- update(fit, spatial = list("off", "on"))
  }

  b2 <- tidy(fit, "ran_pars", model = 2, conf.int = TRUE)
  sigO2 <- b2$estimate[b2$term == "sigma_O"]
  sigO2se <- b2$std.error[b2$term == "sigma_O"]
  sigO2lwr <- b2$conf.low[b2$term == "sigma_O"]

  if (sigO2 < 0.01 || sigO2se / sigO2 > 3 || sigO2lwr < 0.01) { # sometimes collapses here
    fit <- update(fit, spatial = list("on", "off"))
  }
  }

  s <- sanity(fit)

  if(!s$gradients_ok){
    fit <- run_extra_optimization(fit)
  }

  # sanity(fit)

  if (FALSE) {
    set.seed(1)
    s <- simulate(fit, 300L, type = "mle-mvn")
    dh <- dharma_residuals(s, fit, return_DHARMa = TRUE)
    plot(dh)
    DHARMa::testResiduals(dh)
  }

  if(hs){
    nd <- readRDS("output/prediction_grid_hs.rds")
    nd <- add_utm_columns(nd, units = "km", utm_crs = coast_crs) %>%
      rename("UTM.lon" = "X", "UTM.lat" = "Y") |>
      mutate(
        UTM.lon = round(UTM.lon, 3),
        X = UTM.lon, Y = UTM.lat,
        depth_m  = bot_depth
      )
    # years <- seq(min(dd$year), max(dd$year), 1)
    nd <- purrr::map_dfr(unique(dd$year), ~ tibble(nd, year = .x))
    # ggplot(nd) + geom_point(aes(X,Y)) + facet_wrap(~year)

  } else {
  nd <- grid
  nd <- filter(nd, region %in% dd$region)
  nd <- filter(nd, year %in% unique(dd$year))
  if (length(unique(dd$survey_name)) > 1) {
    nd$survey_name <-
      factor(levels(dd$survey_name)[1],
        levels = levels(dd$survey_name))
  }
  }

  nd$julian_c <- 0

  p <- predict(fit, newdata = nd, return_tmb_object = TRUE)
  ind <- get_index(p, bias_correct = TRUE, area = nd$area_km)
  list(index = ind, fit = fit, pred = p)
}

## For updating one model at a time
# dat2 <- filter(dat, region == "NWFSC")
# # dat2 <- filter(dat, region == "GOA")
# # dat2 <- filter(dat, region == "BC")
# #
# out <- split(dat2, dat2$region) |> lapply(fit_trawl_region)
# out2 <- out
# out <- readRDS("output/fit-trawl-by-region-lognormal-poisson-link-w-julian3.rds")
# out$NWFSC <- out2$NWFSC

out <- split(dat, dat$region) |> lapply(fit_trawl_region)
saveRDS(out, file = "output/fit-trawl-by-region-lognormal-poisson-link-w-julian-i.rds")
out <- readRDS(file = "output/fit-trawl-by-region-lognormal-poisson-link-w-julian-i.rds")

out$BC$fit
out$GOA$fit
out$NWFSC$fit


ind <- purrr::map_dfr(out, \(x) {
  ii <- x$ind
  ii$region <- x$pred$data$region[1]
  ii
})

# awkward temporary hack for HS, will update with something better
ind$subregion <- ind$region
# ind[ind$subregion=="HS",]$region <- "BC"
# ind[ind$subregion=="HS",]$subregion <- "Hecate (subregion)"
ind[ind$subregion!="HS",]$subregion <- "Region-specific"


coast_index <- readRDS("output/coast-index-trawl.rds")
coast_index$subregion <- "Region-specific"

aa <- bind_rows(
  mutate(ind, model = "Region-specific"),
  mutate(coast_index, model = "Combined")
)
aa$region <- gsub("Coast", "Coastwide", aa$region)
aa$region <- gsub("GOA", "Gulf of Alaska", aa$region)
aa$region <- gsub("BC", "British Columbia", aa$region)
aa$region <- gsub("NWFSC", "US West Coast", aa$region)
aa$region <- gsub("nwfsc", "US West Coast", aa$region)
row.names(aa) <- NULL

saveRDS(aa, "output/trawl-coast-indexes.rds")
