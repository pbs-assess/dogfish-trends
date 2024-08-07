library(sdmTMB)
library(dplyr)
library(ggplot2)
library(tictoc)

# data prep -----------------------------------------------------------------
source("data-prep/00-set-crs.R")
source("analysis/999-prep-overall-trawl.R")
rm(dat) # only keep 'grid'
source("analysis/999-prep-maturity-split-data.R")
d <- prep_maturity_split_data()

ggplot(d, aes(X, Y, colour = survey_name)) + geom_point() +
  facet_wrap(~lengthgroup)
table(d$survey_name, d$lengthgroup)

group_by(d, survey_name, lengthgroup) |>
  summarise(npos = mean(catch_weight > 0))

# mesh design ---------------------------------------------------------------

example_dat <- filter(d, lengthgroup == "imm")
domain <- fmesher::fm_nonconvex_hull_inla(
  as.matrix(example_dat[, c("X", "Y")]),
  concave = -0.01, convex = -0.01, resolution = c(200, 200)
)

## used for overall trawl
# min_edge <- 30
# max_edge <- 55
# ## drop resolution
# min_edge <- 40
min_edge <- 50
max_edge <- 55

mesh3 <- fmesher::fm_mesh_2d_inla(
  loc = as.matrix(example_dat[,c("X", "Y")]),
  boundary = domain,
  max.edge = c(max_edge, 1000),
  offset = c(10, 300),
  cutoff = min_edge
)
mesh <- make_mesh(example_dat, c("X", "Y"), mesh = mesh3)
plot(mesh)
mesh$mesh$n

# model fit -----------------------------------------------------------------

fits <- list()
indexes <- list()
groups <- sort(unique(d$lengthgroup))

for (i in seq_along(groups)) {
  this_group <- groups[i]
  cat("Fitting:", this_group, "\n")

  dd <- filter(d, lengthgroup == this_group)
  this_mesh <- make_mesh(dd, c("X", "Y"), mesh = mesh$mesh) # in case

  fit <- sdmTMB(
    formula = catch_weight_t ~ 1 + poly(log(depth_m), 2),
    data = dd,
    time = "year",
    offset = "offset_km2",
    # time_varying = ~ 1 + poly(log(depth_m), 2),
    # time_varying_type = "rw0",
    mesh = this_mesh,
    spatial = "on",
    spatiotemporal = "rw",
    extra_time = seq(min(dd$year), max(dd$year)),
    family = delta_lognormal(type = "poisson-link"),
    control = sdmTMBcontrol(
      start = list(logit_p_mix = qlogis(0.01), log_ratio_mix = -1),
      map = list(logit_p_mix = factor(NA))
    ),
    silent = TRUE,
    share_range = FALSE,
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = max_edge*3, sigma_lt = 2),
      matern_st = pc_matern(range_gt = max_edge*3, sigma_lt = 2)
    ),
  )

  # check if spatial SDs collapsed; if so fix at zero:
  b1 <- tidy(fit, effects = 'ran_pars', model = 1, se.fit = TRUE)
  b2 <- tidy(fit, effects = 'ran_pars', model = 2, se.fit = TRUE)
  .spatial <- list("on", "on")
  do_refit <- FALSE
  if (b1$estimate[b1$term == "sigma_O"] < 0.01 || is.na(b1$std.error[b1$term == "sigma_O"])) {
    .spatial[[1]] <- "off"
    do_refit <- TRUE
  }
  if (b2$estimate[b2$term == "sigma_O"] < 0.01 || is.na(b2$std.error[b2$term == "sigma_O"])) {
    .spatial[[2]] <- "off"
    do_refit <- TRUE
  }
  if (do_refit) {
    fit <- update(fit, spatial = .spatial)
  }
  print(fit)
  sanity(fit)

  # predict coast wide -------------------------------------------------------

  cat("Integrating index coastwide\n")

  # chunk years to keep memory use down:
  yrs <- sort(unique(dd$year))
  yy <- sdmTMB:::chunk_time(yrs, 2)
  run_index_coastwide <- function(y) {
    cat(y, "\n")
    nd <- dplyr::filter(grid, year %in% y)
    pred <- predict(fit, newdata = nd, return_tmb_object = TRUE)
    ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
    gc()
    ind
  }
  index_l <- lapply(yy, run_index_coastwide)
  index <- do.call(rbind, index_l)
  if (FALSE) {
    ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) + geom_line() +
      geom_ribbon(alpha = 0.2)
  }

  # predict by region -------------------------------------------------------

  cat("Integrating index by region\n")

  regions <- unique(grid$region)
  run_index_by_region <- function(r) {
    cat(r, "\n")
    nd <- dplyr::filter(grid, region %in% r, year %in% dd$year)
    pred <- predict(fit, newdata = nd, return_tmb_object = TRUE)
    ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
    ind$region <- r
    gc()
    ind
  }
  index_reg_l <- lapply(regions, run_index_by_region)
  index_reg <- do.call(rbind, index_reg_l)
  ind <- bind_rows(mutate(index, region = "Coast"), index_reg)

  indexes[[i]] <- ind
  fits[[i]] <- fit
}

names(indexes) <- groups
indexes <- bind_rows(indexes, .id = "group")
row.names(indexes) <- NULL
glimpse(indexes)
saveRDS(indexes, file = "output/index-trawl-by-maturity-poisson-link.rds")
saveRDS(fits, file = "output/fit-trawl-by-maturity-poisson-link.rds")

# # SVC model by maturity: --------------------------------
#
# d$decade <- (d$year - 2010) / 10
# grid$decade <- 0
#
# fits_svc <- list()
# predictions_svc <- list()
#
# for (i in seq_along(groups)) {
#   this_group <- groups[i]
#   cat("Fitting:", this_group, "\n")
#   dd <- filter(d, lengthgroup == this_group)
#   this_mesh <- make_mesh(dd, c("X", "Y"), mesh = mesh$mesh) # in case
#   fit <- sdmTMB(
#     formula = catch_weight_t ~ 1 + poly(log(depth_m), 2) + decade,
#     spatial_varying = ~ 0 + decade,
#     data = dd,
#     time = "year",
#     offset = "offset_km2",
#     mesh = this_mesh,
#     spatial = "on",
#     spatiotemporal = "off",
#     family = delta_lognormal(type = "poisson-link"),
#     silent = TRUE,
#     share_range = FALSE,
#     priors = sdmTMBpriors(
#       matern_s = pc_matern(range_gt = 250, sigma_lt = 2),
#       matern_st = pc_matern(range_gt = 250, sigma_lt = 2)
#     ),
#   )
#   print(fit)
#   s <- sanity(fit)
#   if (!s$all_ok) {
#     fit <- update(fit, family = delta_gamma(type = "poisson-link"))
#   }
#
#   # predict coast wide -------------------------------------------------------
#
#   # pick any year:
#   p <- predict(fit, newdata = filter(grid, year == max(grid$year)))
#
#   b1 <- tidy(fit)
#   b2 <- tidy(fit, model = 2)
#   z1 <- b1$estimate[b1$term == "decade"]
#   z2 <- b2$estimate[b1$term == "decade"]
#
#   p$svc <- z1 + z2 + p$zeta_s_decade1 + p$zeta_s_decade2
#   predictions_svc[[i]] <- p
#   fits_svc[[i]] <- fit
# }
#
# lapply(fits_svc, \(x) x$family$clean_name)
# lapply(fits_svc, print)
# groups
# names(predictions_svc) <- groups
# names(fits_svc) <- groups
#
# saveRDS(predictions_svc, file = "output/predictions-trawl-svc-by-maturity-poisson-link.rds")
# saveRDS(fits_svc, file = "output/fit-trawl-svc-by-maturity-poisson-link.rds")
