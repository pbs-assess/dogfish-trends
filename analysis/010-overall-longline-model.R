library(dplyr)
library(ggplot2)
library(sdmTMB)
theme_set(ggsidekick::theme_sleek())
library(tictoc)

d <- readRDS("output/IPHC_coastdata.rds")
d$offset <- log(d$hooksobserved2) # no hook comp
grid <- readRDS("output/PredictionGridCentres_IPHCcoast_regarea.rds")
grid$depth_m <- grid$bot_depth

test_resids_sim <- function(x, .n = 300) {
  s <- simulate(x, .n, type = "mle-mvn")
  dh <- dharma_residuals(s, x, return_DHARMa = TRUE)
  DHARMa::plotQQunif(dh, testUniformity = TRUE, testDispersion = FALSE, testOutliers = FALSE)
}

# custom mesh:
domain <- fmesher::fm_nonconvex_hull_inla(
  as.matrix(d[, c("UTM.lon", "UTM.lat")]),
  concave = -0.07, convex = -0.05, resolution = c(200, 200)
)
plot(domain)
mesh3 <- fmesher::fm_mesh_2d_inla(
  boundary = domain,
  max.edge = c(130, 2000),
  offset = c(130, 300),
  cutoff = 40
)
mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), mesh = mesh3)
plot(mesh)
mesh$mesh$n

tic()
fit <- sdmTMB(
  formula = number.observed ~ poly(log(depth_m), 2),
  data = d,
  time = "year",
  offset = "offset",
  mesh = mesh,
  spatial = "on",
  spatiotemporal = "rw",
  family = nbinom2(),
  silent = FALSE,
  share_range = FALSE,
  priors = sdmTMBpriors(
    matern_s = pc_matern(range_gt = 250, sigma_lt = 2),
    matern_st = pc_matern(range_gt = 250, sigma_lt = 2)
  )
)
toc()

saveRDS(fit, file = "output/fit-iphc-nb2-coastwide.rds")
fit <- readRDS("output/fit-iphc-nb2-coastwide.rds")

set.seed(1)
test_resids_sim(fit)

r1 <- residuals(fit, type = "mle-mvn")
qqnorm(r1);abline(0, 1)
ks.test(r1, pnorm)

s <- simulate(fit, nsim = 300)
hist(apply(s, 2, sd), breaks = 100)
sd(d$number.observed)
abline(v = sd(d$number.observed), col = "red", lwd = 2)

if (FALSE) {
  # tic()
  # fit2 <- update(fit, family = nbinom1())
  # tictoc::toc()
  # r2 <- residuals(fit2, type = "mle-mvn")
  # qqnorm(r2);abline(0, 1)
  # test_resids_sim(fit2)

  tic()
  d$obs_id <- factor(seq_len(nrow(d)))
  fit3 <- update(
    fit,
    family = poisson(), #<
    formula. = number.observed ~ poly(log(depth_m), 2) + (1 | obs_id) #<
  )
  toc()
  set.seed(1)
  r3 <- residuals(fit3, type = "mle-mvn")
  qqnorm(r3);abline(0, 1)

  test_resids_sim(fit3)

  s <- simulate(fit3, nsim = 300)
  hist(apply(s, 2, sd), breaks = 100)
  sd(d$number.observed)
  abline(v = sd(d$number.observed), col = "red", lwd = 2)

  AIC(fit, fit2, fit3) |>
    as.data.frame() |>
    arrange(AIC)

  logLik(fit)
  logLik(fit3)
}
# conclusion (after a bunch of other delta and mixture modelling that is deleted
# is to go with NB2)

# predict by region and coastwide on grid: ----------------------------

head(grid)
regions <- list(
  "Coast" = c("2A", "2B", "2C", "3A", "3B"),
  "US West Coast" = c("2A"),
  "Gulf of Alaska" = c("3A", "3B"),
  "British Columbia" = c("2B", "2C")
)

gg <- select(as.data.frame(grid), area_km, depth_m, UTM.lon, UTM.lat, iphc_reg = ET_ID) |>
  distinct()
gc()
ret3 <- lapply(regions, \(r) {
  cat(r, "\n")
  nd <- filter(gg, iphc_reg %in% r)
  nd <- replicate_df(nd, "year", unique(fit$data$year))
  nd$obs_id <- NA
  pred <- predict(fit3, newdata = nd, return_tmb_object = TRUE, re_form_iid = NA)
  ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
  gc()
  ind
})

out <- bind_rows(ret, .id = "region")

saveRDS(out, file = "output/indexes-iphc-nb2-coastwide.rds")
out <- readRDS("output/indexes-iphc-nb2-coastwide.rds")

ggplot(out, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_ribbon() +
  geom_line() +
  facet_wrap(~region, scales = "free_y")



