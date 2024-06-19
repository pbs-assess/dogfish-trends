library(dplyr)
library(ggplot2)
library(sdmTMB)
theme_set(ggsidekick::theme_sleek())
library(tictoc)
dir.create("figs", showWarnings = FALSE)

d <- readRDS("output/IPHC_coastdata.rds")
d$offset <- log(d$hooksobserved2) # no hook comp
# grid <- readRDS("output/PredictionGridCentres_IPHCcoast_regarea.rds") #full doesn't have updated projection
grid <- readRDS("output/PredictionGridCentres_IPHCcoast_regarea_trim.rds") #this one doesn't extend as far south in the NW US

grid$depth_m <- grid$bot_depth

test_resids_sim <- function(x, .n = 300) {
  s <- simulate(x, .n, type = "mle-mvn")
  dh <- dharma_residuals(s, x, return_DHARMa = TRUE)
  DHARMa::plotQQunif(dh, testUniformity = TRUE, testDispersion = FALSE, testOutliers = FALSE)
}

# custom mesh:
domain <- fmesher::fm_nonconvex_hull_inla(
  as.matrix(d[, c("UTM.lon", "UTM.lat")]),
  concave = -0.01, convex = -0.01, resolution = c(200, 200)
)
plot(domain)

# try what worked for trawl
# min_edge <- 30
min_edge <- 50
max_edge <- 55

mesh3 <- fmesher::fm_mesh_2d_inla(
  loc = as.matrix(d[,c("UTM.lon","UTM.lat")]),
  boundary = domain,
  # max.edge = c(130, 2000),
  max.edge = c(max_edge, 1000),
  offset = c(10, 300),
  cutoff = min_edge
)

mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), mesh = mesh3)
plot(mesh)
mesh$mesh$n

ggplot() +
  geom_point(data = d, aes(UTM.lon, UTM.lat), size = 0.5, alpha = 0.1, pch = 21) +
  inlabru::gg(mesh$mesh) +
  xlab("UTM (km)") + ylab("UTM (km)") + coord_fixed()

ggsave(paste0("figs/iphc-model-mesh-", min_edge,"-", max_edge,".pdf"), width = 6, height = 6)
ggsave("figs/iphc-model-mesh.pdf", width = 6, height = 6)
ggsave("figs/iphc-model-mesh.png", width = 6, height = 6)

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
    matern_s = pc_matern(range_gt = max_edge*3, sigma_lt = 2),
    matern_st = pc_matern(range_gt = max_edge*3, sigma_lt = 2)
  )
)
toc()

sanity(fit)

saveRDS(fit, file = paste0("output/fit-iphc-nb2-coastwide-",min_edge,"-", max_edge,".rds"))
# fit_old <- readRDS("output/fit-iphc-nb2-coastwide-old-mesh.rds")
fit <- readRDS(paste0("output/fit-iphc-nb2-coastwide-",min_edge,"-", max_edge,".rds"))

set.seed(1)
test_resids_sim(fit)

set.seed(42)
s <- simulate(fit, 400L, type = "mle-mvn")
r <- dharma_residuals(s, fit, plot = FALSE)
ggplot(r, aes(expected, observed)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(x = "Expected", y = "Observed") +
  coord_equal(expand = FALSE, xlim = c(0, 1), ylim = c(0, 1))
ggsave("figs/qq-iphc-main.png", width = 4, height = 4)

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

  fit4 <- update(
    fit,
    time_varying = ~ 1,
    time_varying_type = "rw0"
  )

  AIC(fit, fit4)

  test_resids_sim(fit4)
}
# conclusion (after a bunch of other delta and mixture modelling that is deleted
# is to go with NB2)

# predict by region and coastwide on grid: ----------------------------

head(grid)
regions <- list(
  "Coast" = c("2A", "2B", "2C", "3A", "3B"),
  "US West Coast" = c("2A"),
  "Gulf of Alaska" = c("2C", "3A", "3B"),
  "British Columbia" = c("2B")
)

gg <- select(as.data.frame(grid), area_km, depth_m, UTM.lon, UTM.lat, iphc_reg = ET_ID) |>
  distinct()
gc()
ret <- lapply(regions, \(r) {
  cat(r, "\n")
  nd <- filter(gg, iphc_reg %in% r)
  nd <- replicate_df(nd, "year", unique(fit$data$year))
  nd$obs_id <- NA
  pred <- predict(fit, newdata = nd, return_tmb_object = TRUE, re_form_iid = NA)
  ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
  gc()
  ind
})

out <- bind_rows(ret, .id = "region")

saveRDS(out, file = "output/indexes-iphc-nb2-coastwide.rds")
# out <- readRDS(file = "output/indexes-iphc-nb2-coastwide.rds")

g <- d |> filter(cpue > 0) |> ggplot(aes(UTM.lon, UTM.lat)) + geom_point(data = gg,mapping= aes(UTM.lon, UTM.lat), colour = "red") + geom_point() + facet_wrap(~year) + coord_fixed()
ggsave("figs/iphc-sets.png", width = 12, height = 12)
