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
table(dat_coast$survey_name,  dat_coast$year)

# coast trawl model ---------------------------------------------------------

# custom mesh:
domain <- fmesher::fm_nonconvex_hull_inla(
  as.matrix(dat_coast[, c("UTM.lon", "UTM.lat")]),
  # concave = -0.07, convex = -0.05, resolution = c(200, 200)
  concave = -0.01, convex = -0.01, resolution = c(200, 200)
)
plot(domain)

## started with these: fit with regular lognormal_mix, but not with poisson-link
# min_edge <- 40 #try this as model doesn't converge
# max_edge <- 60
#
## so tried a finer mesh: fit for everything!
min_edge <- 30
max_edge <- 45

if (file.exists("output/mesh-trawl-overall.rds")) {
  # ensure consistent knot locations across platforms:
  mesh3 <- readRDS("output/mesh-trawl-overall.rds")
} else {
  mesh3 <- fmesher::fm_mesh_2d_inla(
    loc = as.matrix(dat_coast[,c("UTM.lon","UTM.lat")]),
    boundary = domain,
    # max.edge = c(150, 2000),
    max.edge = c(max_edge, 1000),
    offset = c(10, 300),
    cutoff = min_edge
  )
  saveRDS(mesh3, "output/mesh-trawl-overall.rds")
}

mesh <- make_mesh(dat_coast, c("UTM.lon", "UTM.lat"), mesh = mesh3)
# mesh <- make_mesh(dat_coast, c("UTM.lon", "UTM.lat"), cutoff = 50)
mesh$mesh$n

# out <- INLA::inla.mesh.assessment(mesh3, spatial.range = 200, alpha = 2)
# ggplot() + inlabru::gg(out, aes(color = sd.dev)) + coord_equal() +
#   scale_color_gradient(limits = range(out$sd.dev, na.rm = TRUE))


ggplot() +
  geom_point(data = dat_coast |> arrange(year), aes(UTM.lon, UTM.lat
             , colour = year), size = 0.25, alpha = 0.5,
             # ), size = 0.25, alpha = 0.25,
             pch = 16) +
  inlabru::gg(mesh$mesh,
              edge.color = "grey80",
              edge.linewidth = 0.15,
              # interior = TRUE,
              # int.color = "blue",
              int.linewidth = 0.25,
              exterior = FALSE,
              # ext.color = "black",
              ext.linewidth = 0.5) +
  scale_colour_viridis_c(direction = -1) +
  labs(colour = "Year") +
  xlab("UTM (km)") + ylab("UTM (km)") + coord_fixed(expand = FALSE) +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.25))
ggsave(paste0("figs/trawl-model-mesh-",min_edge,"-", max_edge,".pdf"), width = 6, height = 6)
ggsave("figs/trawl-model-mesh.png", width = 6, height = 6)

ggplot(dat_coast) +
  geom_histogram(aes(log(catch_weight_t)))

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
    matern_s = pc_matern(range_gt = max_edge*3, sigma_lt = 2),
    matern_st = pc_matern(range_gt = max_edge*3, sigma_lt = 2)
  ),
)
tictoc::toc()
fit1
sanity(fit1)
rm(dat, mesh3, domain)
saveRDS(fit1, file = paste0("output/fit-trawl-coast-lognormal-mix-",min_edge,"-", max_edge,".rds"))
fit1 <- readRDS(paste0("output/fit-trawl-coast-lognormal-mix-",min_edge,"-", max_edge,".rds"))

p1 <- get_pars(fit1)
plogis(p1$logit_p_mix)
1 + exp(p1$log_ratio_mix)
fit1$sd_report

set.seed(1)
s1 <- simulate(fit1, 500L, type = "mle-mvn")

dh1 <- dharma_residuals(s1, fit1, return_DHARMa = TRUE)
plot(dh1)
DHARMa::plotQQunif(dh1)

tictoc::tic()
fit4 <- update(fit1, family = delta_lognormal_mix(type = "poisson-link"))
tictoc::toc()

saveRDS(fit4, file = paste0("output/fit-trawl-coast-lognormal-mix-poisson-link-",min_edge,"-", max_edge,".rds"))
fit4 <- readRDS(paste0("output/fit-trawl-coast-lognormal-mix-poisson-link-",min_edge,"-", max_edge,".rds"))


sanity(fit4)
AIC(fit1, fit4)

set.seed(123)
s4 <- simulate(fit4, 400L, type = "mle-mvn")

r <- dharma_residuals(s4, fit4, plot = FALSE)
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
    matern_s = pc_matern(range_gt = max_edge*3, sigma_lt = 2),
    matern_st = pc_matern(range_gt = max_edge*3, sigma_lt = 2),
    b = normal(c(NA, NA, NA, 0, 0, 0), c(NA, NA, NA, 1, 1, 1))
  ),
)

# rm(fit1, fit4)

saveRDS(fitq, paste0("output/fit-trawl-coast-lognormal-mix-poisson-link-q-",min_edge,"-", max_edge,".rds"))
fitq <- readRDS(paste0("output/fit-trawl-coast-lognormal-mix-poisson-link-q-",min_edge,"-", max_edge,".rds"))
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
saveRDS(index, "output/index_l.rds")
index <- readRDS("output/index_l.rds")
# eao_l <- lapply(yy, \(y) {
#   cat(y, "\n")
#   nd <- dplyr::filter(grid, year %in% y)
#   pred <- predict(fit, newdata = nd, return_tmb_object = TRUE)
#   eao <- get_eao(pred, bias_correct = FALSE, area = nd$area_km)
#   gc()
#   eao
# })
# eao <- do.call(rbind, eao_l)
# ggplot(eao, aes(year, est, ymin = lwr, ymax = upr)) + geom_ribbon(fill = "grey60") + geom_line()

yrs <- unique(dat_coast$year)
yy <- chunk_years(yrs, 2)
yy
index_l <- lapply(yy, \(y) {
  cat(y, "\n")
  nd <- dplyr::filter(grid, year %in% y)
  pred <- predict(fitq, newdata = nd, return_tmb_object = TRUE)
  ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
  gc()
  ind
})
indexq <- do.call(rbind, index_l)

# apply coast model to regions ----------------------------------------------

# Alaska fairly hard on memory, but still OK ~max 20 GB
regions <- unique(grid$region)

gc()

do_expanions <- function(model, type = c("index", "eao")) {
  type <- match.arg(type)
  lapply(regions, \(r) {
    cat(r, "\n")
    nd <- dplyr::filter(grid, region %in% r, year %in% unique(dat_coast$year))
    pred <- predict(model, newdata = nd, return_tmb_object = TRUE)
    if (type == "index") {
      ind <- get_index(pred, bias_correct = TRUE, area = nd$area_km)
    } else {
      ind <- get_eao(pred, bias_correct = FALSE, area = nd$area_km)
    }
    ind$region <- r
    gc()
    ind
  })
}
index_reg_l <- do_expanions(fit)
index_reg_lq <- do_expanions(fitq)
# eao_reg_l <- do_expanions(fit, type = "eao")

index_reg <- do.call(rbind, index_reg_l)
index_regq <- do.call(rbind, index_reg_lq)
# eao_reg <- do.call(rbind, eao_reg_l)

# ggplot(eao_reg, aes(year, est, ymin = lwr, ymax = upr)) + geom_ribbon(fill = "grey60") +
#   geom_line() +
#   facet_wrap(~region, scales = "free_y") +
#   scale_y_log10()

bind_rows(
  mutate(index, model = "No catchability effects", region = "Coastwide"),
  mutate(indexq, model = "With catchability effects", region = "Coastwide"),
  mutate(index_reg, model = "No catchability effects"),
  mutate(index_regq, model = "With catchability effects")
) |>
  # ggplot() + geom_line(aes(year, est, colour = model)) +
  # geom_ribbon(aes(year, ymin = lwr, ymax = upr, fill = model), alpha = 0.2) +
  # facet_wrap(~region)
  saveRDS("output/trawl-indexes-with-catchability.rds")

ind <- bind_rows(mutate(index, region = "Coast"), index_reg)
# ind$region <- factor(ind$region, levels = c("Coast", "GOA", "BC", "NWFSC"))

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
saveRDS(coast_index, "output/coast-index-trawl.rds")
