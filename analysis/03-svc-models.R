library(dplyr)
library(ggplot2)
library(sdmTMB)
source("analysis/01-prep-overall-trawl.R")

dat_coast <- filter(dat, survey_name %in%
    c("syn bc", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "GOA")) |>
  filter(year >= 2003)

# coast SVC trawl model -----------------------------------------------------

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

tictoc::tic()

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
fit
sanity(fit)
rm(dat, mesh3, domain)
saveRDS(fit, file = "output/fit-trawl-svc-lognormal-mix.rds")
fit <- readRDS("output/fit-trawl-svc-lognormal-mix.rds")

set.seed(1)
s <- simulate(fit, 300L, type = "mle-mvn")
dh <- dharma_residuals(s, fit, return_DHARMa = TRUE)
DHARMa::plotQQunif(dh)

fit1 <- update(fit, spatiotemporal = "iid")
saveRDS(fit1, file = "output/fit-trawl-svc-iid-lognormal-mix.rds")
fit1 <- readRDS("output/fit-trawl-svc-iid-lognormal-mix.rds")

sanity(fit1)
fit1
AIC(fit, fit1)
set.seed(1)
s1 <- simulate(fit1, 300L, type = "mle-mvn")
dh1 <- dharma_residuals(s1, fit1, return_DHARMa = TRUE)
DHARMa::plotQQunif(dh1)

if (FALSE) {
  fit2 <- update(fit, spatiotemporal = "rw")
  fit2
  # SVC SD collapses
}

fit3 <- update(fit, spatiotemporal = "iid", family = delta_lognormal_mix(type = "poisson-link"))

fit4 <- update(fit, spatiotemporal = "off",
  family = delta_gamma(type = "poisson-link")
)
fit5 <- update(fit, spatiotemporal = "off",
  family = delta_gamma(type = "standard")
)
simulate(fit4, 100) |> dharma_residuals(fit4)
simulate(fit5, 100) |> dharma_residuals(fit5)

p4 <- predict(fit4, newdata = dat_coast, offset = rep(0, nrow(dat_coast)))
n <- exp(p4$est1)
w <- exp(p4$est2)
p <- 1 - exp(-n * exp(dat_coast$offset_km2))
r <- (n * w * exp(dat_coast$offset_km2)) / p

theta <- get_pars(fit4)
phi <- exp(theta$ln_phi[2])

sims <- matrix(nrow = nrow(p4), ncol = 100)
sims1 <- matrix(nrow = nrow(p4), ncol = 100)
sims2 <- matrix(nrow = nrow(p4), ncol = 100)
for (i in 1:100) {
  s1 <- rbinom(nrow(p4), size = 1, prob = p)
  s2 <- rgamma(nrow(p4), shape = phi, scale = r / phi)
  ss <- s1 * s2
  sims2[,i] <- s2
  sims1[,i] <- s1
  sims[,i] <- ss
}
attr(sims, "type") <- "mle-mvn"
dharma_residuals(sims, fit4)


s4 <- simulate(fit4, 100L, type = "mle-eb")
dharma_residuals(s4, fit4)

s5 <- simulate(fit4, 100L, type = "mle-eb")
s51 <- simulate(fit4, 100L, type = "mle-eb", model = 1)
s52 <- simulate(fit4, 100L, type = "mle-eb", model = 2)

mean(s51)
mean(sims1)

mean(s52)
mean(sims2)

mean(sims)
mean(s5)
mean(dat_coast$catch_weight_t)




p5 <- predict(fit5, offset = dat_coast$offset_km2)
p <- plogis(p5$est1)
r <- exp(p5$est2)
theta <- get_pars(fit5)
phi <- exp(theta$ln_phi[2])
sims <- matrix(nrow = nrow(p5), ncol = 100)
sims1 <- matrix(nrow = nrow(p5), ncol = 100)
sims2 <- matrix(nrow = nrow(p5), ncol = 100)
for (i in 1:100) {
  s1 <- rbinom(nrow(p4), size = 1, prob = p)
  s2 <- rgamma(nrow(p4), shape = phi, scale = r / phi)
  ss <- s1 * s2
  sims2[,i] <- s2
  sims1[,i] <- s1
  sims[,i] <- ss
}
attr(sims, "type") <- "mle-mvn"
dharma_residuals(sims, fit5)

s5 <- simulate(fit5, 100L, type = "mle-eb")
s51 <- simulate(fit5, 100L, type = "mle-eb", model = 1)
s52 <- simulate(fit5, 100L, type = "mle-eb", model = 2)

mean(s51)
mean(sims1)

mean(s52)
mean(sims2)

mean(sims)
mean(s5)
mean(dat_coast$catch_weight_t)

dharma_residuals(s5, fit5)

# works!?
a <- subset(pcod, density > 0)
m <- sdmTMB(density ~ 1, family = Gamma(link = "log"), data = a, spatial = "off")
m
pp <- predict(m)
mu <- exp(pp$est)
theta <- get_pars(m)
phi <- exp(theta$ln_phi)
r <- rgamma(nrow(pp), shape = phi, scale = mu / phi)
s <- simulate(m, 1)
mean(r)
mean(s)
mean(a$density)

plot(s3[,1], dat_coast$catch_weight_t)

dh3 <- dharma_residuals(s3, fit4, return_DHARMa = TRUE)
DHARMa::plotQQunif(dh3)
AIC(fit1, fit3)

nd <- filter(grid, year %in% dat_coast$year)
p <- predict(fit1, newdata = nd)

b <- coef(fit1)
b[['year_scaled']]

p |>
  ggplot(aes(UTM.lon, UTM.lat, fill = b[['year_scaled']] + zeta_s_year_scaled1)) +
  geom_raster() +
  scale_fill_gradient2()
