library(dplyr)
library(sdmTMB)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())
source("analysis/999-prep-overall-trawl.R")
source("analysis/999-colours-etc.R")


## fit_iphc <- readRDS("output/fit-iphc-nb2-coastwide-30-55.rds")
# fit_iphc <- readRDS("output/fit-iphc-nb2-coastwide-50-55.rds") # se on phi smaller
#
# tidy(fit_iphc, conf.int = TRUE)
# tidy(fit_iphc, "ran_pars", conf.int = TRUE)


# load trawl models ---------------------------------------------------------

# fit_reg <- readRDS("output/fit-trawl-by-region-lognormal-mix-poisson-link.rds")
# fit_reg <- readRDS("output/fit-trawl-by-region-lognormal-poisson-link-NW-mix.rds")
# fit_reg <- readRDS("output/fit-trawl-by-region-lognormal-poisson-link-w-julian3.rds")

fit_reg <- readRDS("output/fit-trawl-by-region-lognormal-poisson-link-w-julian-i.rds")

fit_coast <- readRDS("output/fit-trawl-coast-lognormal-mix-poisson-link-30-55.rds")

# look at regional fixed effect coefficients ------------------------------------------

purrr::map_dfr(fit_reg, \(x) tidy(x$fit, model = 1, conf.int = TRUE), .id = "region")
purrr::map_dfr(fit_reg, \(x) tidy(x$fit, model = 2, conf.int = TRUE), .id = "region")

# random param table ---------------------------------------------------------------

purrr::walk(fit_reg, \(x) sanity(x$fit))
purrr::map_dfr(fit_reg, \(x) tidy(x$fit, "ran_pars", model = 1, conf.int = TRUE), .id = "region")
purrr::map_dfr(fit_reg, \(x) tidy(x$fit, "ran_pars", model = 2, conf.int = TRUE), .id = "region")

fits <- c(fit_reg, list(fit_coast))
names(fits) <- c(names(fit_reg), "Coast")

coefs <- purrr::map_dfr(seq_along(fits), function(i) {
  x <- fits[[i]]
  if (length(fits[[i]]) > 3) {
    x1 <- tidy(x, "ran_pars", model = 1, conf.int = TRUE)
    x2 <- tidy(x, "ran_pars", model = 2, conf.int = TRUE)
  } else {
    x1 <- tidy(x$fit, "ran_pars", model = 1, conf.int = TRUE)
    x2 <- tidy(x$fit, "ran_pars", model = 2, conf.int = TRUE)
  }

  if (sum(grepl("range", x1$term)) == 2L) {
    x1$term[1] <- "spatial range"
    x1$term[2] <- "spatiotemporal range"
  } else {
    x1$term[1] <- "spatiotemporal range"
  }

  if (sum(grepl("range", x2$term)) == 2L) {
    x2$term[1] <- "spatial range"
    x2$term[2] <- "spatiotemporal range"
  } else {
    x2$term[1] <- "spatiotemporal range"
  }
  x1$linear_predictor <- 1
  x2$linear_predictor <- 2
  x <- bind_rows(list(x1, x2)) |> select(-std.error)
  x$region <- names(fits)[i]
  x
})

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

coefs <- clean_region_names(coefs)

coefs |>
  mutate(term = gsub("sigma_E", "spatiotemporal SD", term)) |>
  mutate(term = gsub("sigma_O", "spatial SD", term)) |>
  mutate(term = gsub("phi", "lognormal SD", term)) |>
  mutate(term = firstup(term)) |>
  ggplot(aes(estimate,
    y = region, xmin = conf.low, xmax = conf.high,
    colour = as.factor(linear_predictor)
  )) +
  geom_pointrange(position = position_dodge(width = 0.2), pch = 21) +
  facet_wrap(~term, scales = "free_x", nrow = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Estimate", y = "Region", colour = "Linear predictor") +
  ggsidekick::theme_sleek()
ggsave("figs/coefs-w-julian.pdf", width = 8, height = 5)

# depth plots ---------------------------------------------------------------

dd <- c(
  seq(min(dat$depth_m), 400, length.out = 110),
  seq(400, 1000, length.out = 50)
)
nd <- data.frame(depth_m = dd, year = 2003L, julian_c = 0)


ret <- purrr::map_dfr(seq_along(fits), function(i) {
  cat(i, "\n")
  x <- fits[[i]]
  if (length(fits[[i]]) == 3) {
    x <- x$fit
  }
  if ("survey_name" %in% names(fits[[i]]$pred$data)) {
    nd$survey_name <- fits[[i]]$pred$data$survey_name[1]
  }
  pp <- predict(x, newdata = nd, re_form = NA, se_fit = TRUE)
  pp$region <- names(fits)[i]
  pp
})

ret$date  <- "Summer solstice"

# add fall equinox for NWFSC
nd2 <- nd
nd2$julian_c = 265 - 172
nd2$date <- "Fall equinox"
nd2$survey_name <- fits[[3]]$pred$data$survey_name[1]
pp <- predict(fits[[3]]$fit, newdata = nd2, re_form = NA, se_fit = TRUE)
pp$region <- names(fits)[3]
ret2 <- pp

dd <- ret |>
  bind_rows(ret2) |>
  clean_region_names() |>
  group_by(region, date) |>
  mutate(est = log(exp(est) / max(exp(est))),
         date = factor(date, levels = c("Summer solstice","Fall equinox"))
         )

dd |>  ggplot(aes(depth_m, exp(est),
    colour = region, fill = region,
    ymin = exp(est - 2 * est_se),
    ymax = exp(est + 2 * est_se),
  )) +
  # geom_ribbon(alpha = 0.1, colour = NA) +
  geom_ribbon(data = filter(dd, date == "Summer solstice"), alpha = 0.1, colour = NA) +
  geom_ribbon(data = filter(dd, date == "Fall equinox"), alpha = 0.1, colour = NA) +
  geom_line(aes(linetype = date)) +
  ggsidekick::theme_sleek() +
  coord_cartesian(ylim = c(0, 2.2), expand = FALSE, xlim = c(min(dat$depth_m), 750)) +
  scale_colour_manual(values = cols_region) +
  scale_fill_manual(values = cols_region) +
  labs(y = "Standardized depth effect", x = "Depth (m)",
       colour = "Region", fill = "Region",
       linetype = "Season") +
  theme(legend.position.inside = c(0.8, 0.7), legend.position = "inside")

ggsave("figs/depth-effects-i2.pdf", width = 5, height = 4)
ggsave("figs/depth-effects-i2.png", width = 5, height = 4)



# date effect plots ---------------------------------------------------------------


dat$region <- ""
dat$region[dat$survey_name %in%
             c("NWFSC.Combo.pass1", "NWFSC.Combo.pass2",
               "AFSC.Slope", "NWFSC.Slope", "Triennial")] <- "NWFSC"
dat$region[dat$survey_name %in% c("GOA")] <- "GOA"
dat$region[dat$survey_name %in% c("syn bc")] <- "BC"
dat$region[dat$survey_name %in% c("msa bc")] <- "BC"

# dat2 <- filter(dat, region == "NWFSC")

dd <- c(
  seq(min(dat$julian), max(dat$julian), length.out = 80)
)
nd <- data.frame(julian = dd,
                 year = 2003L,
                 depth_m = 100)

nd$julian_c  <- nd$julian - 172
nd$depth_strata  <- "100 m"
nd2 <- nd
nd2$depth_m <- 300
nd2$depth_strata  <- "300 m"
nd <- bind_rows(nd, nd2)

# remove BC as julian date was not in that model
fits <- fit_reg[2:3]
ret <- purrr::map_dfr(seq_along(fits), function(i) {
  cat(i, "\n")
  x <- fits[[i]]
  if (length(fits[[i]]) == 3) {
    x <- x$fit
  }
  if ("survey_name" %in% names(fits[[i]]$pred$data)) {
    nd$survey_name <- fits[[i]]$pred$data$survey_name[1]
  }
  pp <- predict(x, newdata = nd, re_form = NA, se_fit = TRUE)
  pp$region <- names(fits)[i]
  pp
})


date_ranges <- dat |> group_by(region) |>
  summarise(min = min(julian),
            max = max(julian)) |>
  clean_region_names()

# date_ranges[date_ranges$region == "NWFSC",]

dd <- ret |>
  clean_region_names() |>
  left_join(date_ranges) |>
  filter(julian < max, julian > min) |>
  group_by(region) |>
  mutate(est = log(exp(est) / max(exp(est)))) |>
  filter(region != "British Columbia")

  dd |> ggplot(aes(julian, exp(est),
             colour = region, fill = region,
             ymin = exp(est - 2 * est_se),
             ymax = exp(est + 2 * est_se),
  )) +
  geom_ribbon(data = filter(dd, depth_strata == "100 m"), alpha = 0.1, colour = NA) +
    geom_ribbon(data = filter(dd, depth_strata == "300 m"), alpha = 0.1, colour = NA) +
  geom_line(aes(linetype = depth_strata)) +
  ggsidekick::theme_sleek() +
  coord_cartesian(ylim = c(0, 1), expand = FALSE, xlim = c(min(nd$julian),max(nd$julian))) +
  scale_colour_manual(values = cols_region) +
  scale_fill_manual(values = cols_region) +
  labs(y = "Standardized julian effect", x = "Julian date",
       linetype = "Depth",
       colour = "Region", fill = "Region") +
  theme(legend.position.inside = c(0.75, 0.7), legend.position = "inside")

ggsave("figs/date-effects-i.pdf", width = 5, height = 4)
ggsave("figs/date-effects-i.png", width = 5, height = 4)



