library(dplyr)
library(sdmTMB)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())
source("analysis/999-prep-overall-trawl.R")
source("analysis/999-colours-etc.R")

# param table ---------------------------------------------------------------

fit_reg <- readRDS("output/fit-trawl-by-region-lognormal-mix-poisson-link.rds")
fit_coast <- readRDS("output/fit-trawl-coast-lognormal-mix.rds")

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
  x1$term[1] <- "spatial range"
  x1$term[2] <- "spatiotemporal range"
  x2$term[1] <- "spatial range"
  x2$term[2] <- "spatiotemporal range"
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
ggsave("figs/coefs.pdf", width = 8, height = 5)

# depth plots ---------------------------------------------------------------

dd <- c(
  seq(min(dat$depth_m), 400, length.out = 110),
  seq(400, 1000, length.out = 50)
)
nd <- data.frame(depth_m = dd, year = 2003L)

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

ret |>
  clean_region_names() |>
  group_by(region) |>
  mutate(est = log(exp(est) / max(exp(est)))) |>
  ggplot(aes(depth_m, exp(est),
    colour = region, fill = region,
    ymin = exp(est - 2 * est_se),
    ymax = exp(est + 2 * est_se),
  )) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line() +
  ggsidekick::theme_sleek() +
  coord_cartesian(ylim = c(0, 2.2), expand = FALSE, xlim = c(min(dat$depth_m), 750)) +
  scale_colour_manual(values = cols_region) +
  scale_fill_manual(values = cols_region) +
  labs(y = "Standardized depth effect", x = "Depth (m)", colour = "Region", fill = "Region") +
  theme(legend.position.inside = c(0.8, 0.8), legend.position = "inside")

ggsave("figs/depth-effects.pdf", width = 5, height = 4)
ggsave("figs/depth-effects.png", width = 5, height = 4)
