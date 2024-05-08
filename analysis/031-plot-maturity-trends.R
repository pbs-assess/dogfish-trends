library(ggplot2)
library(dplyr)
source("analysis/999-colours-etc.R")
indexes <- readRDS("output/index-trawl-by-maturity-poisson-link.rds")
# indexes <- readRDS("output/index-trawl-by-maturity-poisson-link-tv-pe.rds")
# indexes <- readRDS("output/index-trawl-by-maturity-poisson-link.rds")
# indexes <- readRDS("output/index-trawl-by-maturity-poisson-link-gamma.rds")
# indexes <- readRDS("output/index-trawl-by-maturity-poisson-link-gamma-pe.rds")

# indexes <- readRDS("output/index-trawl-by-maturity-poisson-link-gengamma.rds")
# indexes <- readRDS("output/index-trawl-by-maturity-poisson-link-gengamma-pe.rds")

# ---------------
# indexes <- readRDS("output/index-trawl-by-maturity-poisson-link-gengamma-pe2.rds")
# ----------------

indexes$region <- factor(indexes$region, levels = c("Coast", "GOA", "BC", "NWFSC"))

# # fits1 <- readRDS("output/fit-trawl-by-maturity-poisson-link.rds")
# fits2 <- readRDS("output/fit-trawl-by-maturity-poisson-link-gamma-pe.rds")
# #
# # fits1 <- readRDS("output/fit-trawl-by-maturity-poisson-link-gengamma.rds")
# fits2 <- readRDS("output/fit-trawl-by-maturity-poisson-link-gengamma-pe.rds")
# #
# # lapply(fits1, \(x) x$family)
# # lapply(fits2, \(x) x$family)
# lapply(fits2, \(x) {p <- get_pars(x);p$gengamma_Q})
#
# fits2a <- readRDS("output/fit-trawl-by-maturity-poisson-link-tv-pe.rds")
# lapply(fits2a, \(x) {AIC(x)})
#
# s <- simulate(fits2a[[1]], nsim = 200, type = "mle-mvn")
# r <- dharma_residuals(s, fits2a[[1]], return_DHARMa = TRUE)
# DHARMa::plotQQunif(r)
#
# fits2b <- readRDS("output/fit-trawl-by-maturity-poisson-link-gengamma-pe.rds")
# lapply(fits2b, \(x) {AIC(x)})
#
# s <- simulate(fits2b[[1]], nsim = 200, type = "mle-mvn")
# r <- dharma_residuals(s, fits2b[[1]], return_DHARMa = TRUE)
# DHARMa::plotQQunif(r)
#
# fits2a[[1]]
# fits2b[[1]]
# set.seed(1)
# r1 <- residuals(fits2a[[1]], model = 2)
# set.seed(1)
# r2 <- residuals(fits2b[[1]], model = 2)
#


mm <- lapply(fits2, \(x) x$data$lengthgroup[1]) |> unlist()
#
# x1 <- lapply(fits1, \(x) tidy(x, 'ran_pars')) |> setNames(mm) |>
#   bind_rows(.id = "lengthgroup") |> mutate(linear_predictor = 1)
# x2 <- lapply(fits1, \(x) tidy(x, 'ran_pars', model = 2)) |> setNames(mm) |>
#   bind_rows(.id = "lengthgroup")|> mutate(linear_predictor = 2)
# xx1 <- bind_rows(x1, x2) |> mutate(data = "excluding zero samples")
#
# x1 <- lapply(fits2, \(x) tidy(x, 'ran_pars')) |> setNames(mm) |>
#   bind_rows(.id = "lengthgroup") |> mutate(linear_predictor = 1)
# x2 <- lapply(fits2, \(x) tidy(x, 'ran_pars', model = 2)) |> setNames(mm) |>
#   bind_rows(.id = "lengthgroup")|> mutate(linear_predictor = 2)
# xx2 <- bind_rows(x1, x2)|> mutate(data = "including zero samples")
#
# bind_rows(xx2, xx1) |>
#   group_by(lengthgroup, linear_predictor) |>
#   mutate(term = ifelse(term == "range", paste(1:2, term), term)) |>
#   ggplot(aes(estimate, lengthgroup, colour = data)) + geom_point() + facet_grid(linear_predictor~term, scales = "free_x")



glmdf <- indexes |> filter(year >= 2005) |>
  group_by(region, group) |>
  group_split() |>
  purrr::map_dfr(\(x) {
    x$decade <- x$year / 10
    m <- glm(est ~ decade, data = x, family = Gamma(link = "log"))
    nd <- data.frame(year = seq(min(x$year), max(x$year)))
    nd$decade = nd$year / 10
    p <- predict(m, newdata = nd, type = "response")
    ret <- data.frame(nd, glm_pred = p)
    row.names(ret) <- NULL
    ret$group = x$group[1]
    ret$region <- x$region[1]
    ret$slope <- coef(m)[[2]]
    ci <- confint(m)
    ret$lwr <- ci[2, 1]
    ret$upr <- ci[2, 2]
    ret
  })

glmdf |> select(group, region, slope, lwr, upr) |> distinct() |>
  ggplot(aes(slope, region, xmin = lwr, xmax = upr, colour = group)) +
  geom_pointrange(position = position_dodge(width = 0.2)) +
  geom_vline(xintercept = 0, lty = 2)

indexes <- add_maturity_group_clean_column(indexes)
glmdf <- add_maturity_group_clean_column(glmdf)
indexes <- clean_region_names(indexes)
glmdf <- clean_region_names(glmdf)

slopes_plot <- glmdf |>
  filter(region != "Coastwide") |>
  select(group_clean, region, slope, lwr, upr) |> distinct() |>
  mutate(group_clean = as.character(group_clean)) |>
  mutate(group_clean = gsub(" females", "\nfemales", group_clean)) |>
  mutate(group_clean = gsub(" males", "\nmales", group_clean)) |>
  mutate(group_clean = factor(group_clean,
    levels = rev(c("Immature", "Maturing\nmales","Mature\nmales", "Maturing\nfemales", "Mature\nfemales")))) |>
  mutate(slope = exp(slope), lwr = exp(lwr), upr = exp(upr)) |>
  ggplot(aes(slope, group_clean, xmin = lwr, xmax = upr, colour = region)) +
  geom_vline(xintercept = 1, lty = 2, colour = "grey70") +
  geom_pointrange(position = position_dodge(width = 0.3), pch = 21) +
  scale_colour_manual(values = cols_region, guide = guide_legend(reverse = TRUE)) +
  ggsidekick::theme_sleek() +
  scale_x_log10(breaks = c(0.3, 0.5, 0.7, 1, 1.3)) +
  theme(axis.title.y.left = element_blank()) +
  xlab("Proportion per decade") +
  labs(colour = "Region") +
  coord_cartesian(xlim = c(0.2, 1.4))
slopes_plot
# ggsave("figs/maturity-index-slopes.png", width = 4.4, height = 3.8)
# ggsave("figs/maturity-index-slopes.pdf", width = 4.4, height = 3.8)

if (FALSE) {
  indexes$group_clean <- forcats::fct_rev(indexes$group_clean)
  glmdf$group_clean <- forcats::fct_rev(glmdf$group_clean)
  ggplot(indexes, aes(year, est, ymin = lwr, ymax = upr, colour = group_clean, fill = group_clean)) +
    geom_line() + geom_ribbon(alpha = 0.2, colour = NA) +
    facet_grid(region ~ group_clean, scales = "free_y") +
    ggsidekick::theme_sleek() +
    coord_cartesian(ylim = c(0, NA), expand = FALSE) +
    # geom_line(aes(x = year, y = glm_pred), inherit.aes = FALSE, data = glmdf, lwd = 0.7, colour = "grey10", alpha = 0.8) +
    labs(y = "Biomass index", x = "Year", colour = "Group", fill = "Group") +
    scale_colour_manual(values = cols_maturities) +
    scale_fill_manual(values = cols_maturities) +
    guides(fill = "none", colour = "none")
  # ggsave("figs/maturity-index-trends-facet-grid.pdf", width = 8.5, height = 5.5)

  indexes |>
    ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = group_clean, fill = group_clean)) +
    geom_line() +
    scale_colour_manual(values = cols_maturities) +
    scale_fill_manual(values = cols_maturities) +
    geom_ribbon(alpha = 0.2, colour = NA) +
    facet_wrap(~region, scales = "free_y", nrow = 4) +
    ggsidekick::theme_sleek() +
    coord_cartesian(ylim = c(0, NA), expand = FALSE) +
    labs(y = "Biomass index", x = "Year", fill = "Group", colour = "Group")
  # ggsave("figs/maturity-index-trends-colour.pdf", width = 5, height = 8.5)
}

if (FALSE) {
  indexes |>
    filter(year >= 2003) |>
    group_by(group_clean, region) |>
    mutate(geo_mean = exp(mean(log_est))) |>
    mutate(
      est = est / geo_mean,
      lwr = lwr / geo_mean,
      upr = upr / geo_mean
    ) |>
    mutate(region = gsub("British Columbia", "British\nColumbia", region)) |>
    mutate(region = gsub("Gulf of Alaska", "Gulf of\nAlaska", region)) |>
    mutate(region = gsub("US West Coast", "US West\nCoast", region)) |>
    mutate(region = factor(region, levels = c("Coastwide", "Gulf of\nAlaska", "British\nColumbia", "US West\nCoast"))) |>
    ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = group_clean, fill = group_clean)) +
    geom_line() +
    geom_ribbon(alpha = 0.2, colour = NA) +
    facet_grid(region ~ group_clean, scales = "free_y") +
    ggsidekick::theme_sleek() +
    coord_cartesian(ylim = c(0, NA), expand = FALSE) +
    labs(y = "Biomass index", x = "Year", colour = "Group", fill = "Group") +
    scale_colour_manual(values = cols_maturities) +
    scale_fill_manual(values = cols_maturities) +
    guides(fill = "none", colour = "none") +
    theme(panel.spacing = unit(-0.1,'lines')) +
    scale_x_continuous(breaks = seq(2005, 2025, 10)) +
    tagger::tag_facets(tag = "panel",
      tag_prefix = "(", position = "tl"
    ) +
    theme(tagger.panel.tag.text = element_text(color = "grey30", size = 9), axis.title.x = element_blank())
  ggsave("figs/maturity-index-trends-facet-grid-small.pdf", width = 6, height = 3.8)
}

if (FALSE) {
  indexes |>
    filter(year >= 2003) |>
    # group_by(group_clean, region) |>
    mutate(geo_mean = exp(mean(log_est))) |>
    mutate(
      est = est / geo_mean,
      lwr = lwr / geo_mean,
      upr = upr / geo_mean
    ) |>
    # mutate(region = gsub("British Columbia", "British\nColumbia", region)) |>
    # mutate(region = gsub("Gulf of Alaska", "Gulf of\nAlaska", region)) |>
    # mutate(region = gsub("US West Coast", "US West\nCoast", region)) |>
    # mutate(region = factor(region, levels = c("Coastwide", "Gulf of\nAlaska", "British\nColumbia", "US West\nCoast"))) |>
    ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = region, fill = region)) +
    geom_line() +
    geom_ribbon(alpha = 0.2, colour = NA) +
    facet_grid(~ group_clean, scales = "free_y") +
    ggsidekick::theme_sleek() +
    coord_cartesian(ylim = c(0, 20), expand = FALSE) +
    labs(y = "Biomass index", x = "Year", colour = "Region", fill = "Region") +
    scale_colour_manual(values = cols_region3) +
    scale_fill_manual(values = cols_region3) +
    theme(panel.spacing = unit(-0.1,'lines')) +
    scale_x_continuous(breaks = seq(2005, 2025, 10)) +
    tagger::tag_facets(tag = "panel",
      tag_prefix = "(", position = "tl"
    ) +
    theme(tagger.panel.tag.text = element_text(color = "grey30", size = 9), axis.title.x = element_blank()) +
    theme(legend.position.inside = c(0.9, 0.6), legend.position = "inside")
  # ggsave("figs/maturity-index-trends-facet-row-small-lognormal-pe.pdf", width = 8.5, height = 2.5)
  # ggsave("figs/maturity-index-trends-facet-row-small-gengamma.pdf", width = 8.5, height = 2.5)
}

# don't emphasize between region scales:

quant <- qnorm(0.75)
# quant <- qnorm(0.975)
g1 <- indexes |>
  filter(year >= 2003) |>
  filter(region != "Coastwide") |>
  mutate(group_clean = forcats::fct_rev(group_clean)) |>
  group_by(group_clean, region) |>
  mutate(geo_mean = exp(mean(log_est))) |>
  mutate(
    est = est / geo_mean,
    # lwr = lwr / geo_mean,
    # upr = upr / geo_mean
    lwr = exp(log_est - quant * se) / geo_mean,
    upr = exp(log_est + quant * se) / geo_mean
  ) |>
  # mutate(region = gsub("British Columbia", "British\nColumbia", region)) |>
  # mutate(region = gsub("Gulf of Alaska", "Gulf of\nAlaska", region)) |>
  # mutate(region = gsub("US West Coast", "US West\nCoast", region)) |>
  # mutate(region = factor(region, levels = c("Coastwide", "Gulf of\nAlaska", "British\nColumbia", "US West\nCoast"))) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = region, fill = region)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, colour = NA) +
  facet_grid(~ group_clean, scales = "free_y") +
  ggsidekick::theme_sleek() +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, xlim = c(2003, max(indexes$year)) + c(-0.7, 0.7)) +
  labs(y = "Biomass index", x = "Year", colour = "Region", fill = "Region") +
  scale_colour_manual(values = cols_region3) +
  scale_fill_manual(values = cols_region3) +
  theme(panel.spacing = unit(-0.1,'lines')) +
  scale_x_continuous(breaks = seq(2005, 2025, 10)) +
  tagger::tag_facets(tag = "panel",
    tag_prefix = "(", position = "tl", tag_pool = c("e", "f", "g", "h", "i")
  ) +
  theme(tagger.panel.tag.text = element_text(color = "grey30", size = 9)) +
  theme(legend.position.inside = c(0.13, 0.73), legend.position = "inside", legend.text = element_text(size = 7), legend.title = element_text(size = 8))
g1

g2 <- indexes |>
  filter(year >= 2003) |>
  filter(region != "Coastwide") |>
  group_by(region) |>
  mutate(geo_mean = exp(mean(log_est))) |>
  mutate(
    est = est / geo_mean,
    # lwr = lwr / geo_mean,
    # upr = upr / geo_mean
    lwr = exp(log_est - quant * se) / geo_mean,
    upr = exp(log_est + quant * se) / geo_mean
  ) |>
  # mutate(region = gsub("British Columbia", "British\nColumbia", region)) |>
  # mutate(region = gsub("Gulf of Alaska", "Gulf of\nAlaska", region)) |>
  # mutate(region = gsub("US West Coast", "US West\nCoast", region)) |>
  # mutate(region = factor(region, levels = c("Coastwide", "Gulf of\nAlaska", "British\nColumbia", "US West\nCoast"))) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = group_clean, fill = group_clean)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, colour = NA) +
  facet_grid(~ region, scales = "free_y") +
  ggsidekick::theme_sleek() +
  # coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  coord_cartesian(ylim = c(0.02, 15)) +
  labs(y = "Biomass index", x = "Year", colour = "Group", fill = "Group") +
  scale_colour_manual(values = cols_maturities, guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = cols_maturities, guide = guide_legend(reverse = TRUE)) +
  theme(panel.spacing = unit(-0.1,'lines')) +
  scale_x_continuous(breaks = seq(2005, 2025, 10)) +
  tagger::tag_facets(tag = "panel",
    tag_prefix = "(", position = "tl"
  ) +
  theme(tagger.panel.tag.text = element_text(color = "grey30", size = 9)) +
  scale_y_continuous(trans = "log10") +
  theme(legend.position.inside = c(0.835, 0.22), legend.position = "inside", legend.text = element_text(size = 7), legend.title = element_text(size = 8)) +
  # theme(legend.position = "bottom")
  guides(fill=guide_legend(nrow=3,byrow=TRUE), colour = guide_legend(nrow = 3, byrow = TRUE))
g2


library(patchwork)
g3 <- slopes_plot + guides(colour = "none", fill = "none") +
  tagger::tag_facets(tag = "panel",
  tag_prefix = "(", position = "tl", tag_pool = c("d")
) + theme(tagger.panel.tag.text = element_text(color = "grey30", size = 9))

layout <- "
AAAAAB
CCCCCC
"

th <- theme(axis.title.x = element_text(size = 9.5))

g2 + th +
  g3 + th +
  g1 + th +
  plot_layout(design = layout)
ggsave("figs/maturity-index-trends-combo.pdf", width = 9.2, height = 5)


