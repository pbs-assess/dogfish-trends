library(ggplot2)
library(dplyr)
source("analysis/999-colours-etc.R")
indexes <- readRDS("output/index-trawl-by-maturity-poisson-link.rds")
indexes$region <- factor(indexes$region, levels = c("Coast", "GOA", "BC", "NWFSC"))

glmdf <- indexes |> filter(year >= 2006) |>
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

glmdf |> select(group_clean, region, slope, lwr, upr) |> distinct() |>
  mutate(slope = exp(slope), lwr = exp(lwr), upr = exp(upr)) |>
  ggplot(aes(slope, group_clean, xmin = lwr, xmax = upr, colour = region)) +
  geom_vline(xintercept = 1, lty = 2, colour = "grey70") +
  geom_pointrange(position = position_dodge(width = 0.3), pch = 21) +
  scale_colour_manual(values = cols_region, guide = guide_legend(reverse = TRUE)) +
  ggsidekick::theme_sleek() +
  scale_x_log10(breaks = c(0.3, 0.5, 0.7, 1, 1.3)) +
  theme(axis.title.y.left = element_blank()) +
  xlab("Multiplicative population\nchange per decade") +
  labs(colour = "Region") +
  coord_cartesian(xlim = c(0.2, 1.4))
ggsave("figs/maturity-index-slopes.png", width = 4.4, height = 3.8)
ggsave("figs/maturity-index-slopes.pdf", width = 4.4, height = 3.8)

indexes$group_clean <- forcats::fct_rev(indexes$group_clean)
glmdf$group_clean <- forcats::fct_rev(glmdf$group_clean)
ggplot(indexes, aes(year, est, ymin = lwr, ymax = upr, colour = group_clean, fill = group_clean)) +
  geom_line() + geom_ribbon(alpha = 0.2, colour = NA) +
  facet_grid(region ~ group_clean, scales = "free_y") +
  ggsidekick::theme_sleek() +
  coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  geom_line(aes(x = year, y = glm_pred), inherit.aes = FALSE, data = glmdf, lwd = 0.7, colour = "grey10", alpha = 0.8) +
  labs(y = "Biomass index", x = "Year", colour = "Group", fill = "Group") +
  scale_colour_manual(values = cols_maturities) +
  scale_fill_manual(values = cols_maturities) +
  guides(fill = "none", colour = "none")
ggsave("figs/maturity-index-trends-facet-grid.pdf", width = 8.5, height = 5.5)

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
ggsave("figs/maturity-index-trends-colour.pdf", width = 5, height = 8.5)

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
  coord_cartesian(ylim = c(0, NA), expand = FALSE) +
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
ggsave("figs/maturity-index-trends-facet-row-small.pdf", width = 8.5, height = 2.5)