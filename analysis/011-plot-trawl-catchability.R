library(dplyr)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())

dd <- readRDS("output/trawl-indexes-with-catchability.rds")
if ("nwfsc" %in% unique(dd$region)) {
  dd$region[dd$region == "nwfsc"] <- "NWFSC"
}

dd |>
  mutate(region = gsub("NWFSC", "US West Coast", region)) |>
  mutate(region = factor(region, levels = c("Coastwide", "GOA", "BC", "US West Coast"))) |>
  group_by(model, region) |>
  mutate(geo_mean = exp(mean(log_est[year >= 2003]))) |>
  mutate(
    est = est / geo_mean,
    lwr = lwr / geo_mean,
    upr = upr / geo_mean
  ) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = model, fill = model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  xlab("Year") +
  ylab("Standardized relative biomass") +
  labs(colour = "Model", fill = "Model") +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, xlim = range(index$year) + c(-0.5, 0.5)) +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~region, ncol = 1)

ggsave("figs/index-trawl-main-by-region-catchability-effects.pdf", width = 5.5, height = 10)
