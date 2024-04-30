library(dplyr)
library(ggplot2)

ret <- readRDS("output/biomass-weighted-depth.rds")

lu <- data.frame(
  maturity_group = c("mm", "mf", "maturingm", "maturingf", "imm"),
  group_clean = c("Mature males", "Mature females", "Maturing males", "Maturing females", "Immature"),
  stringsAsFactors = FALSE
)
ret2 <- left_join(ret, lu)
lvls <- rev(c(
  "Immature", "Maturing females", "Maturing males",
  "Mature males", "Mature females"
))
ret2$group_clean <- factor(ret2$group_clean, levels = lvls)

lu <- data.frame(
  region = c("GOA", "BC", "NWFSC"),
  region_clean = c("Gulf of Alaska", "British Columbia", "US West Coast"),
  stringsAsFactors = FALSE
)
ret2 <- left_join(ret2, lu)
ret2$region_clean <- factor(ret2$region_clean,
  levels = c("Gulf of Alaska", "British Columbia", "US West Coast")
)

pal <- "Paired"
g <- ret2 |>
  ggplot(aes(year, mean_depth, colour = group_clean, fill = group_clean)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_depth - sd_depth,
    ymax = mean_depth + sd_depth),
    colour = NA, alpha = 0.2
  ) +
  facet_wrap(~region_clean) +
  scale_y_reverse() +
  scale_colour_brewer(palette = pal) +
  scale_fill_brewer(palette = pal) +
  labs(
    fill = "Group", colour = "Group", x = "Year",
    y = "Biomass-weighted mean depth (m)"
  ) +
  theme(legend.position = "inside", legend.position.inside = c(0.11, 0.24))
g

ggsave("figs/biomass-weighted-depth.png", width = 7, height = 3.5)
ggsave("figs/biomass-weighted-depth.pdf", width = 7, height = 3.5)
