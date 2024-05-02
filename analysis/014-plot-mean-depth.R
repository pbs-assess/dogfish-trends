library(dplyr)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())
source("analysis/999-colours-etc.R")

ret <- readRDS("output/biomass-weighted-depth.rds")
ret <- rename(ret, group = maturity_group)
ret <- add_maturity_group_clean_column(ret)
ret <- clean_region_names(ret)
ret$region <- forcats::fct_rev(ret$region)

g <- ret |>
  ggplot(aes(year, mean_depth, colour = group_clean, fill = group_clean)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_depth - sd_depth,
    ymax = mean_depth + sd_depth),
    colour = NA, alpha = 0.2
  ) +
  facet_wrap(~region) +
  scale_y_reverse() +
  scale_colour_manual(values = cols_maturities) +
  scale_fill_manual(values = cols_maturities) +
  labs(
    fill = "Group", colour = "Group", x = "Year",
    y = "Biomass-weighted mean depth (m)"
  ) +
  theme(legend.position = "inside", legend.position.inside = c(0.11, 0.24), axis.title.x = element_blank()) +
  tagger::tag_facets(tag = "panel",
    tag_prefix = "(", position = "tl"
  ) +
  theme(tagger.panel.tag.text = element_text(color = "grey30", size = 9), axis.title.x = element_blank())
print(g)

ggsave("figs/biomass-weighted-depth.png", width = 6.5, height = 3.2)
ggsave("figs/biomass-weighted-depth.pdf", width = 6.5, height = 3.2)
