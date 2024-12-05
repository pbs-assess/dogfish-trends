library(dplyr)
library(ggplot2)
library(patchwork)
#devtools::install_github("seananderson/ggsidekick")
#devtools::install_github("eliocamp/tagger")
theme_set(ggsidekick::theme_sleek())
source("analysis/999-colours-etc.R")
#ret <- readRDS("output/biomass-weighted-depth.rds")
# ret <- readRDS("output/biomass-weighted-depth-temp.rds") #<- include depth and temp
ret <- readRDS("output/biomass-weighted-depth-temp-constant.rds")

ret <- filter(ret, maturity_group != "mature")
ret <- rename(ret, group = maturity_group)
ret <- add_maturity_group_clean_column(ret)
ret <- clean_region_names(ret)
ret$region <- forcats::fct_rev(ret$region)

g <- ret |>
  filter(region != "Coastwide") |>
  ggplot(aes(year, mean_depth, colour = group_clean, fill = group_clean)) +
  geom_line() +
  geom_ribbon(aes(
    ymin = lwr25,
    ymax = upr75),
    colour = NA, alpha = 0.3
  ) +
  facet_wrap(~region, nrow = 1L) +
  scale_y_reverse() +
  scale_colour_manual(values = cols_maturities) +
  scale_fill_manual(values = cols_maturities) +
  labs(
    fill = "Group", colour = "Group", x = "Year",
    y = "Biomass-weighted\nmean depth (m)"
  ) +
  theme(legend.position = "inside", legend.position.inside = c(0.1, 0.28), axis.title.x = element_blank()) +
  # theme(legend.position = "inside", legend.position.inside = c(0.9, 0.75), axis.title.x = element_blank()) +
  tagger::tag_facets(tag = "panel",
    tag_prefix = "(", position = "tl"
  ) +
  theme(tagger.panel.tag.text = element_text(color = "grey30", size = 9), axis.title.x = element_blank())
print(g)

ggsave("figs/biomass-weighted-depth.png", width = 7.3, height = 2.8)
ggsave("figs/biomass-weighted-depth.pdf", width = 7.3, height = 2.8)

g2 <- ret |>
  filter(region != "Coastwide") |>
  ggplot(aes(year, mean_temp , colour = group_clean, fill = group_clean)) +
  geom_line() +
  geom_ribbon(aes(
    ymin = lwr25_temp,
    ymax = upr75_temp),
    colour = NA, alpha = 0.3
  ) +
  facet_wrap(~region, nrow = 1L) +
  #scale_y_reverse() +
  scale_colour_manual(values = cols_maturities) +
  scale_fill_manual(values = cols_maturities) +
  labs(
    fill = "Group", colour = "Group", x = "Year",
    y = "Biomass-weighted\nmean temperature (C)"
  ) +
  theme(legend.position = "inside", legend.position.inside = c(0.80, 0.28), axis.title.x = element_blank()) +
  # theme(legend.position = "inside", legend.position.inside = c(0.9, 0.75), axis.title.x = element_blank()) +
  tagger::tag_facets(tag = "panel",
                     tag_prefix = "(", position = "tl", tag_pool = c("d", "e", "f")
  ) +
  theme(tagger.panel.tag.text = element_text(color = "grey30", size = 9), axis.title.x = element_blank())
print(g2)

ggsave("figs/biomass-weighted-temp.png", width = 7.3, height = 2.8)
ggsave("figs/biomass-weighted-temp.pdf", width = 7.3, height = 2.8)

patchwork::wrap_plots(
  g + theme(legend.position.inside = c(0.1, 0.32)),
  g2 + theme(legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank()),
  ncol = 1,
  axis_titles = "collect", axes = "collect")
ggsave("figs/biomass-weighted-temp.png", width = 7.3, height = 4.5)
ggsave("figs/biomass-weighted-temp.pdf", width = 7.3, height = 4.5)

g3 <- ret |>
  filter(region != "Coastwide") |>
  ggplot(aes(year, mean_temp_constant_density , colour = group_clean, fill = group_clean)) +
  geom_line() +
  geom_ribbon(aes(
    ymin = lwr25_temp_constant_density,
    ymax = upr75_temp_constant_density),
    colour = NA, alpha = 0.3
  ) +
  facet_wrap(~region, nrow = 1L) +
  #scale_y_reverse() +
  scale_colour_manual(values = cols_maturities) +
  scale_fill_manual(values = cols_maturities) +
  labs(
    fill = "Group", colour = "Group", x = "Year",
    y = "Average biomass-weighted\nmean temperature (C)"
  ) +
  theme(legend.position = "inside", legend.position.inside = c(0.80, 0.28), axis.title.x = element_blank()) +
  # theme(legend.position = "inside", legend.position.inside = c(0.9, 0.75), axis.title.x = element_blank()) +
  tagger::tag_facets(tag = "panel",
    tag_prefix = "(", position = "tl", tag_pool = c("g", "h", "i")
  ) +
  theme(tagger.panel.tag.text = element_text(color = "grey30", size = 9), axis.title.x = element_blank())
print(g3)

patchwork::wrap_plots(
  g + theme(legend.position.inside = c(0.1, 0.32)),
  g2 + theme(legend.position = "none") + theme(legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank()),
  g3 + theme(legend.position = "none") + theme(legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank()), ncol = 1, axis_titles = "collect", axes = "collect")
ggsave("figs/biomass-weighted-temp-constant.png", width = 7.3, height = 7.3)
ggsave("figs/biomass-weighted-temp-constant.pdf", width = 7.3, height = 7.3)
