#create exploratory plots of the wranged trawl set data


library(tidyverse)
library(ggplot2)


survey_sets <- readRDS("output/Wrangled_USCan_trawldata.rds")
dir.create("Figures", showWarnings = FALSE)

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), log(catch_weight))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_name, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()

ggplot(
  survey_sets,
  aes(as.factor(year), area_swept_m2)
) +
  # aes(as.factor(year), cpue)) +
  geom_jitter() +
  geom_violin() +
  facet_wrap(~survey_abbrev, ncol = 1) # , scales = "free")

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), log(catch_weight))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_name, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()
# ggsave("Figures/ExtremeCatchEvents.jpg", width = 10, height = 8)

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), logbot_depth)) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_name, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()
# ggsave("Figures/catch_weight_bydepth.jpg", width = 10, height = 8)

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), (julian))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_abbrev, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()
# ggsave("Figures/catch_weight_byjulian.jpg", width = 10, height = 8)

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), (bottom_temp_c))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  # geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_abbrev, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), (julian))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  facet_wrap(~survey_name, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()

ggplot(filter(survey_sets, catch_weight == 0), aes(as.factor(year), logbot_depth)) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  facet_wrap(~survey_abbrev, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()

range(survey_sets$logbot_depth, na.rm = TRUE)
survey_sets <- survey_sets |>
  group_by(survey_abbrev) |>
  mutate(depthsplit = ifelse(logbot_depth < 3.5, "shallow", ifelse(logbot_depth >= 3.5 & logbot_depth < 4, "middle", "deep")))

x <- survey_sets |>
  group_by(year, depthsplit, survey_abbrev) |>
  filter(catch_weight == 0) |>
  tally()
ggplot(x, aes(year, n, colour = depthsplit)) +
  geom_point() +
  geom_line() +
  facet_wrap(~survey_abbrev, scales = "free")
# ggsave("Figures/zeros_by_depth.jpg", width = 10, height = 8)

