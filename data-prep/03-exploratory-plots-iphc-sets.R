library(ggplot2)
library(dplyr)

#from 02-wrange-IPHC-data.R
iphc <- readRDS("output/IPHC_coastdata.rds")


ggplot(iphc, aes(as.factor(year), julian)) +
  geom_boxplot() +
  facet_wrap(~iphc.reg.area, nrow = 1)

iphc %>%
  group_by(year, iphc.reg.area) %>%
  summarize(sumcount = sum(number.observed)) %>%
  ggplot(aes(year, sumcount)) +
  geom_line(linewidth = 2) +
  geom_point(size = 2, colour = "red") +
  facet_wrap(~iphc.reg.area)

iphc %>%
  group_by(year, iphc.reg.area) %>%
  drop_na(hooksobserved) %>%
  mutate(sumeffhks = sum(hooksobserved2)) %>%
  mutate(catch = sum(number.observed)) %>%
  mutate(cpue = catch / sumeffhks) %>%
  ggplot(aes(year, cpue, group = iphc.reg.area, col = iphc.reg.area)) +
  geom_line(linewidth = 2) +
  geom_point(size = 2, colour = "red") +
  facet_wrap(~iphc.reg.area)

iphc |>
  filter(iphc.reg.area == "2B") |>
  ggplot() +
  geom_point(aes(UTM.lon.m, UTM.lat.m), colour = "grey") +
  facet_wrap(~year)

ggplot(iphc, aes(UTM.lon.m, UTM.lat.m), colour = "grey") +
  geom_point() +
  facet_wrap(~year)

ggplot(iphc, aes(longitude, latitude, colour = diff)) +
  geom_point(size = 0.4) +
  scale_colour_viridis_c()
