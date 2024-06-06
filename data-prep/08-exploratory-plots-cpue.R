
library(gfdata)
library(tidyverse)
library(sf)
library(ggplot2)
library(PBSmapping)
library(here)
library(tidyr)
library(purrr)
library(sdmTMB)
library(gfplot)


fleet <- readRDS("output/wrangled-cpue.rds")

# summary plots -----------------------------------------------------------
fleet$latitude <- as.numeric(as.character(fleet$latitude))
fleet$depth <- as.numeric(as.character(fleet$depth))

#map of offshore points and sean's fleet definition
ggplot() +
  geom_point(data = fleet,
             aes(longitude, latitude), colour = ("black"),
             alpha = 0.5) +
  #facet_wrap(~year) +
  #scale_colour_viridis_c(trans = "log") +
  #scale_colour_viridis_c() +
  theme_classic()

ggplot() +
  geom_point(data = fleet,
             aes(longitude, latitude), colour = ("black"),
             alpha = 0.5) +
  xlim(c(-128, -125)) +
  ylim(c(48, 50)) +
  theme_classic() +
  facet_wrap(~year)

fleet |>
  ggplot() +
  geom_point(aes(longitude, latitude, size = (cpue), colour = (cpue)),
             alpha = 0.5) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "log") +
  #scale_colour_viridis_c() +
  theme_classic()

fleet |>
  group_by(year) |>
  summarize(sum_cpue = sum(cpue)) |>
  ggplot() +
  geom_point(aes(year, sum_cpue))

range(fleet$depth)
fleet$depth <- as.numeric(as.character(fleet$depth))
fleet |>
  ggplot() +
  geom_point(aes(year, (depth), size = cpue, colour = cpue)) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic()

#get rid of 2024
fleet |>
  ggplot() +
  geom_point(aes(year, julian, size = cpue, colour = cpue))

