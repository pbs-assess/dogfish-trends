# Pull IPHC data


sf_use_s2(FALSE)
# load libraries ----------------------------------------------------------
library(sf)
library(wesanderson)
library(dplyr)
library(PBSdata)
library(ggplot2)
library(tidyr)
library(here)
# install.packages("gfdata")
library(rnaturalearthhires)
library(ggsidekick)
library(gfdata)
library(gfplot)
theme_set(ggsidekick::theme_sleek())
library(sp)
library(sdmTMB)
library(gfiphc)
library(tidyverse)
library(marmap)
library(RColorBrewer)
library("gridExtra")
# install.packages("devtools")
devtools::install_github("jakelawlor/PNWColors")
# remotes::install_github("pbs-assess/sdmTMB", ref = "rw0")
# install.packages("sdmTMB")
# remotes::install_github("pbs-assess/sdmTMB", ref = "rw0")


# load source code --------------------------------------------------------
source("source_functions.R")


# CHECK IPHC coast data clean - get rid of expansion set, add date, checks --------------------------------------------------

# downloaded from here:
# https://www.iphc.int/data/fiss-data-query

iphc_stations <- read.csv("data-raw/IPHC data download from IPHC website/Map select_standardgrid.csv")
iphc_coast <- read.csv("data-raw/IPHC data download from IPHC website/Non-Pacific halibut data_raw.csv")
iphc_latlongs <- read.csv("data-raw/IPHC data download from IPHC website/Set and Pacific halibut data_raw.csv") %>%
  dplyr::select(IPHC.Reg.Area, Date, Eff, Ineffcde, BeginLat, BeginLon, AvgDepth..fm., Stlkey)

glimpse(iphc_latlongs)
glimpse(iphc_coast)
glimpse(iphc_stations)

iphc_coast2 <- iphc_coast %>%
  inner_join(iphc_stations) %>%
  inner_join(iphc_latlongs, by = "Stlkey")

names(iphc_coast2) <- tolower(names(iphc_coast2))

iphc_coast3 <- iphc_coast2 %>%
  filter(eff == "Y") %>%
  filter(purpose == "Standard Grid") %>%
  mutate(startlonfix = ifelse(beginlon > 0, beginlon * -1, beginlon)) %>%
  filter(iphc.reg.area %in% c("2A", "2B", "2C", "3A", "3B")) %>%
  mutate(depth_m = 1.8288 * avgdepth..fm.) %>%
  mutate(depth_m_log = log(depth_m)) %>%
  dplyr::select(
    depth_m_log, year, beginlat, beginlon, station,
    iphc.reg.area, number.observed, hooksobserved, date
  ) %>%
  mutate(hooksobserved2 = as.numeric(hooksobserved)) %>%
  drop_na(hooksobserved2)

iphc_coast4 <- add_utm_columns(iphc_coast3,
  ll_names = c("beginlon", "beginlat"),
  utm_names = c("UTM.lon.m", "UTM.lat.m"),
  utm_crs = 32609
) %>%
  inner_join(iphc_coast3) %>%
  rename(latitude = beginlat, longitude = beginlon) %>%
  mutate(cpue = number.observed / hooksobserved2) %>%
  mutate(dmy = lubridate::dmy(date)) %>%
  mutate(julian = lubridate::yday(dmy)) %>%
  drop_na(julian) %>%
  mutate(station = as.integer(station)) %>%
  mutate(UTM.lat = UTM.lat.m, UTM.lon = UTM.lon.m) |>
  mutate(UTM.lat.m = UTM.lat.m * 1000, UTM.lon.m = UTM.lon.m * 1000) |>
  distinct(.keep_all = TRUE)

ggplot(
  data = iphc_coast4,
  aes(UTM.lon, UTM.lat), size = 1.5, col = "blue"
) +
  geom_point()

filter(iphc_coast4, station == 2099 & year == 2019) # check no duplications
iphc_coast4[duplicated(iphc_coast4), ] # just checking


# get rid of SOG points the expansion set in 2018
# get rid of expansion set first by filtering those sampled once
surveyed1 <- iphc_coast4 %>%
  group_by(station) %>%
  mutate(count = n()) %>%
  filter(count == 1 & year == 2018)

iphc_coast5 <- filter(iphc_coast4, !(station %in% surveyed1$station))

# remove points that fall into the inside waters using the GMA fishing areas
library(PBSdata)
data(major) # from PBSdata

gmas_PIDs <- data.frame(PID = c(1, seq(3, 9, 1)), GMAs = c(
  "5E", "5D", "5C", "5B", "5A",
  "3D", "3C", "4B"
))
gma <- major %>%
  left_join(gmas_PIDs) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_transform(crs = 32609) %>%
  group_by(GMAs) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") |>
  filter(GMAs %in% c("3C", "3D", "4B", "5A", "5B", "5C", "5D"))
plot(st_geometry(gma))

# overlay points
iphc_coast6 <- iphc_coast5 %>%
  mutate(lat = latitude, long = longitude) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 32609)

iphc_coast7 <- st_intersection(iphc_coast6, st_geometry(gma)) %>%
  st_drop_geometry() %>%
  dplyr::select(-dmy) %>%
  mutate(UTM.lat.m = UTM.lat * 1000, UTM.lon.m = UTM.lon * 1000) |>
  bind_rows(filter(iphc_coast6, iphc.reg.area !="2B"))

iphc_coast7 %>%
  dplyr::select(station, year) %>%
  filter(n() > 1) # some duplicates in the database

ggplot(
  data = iphc_coast7,
  aes(UTM.lon, UTM.lat), size = 1.5, col = "blue"
) +
  geom_point()


saveRDS(iphc_coast7, "output/IPHC_coastdata.rds")
iphc <- readRDS("output/IPHC_coastdata.rds")



# add depth from marmap ---------------------------------------------------
iphc <- readRDS("output/IPHC_coastdata.rds") |>
  mutate(depth_m_raw = exp(depth_m_log), depth_m_log_raw = depth_m_log) |>
  dplyr::select(-c(depth_m_log, depth_m_log))

ggplot(iphc, aes(longitude, latitude)) + geom_point()

b <- marmap::getNOAA.bathy(lon1 = -180, lon2 = -110, lat1 = 20, lat2 = 80, resolution = 1)
x <- iphc |> dplyr::select(longitude, latitude)
x
survey_sets2 <- x[!duplicated(x), ] #get depth for the unique locations to save time

iphc2 <- marmap::get.depth(b, survey_sets2[, c("longitude", "latitude")], locator = FALSE) %>%
  filter(depth < 0 ) |>
  mutate(depth_m = (depth * -1)) %>%
  rename(longitude = lon, latitude = lat) %>%
  mutate(depth_m_log = log(depth_m)) %>%
  right_join(iphc, by = c("longitude" = "longitude", "latitude" = "latitude"))

iphc2[duplicated(iphc2), ]

iphc2 |>
  filter(is.na(depth_m_log) == TRUE) |>
  tally()

ggplot(iphc2, aes(depth_m, depth_m_raw)) +
  geom_point() # remove that outlier

# replace NAs depths with the depth that was in the observational database
# it's 15 points so negligible, these have positive logbot depths
iphc3 <- iphc2 |>
  mutate(depth_m_log = ifelse(is.na(depth_m_log) == TRUE,
    depth_m_log_raw, depth_m_log
  )) |>
  mutate(depth_m = ifelse(depth_m < 0, exp(depth_m_log_raw), depth_m)) |>
  filter(depth_m < 3199.3723) |>
  mutate(diff = depth_m - depth_m_raw)
ggplot(iphc3, aes(depth_m, depth_m_raw)) +
  geom_point() # remove that outlier

ggplot(iphc3, aes(longitude, latitude, colour = diff)) +
  geom_point(size = 0.4) +
  scale_colour_viridis_c(trans = "sqrt")

saveRDS(iphc3, "output/IPHC_coastdata.rds")




# Exploratory plots  -----------------------------------------

iphc <- readRDS("output/IPHC_coastdata.rds")

ggplot(iphc, aes(as.factor(year), julian)) +
  geom_boxplot() +
  facet_wrap(~iphc.reg.area, nrow = 1)


iphc %>%
  group_by(year, iphc.reg.area) %>%
  summarize(sumcount = sum(number.observed)) %>%
  ggplot(aes(year, sumcount)) +
  geom_line(size = 2) +
  geom_point(size = 2, colour = "red") +
  facet_wrap(~iphc.reg.area)


iphc %>%
  group_by(year, iphc.reg.area) %>%
  drop_na(hooksobserved) %>%
  mutate(sumeffhks = sum(hooksobserved2)) %>%
  mutate(catch = sum(number.observed)) %>%
  summarise(cpue = catch / sumeffhks) %>%
  ggplot(aes(year, cpue, group = iphc.reg.area, col = iphc.reg.area)) +
  geom_line(size = 2) +
  geom_point(size = 2, colour = "red") +
  facet_wrap(~iphc.reg.area)

iphc |>
  filter(iphc.reg.area == "2B") |>
  ggplot() +
  geom_point(aes(UTM.lon.m, UTM.lat.m), colour = "grey") +
  facet_wrap(~year)

x <- ggplot(iphc, aes(UTM.lon.m, UTM.lat.m), colour = "grey") +
  geom_point() +
  facet_wrap(~year)


