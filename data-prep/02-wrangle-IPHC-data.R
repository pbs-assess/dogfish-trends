
# bccrs <- 32609
bccrs  <- paste0(
  "+proj=aea +lat_0=48 +lon_0=-133 +lat_1=38.5 ",
  "+lat_2=56 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
)

# load libraries ----------------------------------------------------------
library(sf)
library(PBSdata)
library(ggplot2)
library(here)
library(gfiphc)
library(tidyverse)
library(marmap)
library(sdmTMB)


# Clean data --------------------------------------------------------------

# from 01-load-IPHC-data.R
# https://www.iphc.int/data/fiss-data-query
iphc_stations <- read.csv("data-raw/Map select_standardgrid.csv")
iphc_coast <- read.csv("data-raw/Non-Pacific halibut data_raw.csv")
iphc_latlongs <- read.csv("data-raw/Set and Pacific halibut data_raw.csv") %>%
  dplyr::select(IPHC.Reg.Area, Date, Eff, Ineffcde, BeginLat, BeginLon, AvgDepth..fm., Stlkey)

iphc_coast2 <- iphc_coast %>%
  inner_join(iphc_stations, relationship = "many-to-many") %>%
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
  mutate(hooksobserved = gsub(",", "", hooksobserved)) |> #! IMPORTANT! commas to remove
  mutate(hooksobserved2 = as.numeric(hooksobserved))

iphc_coast4 <- add_utm_columns(iphc_coast3,
  ll_names = c("beginlon", "beginlat"),
  utm_names = c("UTM.lon.m", "UTM.lat.m"),
  utm_crs = bccrs
) %>%
  inner_join(iphc_coast3, relationship = "many-to-many") %>%
  rename(latitude = beginlat, longitude = beginlon) %>%
  mutate(cpue = number.observed / hooksobserved2) %>%
  mutate(dmy = lubridate::dmy(date)) %>%
  mutate(julian = lubridate::yday(dmy)) %>%
  drop_na(julian) %>%
  mutate(station = as.integer(station)) %>%
  mutate(UTM.lat = UTM.lat.m, UTM.lon = UTM.lon.m) |>
  mutate(UTM.lat.m = UTM.lat.m * 1000, UTM.lon.m = UTM.lon.m * 1000) |>
  distinct(.keep_all = TRUE)

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
data(major) # from PBSdata

gmas_PIDs <- data.frame(PID = c(1, seq(3, 9, 1)), GMAs = c(
  "5E", "5D", "5C", "5B", "5A",
  "3D", "3C", "4B"
))

gma <- major %>%
  left_join(gmas_PIDs) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_transform(crs = bccrs) %>%
  group_by(GMAs) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") |>
  filter(GMAs %in% c("3C", "3D", "4B", "5A", "5B", "5C", "5D"))

plot(st_geometry(gma))

# overlay points
iphc_coast6 <- iphc_coast5 %>%
  mutate(lat = latitude, long = longitude) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = bccrs)

iphc_coast6 %>%
  dplyr::select(station, year) %>%
  filter(n() > 1)

#rm puget sound inside waters
df_2ainside <-
  iphc_coast6 |>
  filter(iphc.reg.area == "2A") |>
  mutate(id = paste0(station, year, latitude)) |>
  filter(UTM.lon > 685 & UTM.lat > -100)

iphc_coast7 <-
  iphc_coast6 |>
  filter(iphc.reg.area == "2A") |>
  mutate(id = paste0(station, year, latitude)) |>
  filter(!id %in% df_2ainside$id) |>
  dplyr::select(-c(id)) |>
  bind_rows(filter(iphc_coast6, iphc.reg.area != "2A"))

x <- ggplot(iphc_coast7, aes(UTM.lon, UTM.lat)) + geom_point()
x + geom_point(data = df_2ainside, aes(UTM.lon, UTM.lat), col = "red")

# add depth from marmap ---------------------------------------------------
iphc <- iphc_coast7 |>
  mutate(depth_m_raw = exp(depth_m_log), depth_m_log_raw = depth_m_log) |>
  dplyr::select(-c(depth_m_log, depth_m_log, geometry))
iphc <- st_drop_geometry(iphc)

b <- marmap::getNOAA.bathy(lon1 = -180, lon2 = -110, lat1 = 20, lat2 = 80, resolution = 1, keep = TRUE)
x <- iphc |> dplyr::select(longitude, latitude)

survey_sets2 <- x[!duplicated(x), ] # get depth for the unique locations to save time

iphc2 <- marmap::get.depth(b, survey_sets2[, c("longitude", "latitude")], locator = FALSE) %>%
  filter(depth < 0) |>
  mutate(depth_m = (depth * -1)) %>%
  rename(longitude = lon, latitude = lat) %>%
  mutate(depth_m_log = log(depth_m)) %>%
  right_join(iphc, by = c("longitude" = "longitude", "latitude" = "latitude"))

iphc2[duplicated(iphc2), ]

iphc2 |>
  filter(is.na(depth_m_log) == TRUE) |>
  tally()

ggplot(iphc2, aes(depth_m, depth_m_raw)) +
  geom_point()

# replace NAs depths with the depth that was in the observational database
# it's 15 points, these have positive logbot depths
iphc3 <- iphc2 |>
  mutate(depth_m_log = ifelse(is.na(depth_m_log) == TRUE,
    depth_m_log_raw, depth_m_log
  )) |>
  mutate(depth_m = ifelse(depth_m < 0, exp(depth_m_log_raw), depth_m)) |>
  filter(depth_m < 3199.3723) |> #remove this outlier
  mutate(diff = depth_m - depth_m_raw)

ggplot(iphc3, aes(depth_m, depth_m_raw)) +
  geom_point()

saveRDS(iphc3, "output/IPHC_coastdata.rds")
