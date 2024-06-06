library(gfdata)
library(ggplot2)
library(sf)
library(sdmTMB)
library(PBSdata)
bccrs <- 32609

# load data
cpuetl <- readRDS("data-raw/cpue-trawl-bc.rds")

# remove outlier points but include offshore points
cpuetl |>
  ggplot() +
  geom_point(aes(longitude, latitude, size = catch_kg, colour = catch_kg)) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "sqrt")

shelf <- st_read("data-raw", "Shelf_polygon_noSOG")
shelf2 <- sf::st_transform(shelf, crs = bccrs)

cpuetl_sp <- cpuetl |>
  mutate(longitude2 = longitude, latitude2 = latitude) |>
  drop_na(longitude, latitude) |>
  sdmTMB::add_utm_columns(
  ll_names = c("longitude2", "latitude2"),
  utm_names = c("UTM.lon", "UTM.lat"),
  utm_crs = bccrs
) |>
  mutate(
    UTM.lon.m = UTM.lon * 1000,
    UTM.lat.m = UTM.lat * 1000
  ) |>
  st_as_sf(
    coords = c("UTM.lon.m", "UTM.lat.m"),
    crs = bccrs
  )

cpuetl3 <- st_intersection(cpuetl_sp, st_geometry(shelf2)) %>%
  st_drop_geometry()

cpuetl3 |>
  mutate(UTM.lat.m = UTM.lat * 1000, UTM.lon.m = UTM.lon * 1000)

# get fleet information. See Sean's for parameter definition
dat <-
  gfplot::tidy_cpue_index(
    cpuetl3,
    species_common = tolower("SPINY DOGFISH"),
    gear = "bottom trawl",
    # alt_year_start_date = "02-21", #what is this?
    use_alt_year = FALSE,
    year_range = c(1996, 2024),
    lat_range = c(48, Inf),
    min_positive_tows = 100,
    min_positive_trips = 5,
    min_yrs_with_trips = 5,
    depth_band_width = 1,
    area_grep_pattern = c("^5A|^5B|^5C|^5D|^5E|^3C|^3D"),
    depth_bin_quantiles = c(0.001, 0.999),
    min_bin_prop = 0.001,
    lat_band_width = 0.02 # ,
    # return_raw_data = TRUE
  )

# create matching columns to get longitude back into dat database
dat$fishing_event_id <- sub(".+-(.+)", "\\1", dat$fishing_event_id)
dat$fishing_event_id_unique <-
  paste0(
    dat$year, "-",
    dat$trip_id, "-",
    dat$fishing_event_id
  )

cpuetl3$fishing_event_id_unique <-
  paste0(
    cpuetl3$year, "-",
    cpuetl3$trip_id, "-",
    cpuetl3$fishing_event_id
  )

cpuetl3$month <- as.factor(cpuetl3$month)
cpuetl3 <- cpuetl3 |> dplyr::select(c(fishing_event_id_unique, dmy, julian, longitude))
dat2 <- left_join(dat, cpuetl3, by = c("fishing_event_id_unique" = "fishing_event_id_unique"))

saveRDS(dat2, "output/wrangled-cpue.rds")

