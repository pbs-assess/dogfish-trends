
library(gfdata)
library(ggplot2)
library(sf)
library(sdmTMB)
library(PBSdata)
bccrs = 32609

#load data
cpuetl <- readRDS("data-raw/cpue-trawl-bc.rds")

# remove outlier points but include offshore points
cpuetl |>
  ggplot() +
  geom_point(aes(longitude, latitude, size = catch_kg, colour = catch_kg)) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "sqrt")

#is there any shelf polygon available? I don't see one on pacea and these mmt areas are too big, I could but my isobath
#load shp of bc's waters, clip everything outside of this including in sog
# remove points that fall into the inside waters using the GMA fishing areas
# data(major) # from PBSdata
#
# gmas_PIDs <- data.frame(PID = c(1, seq(3, 9, 1)), GMAs = c(
#   "5E", "5D", "5C", "5B", "5A",
#   "3D", "3C", "4B"
# ))
#
# gma <- major %>%
#   left_join(gmas_PIDs) %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
#   st_transform(crs = bccrs) %>%
#   group_by(GMAs) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON") |>
#   filter(GMAs %in% c("3C", "3D", "4B", "5A", "5B", "5C", "5D"))
#
# plot(st_geometry(gma))


# ###end
shelf <- st_read("data-raw", "Shelf_polygon_noSOG")
shelf2 <- sf::st_transform(shelf, crs = "EPSG:32609")
#shelf3 <- st_buffer(shelf2, dist = 25000) #25km
plot(st_geometry(shelf2))
#plot(st_geometry(shelf3), add = TRUE)

cpuetl2 <- cpuetl |>
  mutate(longitude2 = longitude, latitude2 = latitude) |>
  drop_na(longitude, latitude)

#cpuetl2
cpuetl_sp<- sdmTMB::add_utm_columns(cpuetl2,
                                   ll_names = c("longitude2", "latitude2"),
                                   utm_names = c("UTM.lon", "UTM.lat"),
                                   utm_crs = bccrs) |>
  mutate(UTM.lon.m = UTM.lon * 1000,
         UTM.lat.m = UTM.lat * 1000) |>
  st_as_sf(coords = c("UTM.lon.m", "UTM.lat.m"),
           crs = "EPSG:32609")

#cpuetl_sp <- st_as_sf(cpuetl2,
#                      coords = c("UTM.lon.m", "UTM.lat.m"),
#                      crs = "EPSG:32609")

plot(st_geometry(gma))
plot(st_geometry(shelf2), add = TRUE)
plot(st_geometry(cpuetl_sp
                 ), add = TRUE)

cpuetl3 <- st_intersection(cpuetl_sp, st_geometry(shelf2)) %>%
  st_drop_geometry()

cpuetl3 |>
  mutate(UTM.lat.m = UTM.lat*1000, UTM.lon.m = UTM.lon*1000)



# get fleet seans def


#get fleet information. See Sean's for parameter definition
dat <-
  gfplot::tidy_cpue_index(
    cpuetl,
    species_common = tolower("SPINY DOGFISH"),
    gear = "bottom trawl",
    #alt_year_start_date = "02-21", #what is this?
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
    lat_band_width = 0.02#,
    #return_raw_data = TRUE
  )

#create matching columns to get longitude back into dat database
dat$fishing_event_id <- sub('.+-(.+)', '\\1', dat$fishing_event_id)
dat$fishing_event_id_unique <-
  paste0(dat$year, "-",
         dat$trip_id, "-",
         dat$fishing_event_id)
cpuetl$fishing_event_id_unique <-
  paste0(cpuetl$year, "-",
         cpuetl$trip_id, "-",
         cpuetl$fishing_event_id)
glimpse(cpuetl)
glimpse(dat)
cpuetl$month <- as.factor(cpuetl$month)
cpuetl <- cpuetl |> dplyr::select(c(fishing_event_id_unique,dmy, julian,longitude))
test <- left_join(dat, cpuetl, by = c("fishing_event_id_unique" = "fishing_event_id_unique"))

#saveRDS(test, "data-raw/cpue_bottomtrawl_fleet.rds")


saveRDS(cpuetl3, "output/wrangled-cpue.rds")



# cpuell <- get_cpue_index(gear = "hook and line")
# cpuell <- cpuell |> filter(species_code == "044")
# saveRDS(cpuell, "data-raw/cpue_ll.rds")
#
# cpuemd <- get_cpue_index(gear = "midwater trawl")
# cpuemd <- cpuemd |> filter(species_code == "044")
# saveRDS(cpuemd, "data-raw/cpue_midwater.rds")
