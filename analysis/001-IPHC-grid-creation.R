# create a IPHC grid

#params
Coastalcrs <- 32609
gridsize <- 3000
buffersize <- 25000
mindepth <- 11
maxdepth <- 1096

# library -----------------------------------------------------------------
install.packages('spatialEco')
library(sf)
library(sdmTMB)
library(dplyr)
library(marmap)
library(here)
library(tidyverse)
library(gfiphc)


# Create prediction grid from hull  --------------------------------------------------

#version 1
iphc <- readRDS("output/IPHC_coastdata.rds") %>%
  mutate(UTM.lat.m = UTM.lat * 1000, UTM.lon.m = UTM.lon * 1000)
#path <- "output/PredictionGrid_IPHCcoast.shp"
path_center <- "output/PredictionGridCentres_IPHCcoast.shp"
path_centerswithdepths <- "output/PredictionGridCentres_IPHCcoast_wdepths.rds"
path_final <-  "output/PredictionGridCentres_IPHCcoast_regarea.rds"
# this file is from the website: https://www.iphc.int/data/geospatial-data/
iphcreg <- st_read("data-raw/IPHC_RegulatoryAreas_PDC.shp")



#version 2 with NW US trimmed as it extends far in a couple years and starts a year later
iphc <- readRDS("output/IPHC_coastdata.rds") %>%
  mutate(UTM.lat.m = UTM.lat * 1000, UTM.lon.m = UTM.lon * 1000)
iphc <- iphc |>
  mutate(FID = seq(1,n(),1))
trim <- iphc |>
  filter(iphc.reg.area =="2A" & year %in% c(2017, 2013, 2014))
iphc <- filter(iphc, !iphc$FID %in% trim$FID)
path_center <- "output/PredictionGridCentres_IPHCcoast_trim.shp"
path_centerswithdepths <- "output/PredictionGridCentres_IPHCcoast_wdepths_trim.rds"
path_final <-  "output/PredictionGridCentres_IPHCcoast_regarea_trim.rds"
# this file is from the website: https://www.iphc.int/data/geospatial-data/
iphcreg <- st_read("data-raw/IPHC_RegulatoryAreas_PDC.shp")



#make the grid function
iphcgridfunc <- function(iphc, iphcreg){

iphcgrid_sf <- st_as_sf(iphc, coords = c("UTM.lon.m", "UTM.lat.m"), crs = Coastalcrs) #change to coastalcrs object
plot(st_geometry(iphcgrid_sf))
hullsa <- concaveman::concaveman(filter(iphcgrid_sf, iphc.reg.area %in% c("2A", "2B", "2C")))
hulls3a <- concaveman::concaveman(filter(iphcgrid_sf, iphc.reg.area %in% c("3A", "3B", "2C")))
hulls <- rbind(hullsa, hulls3a)

hullsb <- st_buffer(hulls, dist = buffersize) # 25 km buffer
hullsb <- spatialEco::sf_dissolve(hullsb)
plot(st_geometry(hullsb), col = "blue")
plot(st_geometry(hulls), col = "red", add = TRUE)

# change resolution here
polygony <- st_make_grid(hullsb, square = T, cellsize = c(gridsize, gridsize)) %>%
  st_sf() %>%
  mutate(cell_ID = row_number())
center <- st_centroid(polygony)

center_extent <- st_intersection(st_geometry(hulls), st_geometry(center))
grid_extent <- st_intersection(st_geometry(hulls), st_geometry(polygony))
test_center <- st_sf(center_extent)
grid_extent <- st_sf(grid_extent)
grid_extent$area_km <- st_area(grid_extent) / 1000000 # m to km
range(grid_extent$area_km)

st_write(test_center, path_center, append = FALSE)
testcentre <- st_read(path_center)

testcentre2 <- st_transform(testcentre, crs = "+proj=latlon +datum=WGS84")
df <- testcentre2 %>%
  mutate(
    lat = unlist(purrr::map(testcentre2$geometry, 2)),
    long = unlist(purrr::map(testcentre2$geometry, 1))
  ) |>
  st_drop_geometry()

# Use get.depth to get the depth for each point
suppressWarnings(bio_depth <- getNOAA.bathy(lon1 = -170, lon2 = -120, lat1 = 30, lat2 = 70, resolution = 1)
)

df_depths <- marmap::get.depth(bio_depth, df[, c("long", "lat")], locator = FALSE) %>%
  mutate(bot_depth = (depth * -1)) %>%
  rename(longitude = lon, latitude = lat) %>%
  filter(bot_depth > mindepth & bot_depth < maxdepth) %>%
  mutate(logbot_depth = log(bot_depth)) %>%
  inner_join(df, by = c("longitude" = "long", "latitude" = "lat")) |>
  dplyr::select(-FID) |>
  distinct(.keep_all = TRUE)

df_depths[duplicated(df_depths), ] # just checking

grid1 <- add_utm_columns(df_depths,
  ll_names = c("longitude", "latitude"),
  utm_names = c("UTM.lon", "UTM.lat"),
  utm_crs = Coastalcrs
) %>%
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

# join the points back to the grid so I can predict on the grid
grid2 <- st_join(grid_extent,
  st_as_sf(grid1, coords = c("UTM.lon.m", "UTM.lat.m"), crs = Coastalcrs),
  join = st_contains
) %>%
  drop_na(depth) %>%
  st_drop_geometry()

grid2 <- grid2 |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

#grid2[duplicated(grid2), ] # just checking

# intersect with plot of IPHC regulation areas
# # this file is from the website: https://www.iphc.int/data/geospatial-data/
iphcreg_p <- st_transform(iphcreg, crs = Coastalcrs)

grid3 <- st_as_sf(grid2, coords = c("UTM.lon.m", "UTM.lat.m"), crs = Coastalcrs)
grid4 <- st_intersection(grid3, iphcreg_p) |>
  filter(ET_ID != "4A") |>
  mutate(depth_m_log = log(bot_depth), area_km = as.numeric(area_km))

grid4_ras <- grid4 %>%
  mutate(across(c(UTM.lon, UTM.lat), round, digits = 2))
saveRDS(grid4_ras, path_final)
}

#run the grid function
iphcgridfunc (iphc, iphcreg)
