#create a IPHC grid


Coastalcrs <- 32609

# library -----------------------------------------------------------------

library(sf); library(sdmTMB); library(dplyr);library(marmap); library(here)
library(tidyverse); library(gfiphc)



# load source code --------------------------------------------------------
#source("source_functions.R")



# Create prediction grid from hull  --------------------------------------------------

iphc <- readRDS("output/IPHC_coastdata.rds") %>%
  mutate(UTM.lat.m = UTM.lat * 1000, UTM.lon.m = UTM.lon * 1000)
iphcgrid_sf <- st_as_sf(iphc, coords = c("UTM.lon.m", "UTM.lat.m"), crs = 32609)
plot(st_geometry(iphcgrid_sf))
hullsa <- concaveman::concaveman(filter(iphcgrid_sf, iphc.reg.area %in% c("2A", "2B", "2C")))
hulls3a <- concaveman::concaveman(filter(iphcgrid_sf, iphc.reg.area %in% c("3A", "3B", "2C")))
hulls <- rbind(hullsa, hulls3a)

plot(st_geometry(hulls), col = "red")
hullsb <- st_buffer(hulls, dist = 25000) #2.5 km buffer
plot(st_geometry(hullsb), col = "blue" )
plot(st_geometry(hulls), col = "red", add = TRUE)

plot(st_geometry(iphcgrid_sf), add = TRUE)
points(iphc$UTM.lon.m, iphc$UTM.lat.m)

# change resolution here
grid_spacing <- 3000 # 3 kilometers define the size
polygony <- st_make_grid(hullsb, square = T, cellsize = c(grid_spacing, grid_spacing)) %>%
  st_sf() %>%
  mutate(cell_ID = row_number())

center <- st_centroid(polygony)

center_extent <- st_intersection(st_geometry(hulls), st_geometry(center))
grid_extent <- st_intersection(st_geometry(hulls), st_geometry(polygony))
test_center <- st_sf(center_extent)
grid_extent <- st_sf(grid_extent)
grid_extent$area_km <- st_area(grid_extent) / 1000000 # m to km
range(grid_extent$area_km)

st_write(grid_extent, "output/PredictionGrid_IPHCcoast.shp", append = FALSE)
st_write(test_center, "output/PredictionGridCentres_IPHCcoast.shp", append = FALSE)

testcentre <- st_read("output/PredictionGridCentres_IPHCcoast.shp")

plot(st_geometry(testcentre))
testcentre2 <- st_transform(testcentre, crs = "+proj=latlon +datum=WGS84")
testcentre3 <- testcentre2 %>%
  mutate(
    lat = unlist(purrr::map(testcentre2$geometry, 2)),
    long = unlist(purrr::map(testcentre2$geometry, 1))
  )
df <- st_drop_geometry(testcentre3)

plot(df$long, df$lat)
min(df$lat)
min(df$long)
max(df$lat)
max(df$long)

# Use get.depth to get the depth for each point
bio_depth <- getNOAA.bathy(lon1 = -170, lon2 = -120, lat1 = 30, lat2 = 70, resolution = 1)
# x <- get.depth(bio_depth, df[, c("longitude", "latitude")], locator = FALSE)
# glimpse(x)
# df_depths <- inner_join(df, x, by = c("lat" = "lat", "long" = "lon"))
# head(df_depths)

df_depths <- marmap::get.depth(bio_depth, df[, c("long", "lat")], locator = FALSE) %>%
  mutate(bot_depth = (depth * -1)) %>%
  rename(longitude = lon, latitude = lat) %>%
  filter(bot_depth > 11 & bot_depth < 1096) %>%
  mutate(logbot_depth = log(bot_depth)) %>%
  inner_join(df, by = c("longitude" = "long", "latitude" = "lat")) |>
  dplyr::select(-FID) |>
  distinct(.keep_all = TRUE)

df_depths[duplicated(df_depths), ] # just checking

# # see which point are the ones that have positive depths
# df_depths2 <- filter(df_depths, depth > -1096 & depth < -11) %>%
#   mutate(depth_m = depth * -1)
# max(df_depths2$depth_m)
# min(df_depths2$depth_m)

grid1 <- add_utm_columns(df_depths,
                         ll_names = c("longitude", "latitude"),
                         utm_names = c("UTM.lon", "UTM.lat"),
                         utm_crs = Coastalcrs
) %>%
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

# ggplot(df_depths2, aes(long, lat, colour = depth_m)) +
#   geom_point(size = 0.5)

# join the points back to the grid so I can predict on the grid
grid2 <- st_join(grid_extent,
                 st_as_sf(grid1, coords = c("UTM.lon.m", "UTM.lat.m"), crs = Coastalcrs),
                 join = st_contains
) %>%
  drop_na(depth ) %>%
  st_drop_geometry()

saveRDS(grid2, "output/PredictionGridCentres_IPHCcoast_wdepths.rds")
grid2 <- readRDS("output/PredictionGridCentres_IPHCcoast_wdepths.rds") |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

plot(grid2$UTM.lon, grid2$UTM.lat)
points(iphc$UTM.lon, iphc$UTM.lat, col = "red")

test <- grid2[duplicated(grid2), ] # just checking

# intersect with plot of IPHC regulation areas
#this file is from the website: https://www.iphc.int/data/geospatial-data/
iphcreg <- st_read("data-raw/IPHC_RegulatoryAreas_PDC.shp")
st_crs(iphcreg)
iphcreg_p <- st_transform(iphcreg, crs = Coastalcrs)

ggplot(iphcreg_p) +
  geom_sf() +
  geom_point(data = grid2, aes(UTM.lon.m, UTM.lat.m), col = "red")

grid3 <- st_as_sf(grid2, coords = c("UTM.lon.m", "UTM.lat.m"), crs = 32609)
grid4 <- st_intersection(grid3, iphcreg_p) |>
  filter(ET_ID != "4A") |>
  mutate(depth_m_log = log(depth), area_km = as.numeric(area_km))

ggplot(grid4, aes(UTM.lon, UTM.lat, col = ET_ID)) +
  geom_point(size = 0.25)
ggplot(grid4, aes(UTM.lon, UTM.lat, col = log(depth_m_log))) +
  geom_point()

grid4_ras <- grid4 %>%
  mutate(across(c(UTM.lon, UTM.lat), round, digits = 2))

ggplot(grid4_ras, aes(UTM.lon, UTM.lat, col = (depth_m_log))) +
  geom_raster() +
  scale_colour_viridis_c()

ggplot(iphcreg_p) +
  geom_sf() +
  geom_raster(data = grid4_ras, aes(UTM.lon * 1000, UTM.lat * 1000, fill = ET_ID))

ggplot(iphcreg_p) +
  geom_sf() +
  geom_raster(data = grid4_ras, aes(UTM.lon * 1000, UTM.lat * 1000, fill = depth))

saveRDS(grid4_ras, "output/PredictionGridCentres_IPHCcoast_regarea.rds")
grid_final <- readRDS("output/PredictionGridCentres_IPHCcoast_regarea.rds")

st_write(grid4_ras, "output/grid_IPHCcoast.shp", append = FALSE)


