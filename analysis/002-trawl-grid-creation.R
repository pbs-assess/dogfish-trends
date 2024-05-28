# create regional and coast wide prediction grids


# library -----------------------------------------------------------------
theme_set(gfplot::theme_pbs())
sf_use_s2(FALSE)
library(ggplot2)
library(dplyr)
library(wesanderson)
library(tidyr)
library(here)
library(ggsidekick)
library(gfdata)
library(gfplot)
theme_set(ggsidekick::theme_sleek())


# Regional: create prediction grid GOA ---------------------------------------------------------------------
# this grid is maybe too big? Could use it if we wanted to keep it consist with NOAA
predgrid <- read.csv("data-raw/NOAA_predictiongrid/GOAThorsonGrid_Less700m.csv")
predgrid$Shape_Area / 1000000 # 13 km 2
plot(predgrid$Longitude, predgrid$Latitude, col = "red")
names(predgrid) <- tolower(names(predgrid))

# assign marmap depth to points
predgrid2 <- predgrid %>%
  mutate(longitude = shapelongitude, latitude = shapelatitude) |>
  st_as_sf(coords = c("shapelongitude", "shapelatitude"), crs = "+proj=latlon +datum=WGS84") |>
  ## mutate(UTM.lon.m = st_coordinates(center_extent2)[, 1]) %>%
  ## mutate(UTM.lat.m = st_coordinates(center_extent2)[, 2]) %>%
  # mutate(UTM.lon = UTM.lon.m / 1000, UTM.lat = UTM.lat.m / 1000) |>
  ## select(-FID) |>
  distinct(.keep_all = TRUE)

# st_crs(center_extent3) <- GOAcrs

# center_extent4 <- st_transform(center_extent3, crs = "+proj=longlat + datum=WGS84")

# center_extent5 <- center_extent4 %>%
#   mutate(longitude = st_coordinates(center_extent4)[, 1]) %>%
#   mutate(latitude = st_coordinates(center_extent4)[, 2])
st_geometry(predgrid2) <- NULL

attr(predgrid2, "names.attrs") <- NULL
str(predgrid2)

b <- marmap::getNOAA.bathy(lon1 = -180, lon2 = -110, lat1 = 20, lat2 = 80, resolution = 1)

grid <- marmap::get.depth(b, predgrid2[, c("longitude", "latitude")], locator = FALSE) %>%
  mutate(bot_depth = (depth * -1)) %>%
  rename(longitude = lon, latitude = lat) %>%
  filter(bot_depth > 25) %>%
  mutate(logbot_depth = log(bot_depth)) %>%
  inner_join(predgrid2, by = c("longitude" = "longitude", "latitude" = "latitude"))

depthpoints_center[duplicated(depthpoints_center), ] # just checking

# # join the points back to the grid so I can predict on the grid
# grid2 <- st_join(grid_extent2,
#                  st_as_sf(depthpoints_center, coords = c("UTM.lon.m", "UTM.lat.m"), crs = GOAcrs),
#                  join = st_contains
# ) %>%
#   drop_na(logbot_depth) %>%
#   st_drop_geometry()

grid$offset <- 0

gridgoa <- add_utm_columns(grid,
  units = "km",
  ll_names = c("longitude", "latitude"),
  utm_crs = GOAcrs
) %>%
  rename("UTM.lon" = "X", "UTM.lat" = "Y") %>%
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

ggplot(gridgoa, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_raster() # doesn't work, needs to be spaced evenly
ggplot(gridgoa, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_point()

gridgoa2 <- gridgoa %>%
  mutate(across(c(UTM.lon, UTM.lat), round, digits = 2)) # make them evenly spaced
ggplot(gridgoa2, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_raster()

# gridgoa2$shape_area <- as.numeric(gridgoa2$shape_area)
# gridgoa2$UTM.lon <- as.numeric(gridgoa2$UTM.lon)
# gridgoa2$UTM.lat <- as.numeric(gridgoa2$UTM.lat)

saveRDS(gridgoa2, "output/predictiongrid_GoA_13km.rds")

grid2 <- gridgoa2 |>
  st_as_sf(coords = c("UTM.lon.m", "UTM.lat.m"), crs = GOAcrs)
st_write(grid2, "output/grid_GOA_13km.shp", append = FALSE)


# Regional: create prediction grid NWFSC Combo 1 and 2  ---------------------------------------------------
library(nwfscSurvey)
data(availablecells) # from the nwfscSurvey’ version 2.1 package, grid area is ~10000 hectares, 10km2
availablecells
unique(availablecells$Depth.Range)

# create grid that is 2km X 2 km
predgrid_nwfsc0 <- add_utm_columns(availablecells,
  units = "km",
  ll_names = c("Cent.Long", "Cent.Lat"),
  utm_crs = NWFSCcrs
) %>%
  rename("UTM.lon" = "X", "UTM.lat" = "Y") %>%
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

predgrid_nwfsc0_sf <- st_as_sf(predgrid_nwfsc0, coords = c("UTM.lon.m", "UTM.lat.m"), crs = NWFSCcrs)
hulls <- concaveman::concaveman(predgrid_nwfsc0_sf)
plot(hulls)

# convert to 3*3 km grid, here it's in m
grid_spacing <- 3000

polygony <- st_make_grid(hulls, square = T, cellsize = c(grid_spacing, grid_spacing)) %>%
  st_sf() %>%
  mutate(cell_ID = row_number())

center <- st_centroid(polygony)
grid_extent <- st_intersection(st_geometry(hulls), st_geometry(polygony))
center_extent <- st_intersection(st_geometry(hulls), st_geometry(center))

center_extent2 <- st_sf(center_extent)
grid_extent2 <- st_sf(grid_extent)

grid_extent2$area_km <- st_area(grid_extent2) / 1000000 # m to km

# assign depth to points
center_extent3 <- center_extent2 %>%
  mutate(UTM.lon.m = st_coordinates(center_extent2)[, 1]) %>%
  mutate(UTM.lat.m = st_coordinates(center_extent2)[, 2]) %>%
  mutate(UTM.lon = UTM.lon.m / 1000, UTM.lat = UTM.lat.m / 1000) |>
  # select(-FID) |>
  distinct(.keep_all = TRUE)
st_crs(center_extent3) <- NWFSCcrs
center_extent4 <- st_transform(center_extent3, crs = "+proj=longlat + datum=WGS84")
center_extent5 <- center_extent4 %>%
  mutate(longitude = st_coordinates(center_extent4)[, 1]) %>%
  mutate(latitude = st_coordinates(center_extent4)[, 2])
st_geometry(center_extent5) <- NULL

attr(center_extent5, "names.attrs") <- NULL
str(center_extent5)

b <- marmap::getNOAA.bathy(lon1 = -180, lon2 = -110, lat1 = 20, lat2 = 80, resolution = 1)

depthpoints_center <- marmap::get.depth(b, center_extent5[, c("longitude", "latitude")], locator = FALSE) %>%
  mutate(bot_depth = (depth * -1)) %>%
  rename(longitude = lon, latitude = lat) %>%
  filter(bot_depth > 25) %>%
  mutate(logbot_depth = log(bot_depth)) %>%
  inner_join(center_extent5, by = c("longitude" = "longitude", "latitude" = "latitude"))

depthpoints_center[duplicated(depthpoints_center), ] # just checking

# join the points back to the grid so I can predict on the grid
grid2 <- st_join(grid_extent2,
  st_as_sf(depthpoints_center, coords = c("UTM.lon.m", "UTM.lat.m"), crs = NWFSCcrs),
  join = st_contains
) %>%
  drop_na(logbot_depth) %>%
  st_drop_geometry()

grid2$offset <- 0

ggplot(grid2, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_raster() # doesn't work, needs to be spaced evenly
ggplot(grid2, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_point()

gridtlcoastal_ras <- grid2 %>% mutate(across(c(UTM.lon, UTM.lat), round, digits = 2)) # make them evenly spaced
ggplot(gridtlcoastal_ras, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_raster()
gridnew <- gridtlcoastal_ras

gridnew$area_km <- as.numeric(gridnew$area_km)
gridnew$UTM.lon <- as.numeric(gridnew$UTM.lon)
gridnew$UTM.lat <- as.numeric(gridnew$UTM.lat)

attr(gridnew, "names.attrs") <- NULL
str(gridnew)


plot(gridnew$longitude, gridnew$latitude)
saveRDS(gridnew, "output/prediction_grid_nwfsc3km.rds")
grid <- readRDS("output/prediction_grid_nwfsc3km.rds")


# Regional: create prediction grid BC ---------------------------------------------------
grid <- gfplot::synoptic_grid

# Stitched: Prediction grid all regions (don't run, time consuming) --------------------------------------------------
# Ones from github are different resolutions
# create a grid across the entire region that is 4*4km and covers GOA, NWFSC, and BC

library(nwfscSurvey)
data(availablecells) # from the nwfscSurvey’ version 2.1 package, grid area is ~10000 hectares, 10km2
unique(availablecells$Depth.Range)
plot(availablecells$Cent.Long, availablecells$Cent.Lat)
# gridnwfsc <- readRDS("output/prediction_grid_nwfsc2km.rds")
#
goagrid <- read.csv("data-raw/NOAA_predictiongrid/GOAThorsonGrid_Less700m.csv") |> # from Lewis
  rename(Cent.Long = Longitude, Cent.Lat = Latitude) |> mutate(region = "GOA")
glimpse(goagrid)
plot(goagrid$Cent.Long, goagrid$Cent.Lat, col = "red")

gridnwfsc <-
  availablecells %>%
  mutate(survey_name = "NWFSC") %>%
  dplyr::select(Cent.Long, Cent.Lat, survey_name) %>%
  distinct(.keep_all = TRUE) %>%
  add_utm_columns(
    ll_names = c("Cent.Long", "Cent.Lat"),
    units = "km", utm_crs = 32607
  ) %>%
  rename("UTM.lon" = "X", "UTM.lat" = "Y") %>%
  dplyr::select(UTM.lon, UTM.lat, survey_name) |>
  mutate(region = "NWFSC")

gridgoa <-
  goagrid |>
  mutate(survey_name = "GOA") %>%
  dplyr::select(Cent.Long, Cent.Lat, survey_name) %>%
  distinct(.keep_all = TRUE) %>%
  add_utm_columns(
    ll_names = c("Cent.Long", "Cent.Lat"),
    units = "km", utm_crs = 32607
  ) %>%
  rename("UTM.lon" = "X", "UTM.lat" = "Y") %>%
  dplyr::select(UTM.lon, UTM.lat, survey_name) |>
  mutate(region = "GOA")

# where is this from?
bcpred <- gfplot::synoptic_grid |>
  dplyr::select(X, Y, survey) |>
  rename(survey_name = survey) |>
  mutate(longitude = X, latitude = Y, X = X * 1000, Y = Y * 1000) |>
  distinct(.keep_all = TRUE) |>
  st_as_sf(coords = c("X", "Y"), crs = 32609) |>
  st_transform(crs = 32607) |>
  rename("UTM.lon" = "longitude", "UTM.lat" = "latitude") |>
  dplyr::select(UTM.lon, UTM.lat, survey_name) |>
  mutate(region = "BC")

gridbc <- bcpred %>%
  mutate(
    UTM.lat.m = unlist(purrr::map(bcpred$geometry, 2)),
    UTM.lon.m = unlist(purrr::map(bcpred$geometry, 1))
  ) %>%
  mutate(UTM.lon = UTM.lon.m / 1000, UTM.lat = UTM.lat.m / 1000) %>%
  dplyr::select(UTM.lon, UTM.lat, survey_name, region) %>%
  distinct() %>%
  st_drop_geometry()

coastalgrid <- rbind(gridnwfsc, gridbc, gridgoa) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)
ggplot(data = coastalgrid, aes(UTM.lon, UTM.lat, colour = region)) +
  geom_point(size = 0.5)

coastalgrid_sf <- st_as_sf(coastalgrid, coords = c("UTM.lon.m", "UTM.lat.m"), crs = 32607)
hulls <- concaveman::concaveman(coastalgrid_sf)
plot(hulls)
# add a small buffer
hullsb <- st_buffer(hulls, dist = 25000) # 5 km buffer
plot(hullsb)

# convert to 4*4 km grid, here it's in m
grid_spacing <- 4000

polygony <- st_make_grid(hulls, square = T, cellsize = c(grid_spacing, grid_spacing)) %>%
  st_sf() %>%
  mutate(cell_ID = row_number())

center <- st_centroid(polygony)
grid_extent <- st_intersection(st_geometry(hulls), st_geometry(polygony))
center_extent <- st_intersection(st_geometry(hulls), st_geometry(center))

center_extent2 <- st_sf(center_extent)
grid_extent2 <- st_sf(grid_extent)

grid_extent2$area_km <- st_area(grid_extent2) / 1000000 # m to km

# assign depth to points
center_extent3 <- center_extent2 %>%
  mutate(UTM.lon.m = st_coordinates(center_extent2)[, 1]) %>%
  mutate(UTM.lat.m = st_coordinates(center_extent2)[, 2]) %>%
  mutate(UTM.lon = UTM.lon.m / 1000, UTM.lat = UTM.lat.m / 1000) |>
  # select(-FID) |>
  distinct(.keep_all = TRUE)
st_crs(center_extent3) <- 32607
center_extent4 <- st_transform(center_extent3, crs = "+proj=longlat + datum=WGS84")
center_extent5 <- center_extent4 %>%
  mutate(longitude = st_coordinates(center_extent4)[, 1]) %>%
  mutate(latitude = st_coordinates(center_extent4)[, 2])
st_geometry(center_extent5) <- NULL

attr(center_extent5, "names.attrs") <- NULL
str(center_extent5)

b <- marmap::getNOAA.bathy(lon1 = -180, lon2 = -110, lat1 = 20, lat2 = 80, resolution = 1)

depthpoints_center <- marmap::get.depth(b, center_extent5[, c("longitude", "latitude")], locator = FALSE) %>%
  mutate(bot_depth = (depth * -1)) %>%
  rename(longitude = lon, latitude = lat) %>%
  filter(bot_depth > 25) %>%
  mutate(logbot_depth = log(bot_depth)) %>%
  inner_join(center_extent5, by = c("longitude" = "longitude", "latitude" = "latitude"))
depthpoints_center[duplicated(depthpoints_center), ] # just checking

# join the points back to the grid so I can predict on the grid
grid2 <- st_join(grid_extent2,
  st_as_sf(depthpoints_center, coords = c("UTM.lon.m", "UTM.lat.m"), crs = 32607),
  join = st_contains
) %>%
  drop_na(logbot_depth) %>%
  st_drop_geometry()

# grid2 <- grid2 |> mutate(survey_name = "NWFSC.Combo.1")
grid2 <- grid2 |> mutate(survey_name = "syn bc")
# grid2$survey_name <- factor("syn bc", levels = c("syn bc", "GOA_bottom_trawl", "NWFSC.Combo2", "NWFSC.Combo1"))
grid2$offset <- 0

ggplot(grid2, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_raster() # doesn't work, needs to be spaced evenly
ggplot(grid2, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_point(size = 0.25) +
  scale_colour_viridis_c()

gridtlcoastal_ras <- grid2 %>% mutate(across(c(UTM.lon, UTM.lat), round, digits = 2)) # make them evenly spaced
ggplot(gridtlcoastal_ras, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_raster()
gridnew <- gridtlcoastal_ras

gridnew$area_km <- as.numeric(gridnew$area_km)
gridnew$UTM.lon <- as.numeric(gridnew$UTM.lon)
gridnew$UTM.lat <- as.numeric(gridnew$UTM.lat)

attr(gridnew, "names.attrs") <- NULL
str(gridnew)
unique(gridnew$region)

saveRDS(gridnew, "output/prediction_grid_coastaltl_new.rds")
gridnew <- readRDS("output/prediction_grid_coastaltl_new.rds")
ggplot(gridnew, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_tile() +
  facet_wrap(~survey_name)
range(gridnew$depth)

# put the region back in but there has to be a better way
library(PBSdata)
data(major) # from PBSdata

gmas_PIDs <- data.frame(PID = c(1, seq(3, 9, 1)), GMAs = c(
  "5E", "5D", "5C", "5B", "5A",
  "3D", "3C", "4B"
))

gma <- major %>%
  left_join(gmas_PIDs) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_transform(crs = 32607) %>%
  group_by(GMAs) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") |>
  filter(GMAs %in% c("3C", "3D", "4B", "5A", "5B", "5C", "5D"))
plot(st_geometry(gma))

gridnew$UTM.lon.m <- gridnew$UTM.lon * 1000
gridnew$UTM.lat.m <- gridnew$UTM.lat * 1000

gridtest <- st_as_sf(gridnew, coords = c("UTM.lon.m", "UTM.lat.m"), crs = 32607)
gma <- gma |> mutate(region = "BC")
gridregion <- st_join(gridtest, gma)

ggplot(gridregion, aes(UTM.lon, UTM.lat, col = region)) +
  geom_tile()

gridregion <- gridregion |>
  mutate(region = ifelse(is.na(region) == TRUE & UTM.lat > 6085.07, "GOA",
    ifelse(is.na(region) == TRUE & UTM.lat < 5600, "nwfsc",
      region
    )
  ))

ggplot(gridregion, aes(UTM.lon, UTM.lat, col = region)) +
  geom_tile()

gridregion <- gridregion |>
  filter(is.na(region) != TRUE)

st_write(gridregion, "output/coasttrawlgrid_test.shp")
gridregion <- st_drop_geometry(gridregion)
saveRDS(gridregion, "output/prediction_grid_coastaltl_new.rds")
