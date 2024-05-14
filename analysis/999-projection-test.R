# create a IPHC grid

Coastalcrs = 32609

# library -----------------------------------------------------------------
library(sf)
library(sdmTMB)
library(dplyr)
library(here)
library(tidyverse)


# load data ---------------------------------------------------------------

iphc <- readRDS("output/IPHC_coastdata.rds") %>%
  mutate(UTM.lat.m = UTM.lat * 1000, UTM.lon.m = UTM.lon * 1000)

iphcgrid_sf <- st_as_sf(iphc, coords = c("UTM.lon.m", "UTM.lat.m"), crs = Coastalcrs) #change to coastalcrs object
#hullsa <- concaveman::concaveman(filter(iphcgrid_sf, iphc.reg.area %in% c("2A", "2B", "2C")))
hulls <- concaveman::concaveman(filter(iphcgrid_sf, iphc.reg.area %in% c("3A", "3B", "2C")))
#hulls <- rbind(hullsa, hulls)

hullsb <- st_buffer(hulls, dist = 25000) # 25 km buffer
hullsb <- spatialEco::sf_dissolve(hullsb)

# change resolution here
polygony <- st_make_grid(hullsb, square = T, cellsize = c(3000, 3000)) %>%
  st_sf() %>%
  mutate(cell_ID = row_number())
grid_extent <- st_intersection(st_geometry(hulls), st_geometry(polygony))
grid_extent <- st_sf(grid_extent)
grid_extent$area_km <- st_area(grid_extent) / 1000000 # m to km
range(grid_extent$area_km)

#area of grid in 32609 proj
mean(grid_extent$area_km)
grid_extent <- grid_extent |> mutate(FID = seq(1, n(), 1))
grid_extent$group <- "bcproj"
grid_extent$area_km <- as.numeric(grid_extent$area_km)

#area of grid in 32607 proj
ip07 <- st_transform(grid_extent, crs = 32607)
ip07$area_km_07 <- st_area(ip07) / 1000000 # m to km
ip07$area_km_07 <- as.numeric(ip07$area_km_07)
ip07$group <- "NWUSproj"
range(ip07$area_km)
mean(ip12$area_km)

#plot
grid_extent <- st_drop_geometry(grid_extent)
ip07 <- st_drop_geometry(ip07)

all <- left_join(grid_extent, ip07, by = c ("FID" = "FID"))
ggplot(all, aes(area_km_07, area_km.x)) + geom_point()
all <- all |> mutate(difference = area_km.x - area_km_07)

