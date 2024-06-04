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
hullsa <- concaveman::concaveman(filter(iphcgrid_sf, iphc.reg.area %in% c("2A", "2B", "2C")))
hulls <- concaveman::concaveman(filter(iphcgrid_sf, iphc.reg.area %in% c("3A", "3B", "2C")))
hulls <- rbind(hullsa, hulls)

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


#####Calc areas with diff projections
iphcreg <- st_read("data-raw/IPHC_RegulatoryAreas_PDC.shp")
iphcreg_p <- st_transform(iphcreg, crs = Coastalcrs)

grid_extent1 <- st_intersection(grid_extent, iphcreg_p)
grid_extent1 <- grid_extent1 |>
                mutate(region = ifelse(ET_ID == "2B", "BC",
                                       ifelse(ET_ID == "2A", "NWUS",
                                              ifelse(ET_ID %in% c("2C", "3A", "3B"), "Alaska",
                                                     ifelse (ET_ID  == "4A", "SOG", "NA")))))



#area of grid in 32607 proj
ip04 <- st_transform(grid_extent1, crs = 32604)
ip04$area_km_04 <- st_area(ip04) / 1000000 # m to km
ip04 <- ip04 |>
        group_by(region) |>
  reframe(area = sum(as.numeric(area_km_04))) |>
  mutate(group = "Alaskaproj")
ip04[1,]$area/ip04[2,]$area
ip04[1,]$area/ip04[3,]$area
ip04[2,]$area/ip04[3,]$area

#area of grid in 32609 proj
ip09 <- st_transform(grid_extent1, crs = 32609)
ip09$area_km_09 <- st_area(ip09) / 1000000 # m to km
ip09 <- ip09 |>
  group_by(region) |>
  reframe(area = sum(as.numeric(area_km_09))) |>
  mutate(group = "BCproj")
ip09[1,]$area/ip09[2,]$area
ip09[1,]$area/ip09[3,]$area
ip09[2,]$area/ip09[3,]$area

#area of grid in 32610 proj
ip10 <- st_transform(grid_extent1, crs = 32610)
ip10$area_km_10 <- st_area(ip10) / 1000000 # m to km
ip10 <- ip10 |>
  group_by(region) |>
  reframe(area = sum(as.numeric(area_km_10))) |>
  mutate(group = "NWUSproj")
ip10[1,]$area/ip10[2,]$area
ip10[1,]$area/ip10[3,]$area
ip10[2,]$area/ip10[3,]$area

#area of grid in 32611 proj (southern cali/mexico utm zone)
ip11 <- st_transform(grid_extent1, crs = 32611)
ip11$area_km_11 <- st_area(ip11) / 1000000 # m to km
ip11 <- ip11 |>
  group_by(region) |>
  reframe(area = sum(as.numeric(area_km_11))) |>
  mutate(group = "NWUSproj")
ip11[1,]$area/ip11[2,]$area
ip11[1,]$area/ip11[3,]$area
ip11[2,]$area/ip11[3,]$area

a <- c(ip04[1,]$area/ip04[2,]$area,
       ip04[1,]$area/ip04[3,]$area,
       ip04[2,]$area/ip04[3,]$area)
b <- c(ip09[1,]$area/ip09[2,]$area,
       ip09[1,]$area/ip09[3,]$area,
       ip09[2,]$area/ip09[3,]$area)
c <- c(ip10[1,]$area/ip10[2,]$area,
       ip10[1,]$area/ip10[3,]$area,
       ip10[2,]$area/ip10[3,]$area)
d <- c(ip11[1,]$area/ip11[2,]$area,
       ip11[1,]$area/ip11[3,]$area,
       ip11[2,]$area/ip11[3,]$area)

df <- data.frame(a,b,c, d)
colnames(df) = c('proj32604', "proj32609", "proj32610", "proj32611")
rownames(df) <- c('GOA/BC','GOA/NW US','BC/NW US')
df

