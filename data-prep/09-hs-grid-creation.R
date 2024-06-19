
library(tidyverse)
# theme_set(gfplot::theme_pbs())
# library(gfplot)
# library(gfdata)
library(sf)

source("data-prep/00-set-crs.R")

df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds") |>
  filter(survey_abbrev == "HS MSA") |> sdmTMB::add_utm_columns(utm_crs = coast_crs, units = "km")

g <- readRDS("output/prediction_grid_coastaltl_new.rds")

range(g$UTM.lon)

g <- filter(g, UTM.lon < 500, UTM.lon >-500)
g_sf <- st_as_sf(g, coords = c("UTM.lon", "UTM.lat"), crs = coast_crs)


plot(st_geometry(g_sf))

# convex polygon
d_sf <- st_as_sf(df, coords = c("X", "Y"), crs = coast_crs)
plot(st_geometry(d_sf))
hull <- concaveman::concaveman(d_sf)
# I am going to create a buffer
hull_buf <- st_buffer(hull, 8)
plot(hull_buf)

grid_hs <- st_intersection(hull_buf, g_sf)

plot(st_geometry(grid_hs))
plot(st_geometry(d_sf), col = "red", add = TRUE)

grid_hs$survey_name <- "msa bc"
grid_hs$survey_abbrev <- "HS MSA"
# grid_hs$region <- "BC"

grid_hs <- st_drop_geometry(grid_hs)
saveRDS(grid_hs, "output/prediction_grid_hs.rds")
