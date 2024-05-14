#Summary plots of surveys for SOM


# library -----------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(sdmTMB)
library(sf)
sf_use_s2(FALSE)


# IPHC psf# IPHC plot ---------------------------------------------------------------
# Load coast maps ---------------------------------------------------------------------

map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
goa_coast <- st_crop(
  map_data,
  c(xmin = -175, ymin = 50, xmax = -130, ymax = 65)
)
goa_coast_proj <- sf::st_transform(goa_coast, crs = 32607)


map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
coast <- st_crop(
  map_data,
  c(xmin = -170, ymin = 35, xmax = -120, ymax = 65))

coast_proj <- sf::st_transform(coast, crs = 32611)
ggplot(coast_proj) +
  geom_sf()


coastwide <- st_crop(
  map_data,
  c(xmin = -170, ymin = 20, xmax = -100, ymax = 70)
)

coast_proj2 <- sf::st_transform(coastwide, crs = 32609)
ggplot(coast_proj2) +
  geom_sf()

coast_proj3 <- sf::st_transform(coastwide, crs = 32607)
ggplot(coast_proj3) +
  geom_sf()

# Rotate the land map
#rotate the map
#to have this match the trawl map use the same coordinates to rotate
#this is from 07_UStrawlstitch
predcoastal_svc2 <- readRDS("output/predcoastal_trawl_svc2.rds")
names(coast_proj2)
ggplot(coast_proj2) +
  geom_sf(aes(fill = rownames(coast_proj2)), color = "black") +
  theme(legend.position = "none")

coast_proj3 <-
  st_cast(
    coast_proj2,
    "POLYGON"
  )
# View(st_geometry(coast_proj3))
plot(st_geometry(coast_proj3))
# plot(st_geometry(coast_proj3)[[1]])

final2 <- st_sf(st_sfc())
for (i in 1:dim(coast_proj3)[1]) {
  rotate_coast3 <- splitrotatepolygon(coast_proj3, 30,
                                      mean(predcoastal_svc2$UTM.lon) * 1000,
                                      mean(predcoastal_svc2$UTM.lat) * 1000)
  final2 <- rbind(final2, rotate_coast3)
}


#this is the rotated map that matches the rotate est and svc maps in 02_TrawlStitch.R
prediction_region <- readRDS("output/prediction_stitchedtrawlmodel.rds")
#pred<- (prediction_region$data)

ggplot() +
  geom_sf(data = coast_proj3, aes(fill = rownames(coast_proj3)), color = "black") +
  theme(legend.position = "none") +
  geom_point(data = prediction_region$data, aes(UTM.lon*1000, UTM.lat*1000))

# make them in the right format
coast_proj4 <-
  st_cast(
    coast_proj3,
    "POLYGON"
  )
# View(st_geometry(coast_proj4))
plot(st_geometry(coast_proj4))
# plot(st_geometry(coast_proj4)[[1]])

final2 <- st_sf(st_sfc())
for (i in 1:dim(coast_proj4)[1]) {
  rotate_coast3 <- splitrotatepolygon(
    coast_proj4,
    30, # for coast
    mean(prediction_region$data$UTM.lon)*1000,
    mean(prediction_region$data$UTM.lat)*1000
  )
  final2 <- rbind(final2, rotate_coast3)
}

rotatelon <- mean(prediction_region$data$UTM.lon)*1000
rotatelat <- mean(prediction_region$data$UTM.lat)*1000


# get the maps on the same scale at the trawl --------------------------------------
#from trawl
prediction_region <- readRDS("output/prediction_stitchedtrawlmodel.rds")

ras_df <- prediction_region$data %>%
  mutate(across(c(UTM.lon, UTM.lat), round, digits = 2)) |>
  dplyr::select(-UTM.lat, -UTM.lon) |>
  add_utm_columns(
    ll_names = c("longitude", "latitude"),
    utm_names = c("UTM.lon", "UTM.lat"), units = "km",
    utm_crs = 32609
  )

# rotate maps so that we get true offshore movement
predimm3 <- rotate_coords(
  ras_df$UTM.lon, ras_df$UTM.lat, 30,
  c(mean(prediction_region$data$UTM.lon),
    mean(prediction_region$data$UTM.lat)
  )) |>
  cbind(ras_df)

plot(final2)
points(predimm3$x*1000, predimm3$y*1000, col = "red")

predimm3$est2 <- round(predimm3$est2, 2)

minlon <- min(predimm3$x)*1000 + 10000
maxlon <- max(predimm3$x)*1000 - 10000
minlat <- min(predimm3$y)*1000 + 8000
maxlat <- max(predimm3$y)*1000 - 10000


# Summary raw survey area for SOM -----------------------------------------

d <- readRDS("output/IPHCdata.rds")
d <- d |> dplyr::select(-UTM.lat, -UTM.lon)

# figure for SOM
df_forplotting <- add_utm_columns(d, units = "km", utm_crs = 32612) %>%
  rename("UTM.lon" = "X", "UTM.lat" = "Y")

coast_proj <- sf::st_transform(coast, crs = 32612)



df_forplotting$iphc.reg.area <- factor(df_forplotting$iphc.reg.area,
                                       levels = c("3B", "3A", "2C", "2B", "2A")
)


ggplot() +
  geom_point(data = df_forplotting, aes(longitude, latitude)) +
  facet_wrap(~iphc.reg.area, ncol = 1) +
  geom_sf(data = coast, colour = "grey70", fill = "grey90") +
  theme_classic() +
  scale_colour_viridis_c(trans = "log") +
  scale_x_continuous(
    breaks = c(-160, -140, -120),
    labels = c("-160", "-140", "-120"),
    # breaks = c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10),
    # labels = round(c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10), 0),
    "Longitude"
  ) +
  labs(colour = "Catch weight\n(log, kg)") +
  scale_y_continuous("Latitude",
                     breaks = c(35, 45, 55),
                     labels = c(35, 45, 55)
  ) +
  theme(axis.text = element_text(size = 10))
ggsave("Figures/SummaryPlot_surveyarea_iphc.jpg", width = 5, height = 8)


