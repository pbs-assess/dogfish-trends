# Summary plots of surveys for SOM


# library -----------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(sdmTMB)
library(sf)
sf_use_s2(FALSE)


# load data ---------------------------------------------------------------

d <- readRDS("output/IPHC_coastdata.rds")
tl <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds")

# Load coast maps ---------------------------------------------------------------------

map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

coastwide <- st_crop(
  map_data,
  c(xmin = -170, ymin = 30, xmax = -120, ymax = 65)
)

coast_proj2 <- sf::st_transform(coastwide, crs = 32609)
ggplot(coast_proj2) +
  geom_sf()


# GOA
goa_coast <- st_crop(
  map_data,
  c(xmin = -175, ymin = 50, xmax = -130, ymax = 75)
)
#goa_coast_proj <- sf::st_transform(goa_coast, crs = 32607) # UTM zone 7

# WC
ws_coast <- st_crop(
  map_data,
  c(xmin = -150, ymin = 25, xmax = -110, ymax = 55)
)
#ws_coast_proj <- sf::st_transform(ws_coast, crs = 32610) # utm zone 10

# BC
bc_coast <- st_crop(
  map_data,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)
#bc_coast_proj <- sf::st_transform(bc_coast, crs = 26909)


# IPHC Summary raw by region -----------------------------------------

# d <- readRDS("output/IPHCdata.rds")
d <- readRDS("output/IPHC_coastdata.rds")
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





# IPHC summary data raw figures -------------------------------------------

ggplot(d, aes(UTM.lon * 1000, UTM.lat * 1000,
  fill = number.observed, colour = number.observed,
  size = number.observed
)) +
  geom_sf(data = coast_proj2, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(alpha = 0.3) +
  scale_size_continuous(range = c(0.025, 2)) +
  facet_wrap(vars(year)) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(0, "in"),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  ) +
  scale_x_continuous(
    breaks = c(-160, -150, -140, -130, -120)
  ) +
  # scale_colour_viridis_c() +
  # scale_fill_viridis_c() +
  scale_colour_viridis_c(trans = "log") +
  scale_fill_viridis_c(trans = "log") +
  # labs(x = "Longitude", y = "Latitude", fill = "Adjusted CPUE", colour = "Adjusted CPUE")
  labs(x = "Longitude", y = "Latitude", fill = "Count (log)", colour = "Count (log)")

ggsave("Figures/SummaryPlot_surveyarea_iphc.jpg", width = 5, height = 8)




# Trawl summary figure ----------------------------------------------------

tl <- add_utm_columns(tl, units = "km", utm_crs = 32609) %>%
  rename("UTM.lon" = "X", "UTM.lat" = "Y")

# coast_proj <- sf::st_transform(coast, crs = 32612)


ggplot(tl, aes(longitude, latitude,
  fill = cpue_kgkm2, colour = cpue_kgkm2,
  size = cpue_kgkm2
)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(alpha = 0.3) +
  scale_size_continuous(range = c(0.5, 5)) +
  facet_wrap(vars(year)) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(0, "in"),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  ) +
  scale_x_continuous(
    breaks = c(-180, -160, -150, -140, -130, -120)
  ) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  # scale_colour_viridis_c(trans = "log") +
  # scale_fill_viridis_c(trans = "log") +
  labs(x = "Longitude", y = "Latitude", fill = "Adjusted CPUE", colour = "Adjusted CPUE")


# trawl summary by region -----------------------------------


df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds") |>
  # filter(survey_abbrev %in% c("GOA"))
  # filter(survey_name %in% c("syn bc"))
   filter(survey_name %in% c("NWFSC.Combo", "Triennial", "NWFSC.Slope","NWFSC.Combo.pass2", "NWFSC.Combo.pass1"))

ggplot() +
  geom_point(data = df, aes(longitude, latitude,
    col = (cpue_kgkm2),
    size = (cpue_kgkm2)
  ), alpha = 0.75) +
  facet_wrap(vars(year)) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(0, "in"),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  ) +
  labs(x = "Longitude", y = "Latitude",
       #fill = expression(paste("CPUE (kg/",km^"2",")")) ,
       colour = expression(paste("CPUE (kg/",km^"2",")")) ,
       size = expression(paste("CPUE (kg/",km^"2",")")))  +
  scale_size_continuous(range = c(0.5, 5)) +
  #geom_sf(data = goa_coast, colour = "grey70", fill = "grey90") +
  #geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_sf(data = ws_coast, colour = "grey70", fill = "grey90") +
  scale_color_viridis_c(trans = "log") +
  scale_x_continuous(
    "Longitude"
  ) +
  labs(colour = expression(paste("CPUE (kg/",km^"2",")"))) +
  theme(axis.text = element_text(size = 6))


# ggsave("Figures/SummaryPlot_rawtrawl_goa.jpg", width = 10, height = 8)
# ggsave("Figures/SummaryPlot_rawtrawl_bc.jpg", width = 10, height = 8)
 ggsave("Figures/SummaryPlot_rawtrawl_nwus.jpg", width = 10, height = 8)
