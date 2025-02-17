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

coast <- st_crop(
  map_data,
  c(xmin = -170, ymin = 30, xmax = -120, ymax = 65)
)

coast_proj2 <- sf::st_transform(coast, crs = 32609)

goa_coast <- st_crop(
  map_data,
  c(xmin = -175, ymin = 50, xmax = -130, ymax = 75)
)

ws_coast <- st_crop(
  map_data,
  c(xmin = -150, ymin = 30, xmax = -115, ymax = 50)
)

bc_coast <- st_crop(
  map_data,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)


# IPHC Summary raw by region -----------------------------------------

# d <- readRDS("output/IPHC_coastdata.rds")
# d <- d |> dplyr::select(-UTM.lat, -UTM.lon)
#
# # figure for SOM
# df_forplotting <- add_utm_columns(d, units = "km", utm_crs = 32612) %>%
#   rename("UTM.lon" = "X", "UTM.lat" = "Y")
#
# coast_proj <- sf::st_transform(bc_coast, crs = 32612)
#
# df_forplotting$iphc.reg.area <- factor(df_forplotting$iphc.reg.area,
#   levels = c("3B", "3A", "2C", "2B", "2A")
# )
#
# d |>
#   filter(iphc.reg.area %in% c("3A", "2C", "3B")) |>
#   filter(cpue != 0) |>
#   ggplot() +
#   geom_point(aes(longitude, latitude, colour = cpue), size = 0.5) +
#   facet_wrap(~year) +
#   geom_sf(data = goa_coast, colour = "grey70", fill = "grey90") +
#   theme_classic() +
#   scale_colour_viridis_c(trans = "sqrt") +
#   # scale_x_continuous(
#   #   breaks = c(-160, -140, -120),
#   #   labels = c("-160", "-140", "-120"),
#   # breaks = c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10),
#   # labels = round(c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10), 0),
#   #  "Longitude"
#   # ) +
#   labs(colour = "Catch weight\n(log, kg)") +
#   # scale_y_continuous("Latitude",
#   #   breaks = c(35, 45, 55),
#   #   labels = c(35, 45, 55)
#   # ) +
#   theme(axis.text = element_text(size = 10))
#
#
# ggplot() +
#   geom_point(data = df_forplotting, aes(longitude, latitude)) +
#   facet_wrap(~iphc.reg.area, ncol = 1) +
#   geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
#   theme_classic() +
#   scale_colour_viridis_c(trans = "log") +
#   scale_x_continuous(
#     breaks = c(-160, -140, -120),
#     labels = c("-160", "-140", "-120"),
#     # breaks = c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10),
#     # labels = round(c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10), 0),
#     "Longitude"
#   ) +
#   labs(colour = "Catch weight\n(log, kg)") +
#   # scale_y_continuous("Latitude",
#   #   breaks = c(35, 45, 55),
#   #   labels = c(35, 45, 55)
#   # ) +
#   theme(axis.text = element_text(size = 10))
#
#
# ggplot() +
#   geom_point(data = df_forplotting, aes(longitude, latitude)) +
#   facet_wrap(~iphc.reg.area, ncol = 1) +
#   geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
#   theme_classic() +
#   scale_colour_viridis_c(trans = "log") +
#   scale_x_continuous(
#     breaks = c(-160, -140, -120),
#     labels = c("-160", "-140", "-120"),
#     # breaks = c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10),
#     # labels = round(c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10), 0),
#     "Longitude"
#   ) +
#   labs(colour = "Catch weight\n(log, kg)") +
#   # scale_y_continuous("Latitude",
#   #   breaks = c(35, 45, 55),
#   #   labels = c(35, 45, 55)
#   # ) +
#   theme(axis.text = element_text(size = 10))
#
# ggsave("Figures/SummaryPlot_surveyarea_iphc.jpg", width = 5, height = 8)





# IPHC summary data raw figures -------------------------------------------

d <- readRDS("output/IPHC_coastdata.rds")
d <- d |> dplyr::select(-UTM.lat, -UTM.lon)

# figure for SOM
df_forplotting <- add_utm_columns(d, units = "km", utm_crs = 32612) %>%
  rename("UTM.lon" = "X", "UTM.lat" = "Y")

coast_proj <- sf::st_transform(bc_coast, crs = 32612)

df_forplotting$iphc.reg.area <- factor(df_forplotting$iphc.reg.area,
  levels = c("3B", "3A", "2C", "2B", "2A")
)

coastwide <- st_crop(
  map_data,
  c(xmin = -170, ymin = 40, xmax = -115, ymax = 70)
)

ggplot(df_forplotting, aes(longitude, latitude,
  # fill = number.observed,
  colour = number.observed,
  size = number.observed
)) +
  geom_sf(data = coastwide, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(alpha = 0.3) +
  scale_size_continuous(range = c(0.025, 2), guide = "none") +
  facet_wrap(vars(year), ncol = 8) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(0, "in"),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  ) +
  scale_x_continuous(
    breaks = c(-160, -150, -140, -130, -120)
  ) +
  scale_colour_viridis_c(trans = "log", labels = scales::number_format(accuracy = 0.1)) +
  # scale_fill_viridis_c(trans = "log") +
  labs(x = "Longitude", y = "Latitude", fill = "Count (log)", colour = "Count (log)") +
  theme(
    axis.text.y = element_text(size = 10), #<- for nwus
    axis.title = element_text(size = 15),
    legend.text = element_text(angle = -45, size = 10, vjust = 0),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8),
    strip.text = element_text(size = 10),
    legend.key.size = unit(1, "line")
  )

ggsave("figs/SummaryPlot_surveyarea_iphc.jpg", width = 10, height = 8)




# IPHC julian date figure ------------------------------------------------------

p1 <-
  d |>
  # filter(survey_name %in% c("2A")) |>
  ggplot() +
  # geom_histogram(aes((depth_m))) +
  geom_jitter(aes(year, julian), alpha = 0.25) +
  facet_wrap(~iphc.reg.area, nrow = 1) +
  theme(
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
    strip.text = element_text(size = 15),
    legend.key.size = unit(2, "line")
  ) +
  xlab("Year") +
  ylab("Survey timing\n(julian date)")

p2 <-
  d |>
  # filter(survey_name %in% c("2A")) |>
  ggplot() +
  geom_jitter(aes(year, depth_m), alpha = 0.25) +
  facet_wrap(~iphc.reg.area, nrow = 1) +
  theme(
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
    strip.text = element_text(size = 15),
    legend.key.size = unit(2, "line")
  ) +
  xlab("Year") +
  ylab("Depth (m)")

cowplot::plot_grid(p1, p2, labels = c("A", "B", "C"), label_size = 12, nrow = 2, rel_widths = c(2, 2))
# cowplot::plot_grid(
#   cowplot::plot_grid(p3,NULL,
#                      ncol = 2, nrow = 1, rel_widths = c(1, 3)
#   ), p1, p2,
#   nrow = 3,
#   ncol = 1
# )
ggsave("figs/SummaryPlot_iphc-depth-julian.jpg", width = 12, height = 8)



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
  # scale_size_continuous(range = c(0.5, 5)) +
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
# get rid of size legend, or put it on a different line?

# trawl summary by region -----------------------------------

df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds") |>
#  filter(survey_abbrev %in% c("GOA"))
# filter(survey_name %in% c("syn bc"))
 filter(survey_name %in% c("NWFSC.Combo", "Triennial", "NWFSC.Slope", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1", "AFSC.Slope"))

# rming NWFSC surveys?
# rm1 <- df |> filter(survey_name == "AFSC.Slope" & year < 1997)
# rm2 <- df |> filter(survey_name == "Triennial" & year < 1995)
# rm <- bind_rows(rm1, rm2)
# df <- filter(df, !fishing_event_id %in% rm$fishing_event_id)


# df |> #i don't see any dramatic shift other than in 1993 that was run later in the year
# ggplot() +
#   geom_point(aes(longitude, latitude,
#                             col = (julian),
#                             #size = (julian)
#   ), alpha = 0.75, size = 0.5) +
#   facet_wrap(vars(year)) +
#   geom_sf(data = goa_coast, colour = "grey70", fill = "grey90") +
#   scale_color_viridis_c(trans = "log") +
#   scale_x_continuous(
#     "Longitude"
#   ) +
#   theme(axis.text = element_text(size = 6))

ggplot() +
  geom_point(data = df, aes(longitude, latitude,
    col = (cpue_kgkm2),
    size = (cpue_kgkm2)
  ), alpha = 0.25) +
  # facet_wrap(vars(year)) +
  facet_wrap(vars(year), ncol = 7) + #<- for bc
  # facet_wrap(vars(year), ncol = 10) +  #<- for nwus
  labs(
    x = "Longitude", y = "Latitude",
    # fill = expression(paste("CPUE (kg/",km^"2",")")) ,
    colour = expression(paste("CPUE (kg/", km^"2", ")")),
    size = expression(paste("CPUE (kg/", km^"2", ")"))
  ) +
  scale_size_continuous(range = c(0.5, 5), guide = "none") +
  geom_sf(data = goa_coast, colour = "grey70", fill = "grey90") +
  # geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  # geom_sf(data = ws_coast, colour = "grey70", fill = "grey90") +
  scale_color_viridis_c(trans = "log", labels = scales::number_format(accuracy = 0.1)) +
  scale_x_continuous(
    "Longitude"
  ) +
  labs(colour = expression(paste("CPUE (kg/", km^"2", ")"))) +
  # theme(axis.text = element_text(size = 6)) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(0, "in"),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  ) +
  # theme(axis.text.y = element_text(size = 15),
  #       axis.title = element_text(size = 15),
  #       legend.text = element_text(angle = -45, size = 10, vjust = 0),
  #       legend.title = element_text(size = 15),
  #       axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
  #       strip.text = element_text(size = 15),
  #       legend.key.size = unit(1,"line"))#,
  theme(
    axis.text.y = element_text(size = 10), #<- for nwus
    axis.title = element_text(size = 15),
    legend.text = element_text(angle = -45, size = 10, vjust = 0),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8),
    strip.text = element_text(size = 10),
    legend.key.size = unit(1, "line")
  )

ggsave("figs/SummaryPlot_rawtrawl_goa.jpg", width = 10, height = 8)
# ggsave("figs/SummaryPlot_rawtrawl_bc.jpg", width = 10, height = 8)
# ggsave("figs/SummaryPlot_rawtrawl_nwus.jpg", width = 10, height = 8)


# trawl summary by region and disaggregated by survey -----------------------------------
# newer figures
df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds")

df |>
  filter(survey_name %in% c("NWFSC.Combo", "Triennial", "NWFSC.Slope", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1", "AFSC.Slope")) |>
  mutate(survey_name = ifelse(survey_name == "NWFSC.Combo.pass1", "WCGBT pass 1",
    ifelse(survey_name == "NWFSC.Combo.pass2", "WCGBT pass 2", survey_name)
  )) |>
  ggplot(aes(longitude, latitude,
    colour = log(catch_weight), size = catch_weight
  )) +
  geom_point(alpha = 0.75, size = 1.5) +
  scale_colour_viridis_c("Catch weight\n(tonnes, log)\n") +
  facet_grid(survey_name ~ year) +
  theme(
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
    strip.text = element_text(size = 18),
    legend.key.size = unit(2, "line")
  )
ggsave("figs/SummaryPlot_rawtrawl_nwus2.jpg", width = 26, height = 12)

df |>
  filter(survey_name %in% c("syn bc")) |>
  ggplot(aes(longitude, latitude,
    colour = log(catch_weight), size = catch_weight
  )) +
  geom_point(alpha = 0.75, size = 2) +
  scale_colour_viridis_c("Catch weight\n(tonnes, log)\n") +
  facet_grid(survey_abbrev ~ year) +
  theme(
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
    strip.text = element_text(size = 18),
    legend.key.size = unit(2, "line")
  )
ggsave("figs/SummaryPlot_rawtrawl_bc2.jpg", width = 26, height = 12)

df |>
  filter(survey_name %in% c("Gulf of Alaska Bottom Trawl Survey")) |>
  ggplot(aes(longitude, latitude,
    colour = log(catch_weight), size = catch_weight
  )) +
  geom_point(alpha = 0.75, size = 2) +
  scale_colour_viridis_c("Catch weight\n(tonnes, log)\n") +
  facet_grid(~year) +
  theme(
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 25),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
    strip.text = element_text(size = 25),
    legend.key.size = unit(2, "line")
  )
ggsave("figs/SummaryPlot_rawtrawl_goa2.jpg", width = 26, height = 6)


# trawl julian, depth, and latitude figure ------------------------------------------------------

df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds")
df <- df |>
  mutate(region = ifelse(survey_name %in% c("NWFSC.Combo", "Triennial", "NWFSC.Slope", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1", "AFSC.Slope"), "US West Coast", survey_name)) |>
  mutate(survey_name = ifelse(survey_name == "NWFSC.Combo.pass1", "WCGBT pass 1",
    ifelse(survey_name == "NWFSC.Combo.pass2", "WCGBT pass 2", survey_name)
  )) |>
  mutate(region = ifelse(region == "syn bc", "BC",
    ifelse(region == "Gulf of Alaska Bottom Trawl Survey", "Alaska", region)
  ))
df <- df |> dplyr::select(survey_name, survey_abbrev, region, bot_depth, julian, latitude, year)
head(df)

test <- df %>% tidyr::pivot_longer(
  cols = c("julian", "latitude", "bot_depth"),
  names_to = "variable",
  values_to = "value"
)

head(test)
variable2 <- c(
  julian = "Julian",
  bot_depth = "Depth (m)",
  latitude = "Latitude (N)"
)

p1 <-
  test |>
  ggplot() +
  # geom_histogram(aes((depth_m))) +
  geom_jitter(aes(year, value, colour = survey_abbrev), alpha = 0.25) +
  facet_grid(
    rows = vars(variable), col = vars(region), scales = "free_y",
    labeller = labeller(variable = variable2)
  ) +
  scale_colour_viridis_d() +
  theme_bw() +
  ylab(NULL) +
  xlab("Year") +
  labs(colour = "Survey name") +
  theme(
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
    strip.text = element_text(size = 15),
    legend.key.size = unit(2, "line")
  )

p1
ggsave("figs/SummaryPlot_allvars.jpg", width = 12, height = 8)





# trawl julian, depth, and latitude figure ------------------------------------------------------

df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds") |>
  mutate(region = ifelse(survey_name %in% c("NWFSC.Combo", "Triennial", "NWFSC.Slope", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1", "AFSC.Slope"), "US West Coast", survey_name)) |>
  mutate(survey_name = ifelse(survey_name == "NWFSC.Combo.pass1", "WCGBT pass 1",
    ifelse(survey_name == "NWFSC.Combo.pass2", "WCGBT pass 2", survey_name)
  )) |>
  mutate(region = ifelse(region == "syn bc", "BC",
    ifelse(region == "Gulf of Alaska Bottom Trawl Survey", "Alaska", region)
  )) |>
  dplyr::select(survey_name, survey_abbrev, region, bot_depth, julian, latitude, year) |>
  filter(year >= 2003) |>
  filter(!survey_abbrev %in% c("Triennial", "HS MSA"))

test <- df %>% tidyr::pivot_longer(
  cols = c("julian", "latitude", "bot_depth"),
  names_to = "variable",
  values_to = "value"
)

head(test)
variable2 <- c(
  julian = "Julian",
  bot_depth = "Depth (m)",
  latitude = "Latitude (N)"
)

p1 <-
  test |>
  ggplot() +
  # geom_histogram(aes((depth_m))) +
  geom_jitter(aes(year, value, colour = survey_abbrev), alpha = 0.25) +
  facet_grid(
    rows = vars(variable), col = vars(region), scales = "free_y",
    labeller = labeller(variable = variable2)
  ) +
  scale_colour_viridis_d() +
  theme_bw() +
  ylab(NULL) +
  xlab("Year") +
  labs(colour = "Survey name") +
  theme(
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
    strip.text = element_text(size = 15),
    legend.key.size = unit(2, "line")
  )

p1
ggsave("figs/SummaryPlot_allvars_coastalmodel.jpg", width = 12, height = 8)




# # didn't use trawl julian date figure ------------------------------------------------------
#
# df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds")
#
# p1 <-
#   df |>
#   filter(survey_name %in% c("NWFSC.Combo", "Triennial", "NWFSC.Slope", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1", "AFSC.Slope")) |>
#   mutate(survey_name = ifelse(survey_name == "NWFSC.Combo.pass1", "WCGBT pass 1",
#     ifelse(survey_name == "NWFSC.Combo.pass2", "WCGBT pass 2", survey_name)
#   )) |>
#   ggplot() +
#   # geom_histogram(aes((depth_m))) +
#   geom_jitter(aes(year, julian), alpha = 0.25) +
#   facet_wrap(~survey_name, nrow = 1) +
#   theme(
#     axis.text.y = element_text(size = 15),
#     axis.title = element_text(size = 20),
#     legend.text = element_text(size = 20),
#     legend.title = element_text(size = 20),
#     axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
#     strip.text = element_text(size = 15),
#     legend.key.size = unit(2, "line")
#   )
#
# p2 <- df |>
#   filter(survey_name %in% c("syn bc")) |>
#   ggplot() +
#   # geom_histogram(aes((depth_m))) +
#   geom_jitter(aes(year, julian), alpha = 0.25) +
#   facet_wrap(~survey_abbrev, nrow = 1) +
#   theme(
#     axis.text.y = element_text(size = 15),
#     axis.title = element_text(size = 20),
#     legend.text = element_text(size = 20),
#     legend.title = element_text(size = 20),
#     axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
#     strip.text = element_text(size = 15),
#     legend.key.size = unit(2, "line")
#   )
#
# p3 <- df |>
#   filter(survey_name %in% c("Gulf of Alaska Bottom Trawl Survey")) |>
#   ggplot() +
#   # geom_histogram(aes((depth_m))) +
#   geom_jitter(aes(year, julian), alpha = 0.25) +
#   facet_wrap(~survey_abbrev, nrow = 1) +
#   theme(
#     axis.text.y = element_text(size = 15),
#     axis.title = element_text(size = 20),
#     legend.text = element_text(size = 20),
#     legend.title = element_text(size = 20),
#     axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
#     strip.text = element_text(size = 15),
#     legend.key.size = unit(2, "line")
#   )
#
# # cowplot::plot_grid(p1, p2, p3, labels = c('A', 'B', 'C'), label_size = 12, nrow = 3, rel_widths = c(2, 2, 1))
# cowplot::plot_grid(
#   cowplot::plot_grid(p3, NULL,
#     ncol = 2, nrow = 1, rel_widths = c(1, 3)
#   ), p1, p2,
#   nrow = 3,
#   ncol = 1
# )
# ggsave("figs/SummaryPlot_julian.jpg", width = 12, height = 8)
#
#
#
# # didn't use trawl depth variability plot --------------------------------------------
# p1 <-
#   df |>
#   filter(survey_name %in% c("NWFSC.Combo", "Triennial", "NWFSC.Slope", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1", "AFSC.Slope")) |>
#   mutate(survey_name = ifelse(survey_name == "NWFSC.Combo.pass1", "WCGBT pass 1",
#     ifelse(survey_name == "NWFSC.Combo.pass2", "WCGBT pass 2", survey_name)
#   )) |>
#   ggplot() +
#   geom_jitter(aes(year, bot_depth), alpha = 0.25) +
#   # geom_jitter(aes(year, julian)) +
#   facet_wrap(~survey_name, nrow = 1) +
#   ylab("Depth (m)") +
#   xlab("Year") +
#   theme(
#     axis.text.y = element_text(size = 15),
#     axis.title = element_text(size = 20),
#     legend.text = element_text(size = 20),
#     legend.title = element_text(size = 20),
#     axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
#     strip.text = element_text(size = 15),
#     legend.key.size = unit(2, "line")
#   )
#
# p2 <-
#   df |>
#   filter(survey_name %in% c("syn bc")) |>
#   ggplot() +
#   geom_jitter(aes(year, bot_depth), alpha = 0.25) +
#   facet_wrap(~survey_abbrev, nrow = 1) +
#   ylab("Depth (m)") +
#   xlab("Year") +
#   theme(
#     axis.text.y = element_text(size = 15),
#     axis.title = element_text(size = 20),
#     legend.text = element_text(size = 20),
#     legend.title = element_text(size = 20),
#     axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
#     strip.text = element_text(size = 15),
#     legend.key.size = unit(2, "line")
#   )
#
# p3 <-
#   df |>
#   filter(survey_name %in% c("Gulf of Alaska Bottom Trawl Survey")) |>
#   ggplot() +
#   geom_jitter(aes(year, bot_depth), alpha = 0.25) +
#   facet_wrap(~survey_abbrev, nrow = 1) +
#   ylab("Depth (m)") +
#   xlab("Year") +
#   theme(
#     axis.text.y = element_text(size = 15),
#     axis.title = element_text(size = 20),
#     legend.text = element_text(size = 20),
#     legend.title = element_text(size = 20),
#     axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15),
#     strip.text = element_text(size = 15),
#     legend.key.size = unit(2, "line")
#   )
#
# # cowplot::plot_grid(p1, p2, p3, labels = c('A', 'B', 'C'), label_size = 12, nrow = 3, rel_widths = c(2, 2, 1))
# cowplot::plot_grid(
#   cowplot::plot_grid(p3, NULL,
#     ncol = 2, nrow = 1, rel_widths = c(1, 3)
#   ), p1, p2,
#   nrow = 3,
#   ncol = 1
# )
#
# ggsave("figs/SummaryPlot_depth.jpg", width = 12, height = 8)
#
#
#

# didnt use Figures SOM julian date and IPHC, trawl surveys -------------------------------------
# df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds") |>
#   mutate(survey_abbrev2 = ifelse(survey_abbrev == "NWFSC.Combo" & julian <= 226, "NWFSC.Combo.pass1",
#     ifelse(survey_abbrev == "NWFSC.Combo" & julian > 226, "NWFSC.Combo.pass2",
#       survey_abbrev
#     )
#   )) |>
#   mutate(survey_name = ifelse(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"),
#     "syn bc", survey_abbrev2
#   ))
#
# trawldate <- df |>
#   group_by(survey_name, year) |>
#   mutate(
#     julian_mean = mean(julian),
#     julian_max = max(julian),
#     julian_min = min(julian),
#     survey_type = "trawl"
#   ) |>
#   mutate(
#     depth_mean = mean(bot_depth),
#     depth_max = max(bot_depth),
#     depth_min = min(bot_depth),
#     survey_type = "trawl"
#   ) |>
#   dplyr::select(
#     survey_type, survey_abbrev, survey_name, year, julian_mean, julian_max, julian_min,
#     depth_mean, depth_max, depth_min
#   )
#
# iphc_coast_trimmed3 <- readRDS("output/IPHC_coastdata.rds")
# iphcdate <- iphc_coast_trimmed3 |>
#   mutate(survey_name = ifelse(iphc.reg.area %in% c("3A", "3B", "3C"), "Alaska",
#     ifelse(iphc.reg.area == "2B", "CAN", "NW US")
#   )) |>
#   group_by(year, survey_name) |>
#   mutate(
#     julian_mean = mean(julian),
#     julian_max = max(julian),
#     julian_min = min(julian),
#     survey_type = "IPHC"
#   ) |>
#   mutate(
#     depth_mean = mean(depth_m),
#     depth_max = max(depth_m),
#     depth_min = min(depth_m),
#     survey_type = "IPHC"
#   ) |>
#   dplyr::select(
#     survey_type, survey_name, year, julian_mean, julian_max, julian_min,
#     depth_mean, depth_max, depth_min
#   ) |>
#   rename(survey_abbrev = survey_name)
#
# both <- rbind(iphcdate, trawldate)
#
# unique(both$survey_abbrev)
# ggplot() +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 121, ymax = 151, alpha = .5, fill = "grey10") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 152, ymax = 181, alpha = .5, fill = "grey30") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 182, ymax = 212, alpha = .5, fill = "grey50") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 213, ymax = 243, alpha = .5, fill = "grey70") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 244, ymax = 273, alpha = .5, fill = "grey90") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 274, ymax = 304, alpha = .5, fill = "grey95") +
#   facet_wrap(~ survey_abbrev + survey_type, nrow = 1) +
#   geom_pointrange(
#     data = both, mapping = aes(x = year, y = julian_mean, ymin = julian_min, ymax = julian_max, color = survey_type),
#     size = 0.25, shape = 19
#   ) +
#   theme_classic() +
#   scale_colour_manual(values = c("grey50", "black")) +
#   scale_x_continuous("Year") +
#   # scale_x_continuous("Year", breaks = c(2005, 2010, 2015), labels = c(2005, 2010, 2015)) +
#   scale_y_continuous("Julian date")
# ggsave("Figures/JulianDate_variability_both.png", width = 13, height = 2)
#
# ggplot() +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 121, ymax = 151, alpha = .5, fill = "grey10") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 152, ymax = 181, alpha = .5, fill = "grey30") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 182, ymax = 212, alpha = .5, fill = "grey50") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 213, ymax = 243, alpha = .5, fill = "grey70") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 244, ymax = 273, alpha = .5, fill = "grey90") +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 274, ymax = 304, alpha = .5, fill = "grey95") +
#   facet_wrap(~ survey_abbrev + survey_type, nrow = 1) +
#   geom_pointrange(
#     data = both, mapping = aes(x = year, y = julian_mean, ymin = julian_min, ymax = julian_max, color = survey_type),
#     size = 0.25, shape = 19
#   ) +
#   theme_classic() +
#   scale_colour_manual(values = c("grey50", "black")) +
#   scale_x_continuous("Year") +
#   # scale_x_continuous("Year", breaks = c(2005, 2010, 2015), labels = c(2005, 2010, 2015)) +
#   scale_y_continuous("Depth (m)")
# ggsave("Figures/Depth_variability_both.png", width = 13, height = 2)
#
#
# # time of survey
# bc <- readRDS("data-raw/data_surveysets.rds") |>
#   drop_na(time_deployed) |>
#   mutate(time = as.numeric(format(time_deployed, format = "%H"))) |>
#   drop_na(time) |>
#   group_by(year, survey_abbrev) |>
#   mutate(
#     time_mean = mean(time),
#     time_max = max(time),
#     time_min = min(time),
#     survey_type = "trawl"
#   )
#
# # load NWFSC data
# # i don't see time in here
# # load("data-raw/nwfsc_haul.rda")
# #
# # catch_nwfsc_combo <- readRDS("data-raw/nwfsc_sets_combo.rds")
# # catch_nwfsc_triennial <- readRDS("data-raw/nwfsc_sets_triennial.rds")
# # catch_nwfsc_slope <- readRDS("data-raw/nwfsc_sets_slope.rds")
# # catch_nwfsc_slope2 <- readRDS("data-raw/nwfsc_sets_slope_AFSC.rds")
# #
# # nwfsc <- bind_rows(
# #   catch_nwfsc_combo,
# #   catch_nwfsc_triennial,
# #   catch_nwfsc_slope,
# #   catch_nwfsc_slope2
# # )
# # nwfsctest <- nwfsc |>
# #   mutate(date = as.POSIXct(Datetime_utc_iso, format = "%m/%d/%Y %H:%M:%S")) |>
# #   mutate(time = as.numeric(format(date, format = "%H")))
#
# # load GOA trawl data
# goa_all_sets <- readRDS("data-raw/goa-sets.rds")
# goa_all_catch <- readRDS("data-raw/goa-catch.rds")
#
# goa_sets <-
#   left_join(goa_all_sets, goa_all_catch) %>%
#   mutate(
#     fishing_event_id = as.character(event_id),
#     survey_abbrev = case_when(
#       survey_name == "Gulf of Alaska Bottom Trawl Survey" ~ "GOA",
#       survey_name == "Aleutian Islands Bottom Trawl Survey" ~ "Aleutian",
#       TRUE ~ "Other Alaska"
#     )
#   ) |>
#   mutate(date = as.POSIXct(date, format = "%Y/%m/%d %H:%M:%S")) |>
#   mutate(time = as.numeric(format(date, format = "%H"))) |>
#   mutate(year = as.numeric(format(date, format = "%Y"))) |>
#   drop_na(time) |>
#   group_by(year) |>
#   mutate(
#     time_mean = mean(time),
#     time_max = max(time),
#     time_min = min(time),
#     survey_type = "trawl"
#   ) |>
#   dplyr::select(
#     survey_type, survey_abbrev,
#     year, time_mean, time_max, time_min
#   )
#
# both <- rbind(goa_sets, bc)
#
# ggplot() +
#   facet_wrap(~ survey_abbrev + survey_type, nrow = 1) +
#   geom_pointrange(
#     data = both, mapping = aes(
#       x = year, y = time_mean, ymin = time_min,
#       ymax = time_max, color = survey_type, group = survey_abbrev
#     ),
#     size = 0.25, shape = 19
#   ) +
#   scale_colour_manual(values = c("black"))
# ggsave("Figures/time_variability_both.png", width = 13, height = 2)
