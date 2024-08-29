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

goa_coast <- st_crop(
  map_data,
  c(xmin = -175, ymin = 50, xmax = -130, ymax = 75)
)

ws_coast <- st_crop(
  map_data,
  c(xmin = -150, ymin = 25, xmax = -110, ymax = 55)
)

bc_coast <- st_crop(
  map_data,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)


# IPHC Summary raw by region -----------------------------------------

# d <- readRDS("output/IPHCdata.rds")
d <- readRDS("output/IPHC_coastdata.rds")
d <- d |> dplyr::select(-UTM.lat, -UTM.lon)

# figure for SOM
df_forplotting <- add_utm_columns(d, units = "km", utm_crs = 32612) %>%
  rename("UTM.lon" = "X", "UTM.lat" = "Y")

coast_proj <- sf::st_transform(bc_coast, crs = 32612)


df_forplotting$iphc.reg.area <- factor(df_forplotting$iphc.reg.area,
  levels = c("3B", "3A", "2C", "2B", "2A")
)

d |> filter(iphc.reg.area %in% c("3A", "2C", "3B")) |>
  filter(cpue != 0) |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = cpue), size = 0.5) +
  facet_wrap(~year) +
  geom_sf(data = goa_coast, colour = "grey70", fill = "grey90") +
  theme_classic() +
  scale_colour_viridis_c(trans = "sqrt") +
  # scale_x_continuous(
  #   breaks = c(-160, -140, -120),
  #   labels = c("-160", "-140", "-120"),
    # breaks = c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10),
    # labels = round(c(min(df_forplotting$longitude)+10, -145, max(df_forplotting$longitude)+10), 0),
  #  "Longitude"
  #) +
  labs(colour = "Catch weight\n(log, kg)") +
  # scale_y_continuous("Latitude",
  #   breaks = c(35, 45, 55),
  #   labels = c(35, 45, 55)
  # ) +
  theme(axis.text = element_text(size = 10))


ggplot() +
  geom_point(data = df_forplotting, aes(longitude, latitude)) +
  facet_wrap(~iphc.reg.area, ncol = 1) +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
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
  # scale_y_continuous("Latitude",
  #   breaks = c(35, 45, 55),
  #   labels = c(35, 45, 55)
  # ) +
  theme(axis.text = element_text(size = 10))


ggplot() +
  geom_point(data = df_forplotting, aes(longitude, latitude)) +
  facet_wrap(~iphc.reg.area, ncol = 1) +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
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
  # scale_y_continuous("Latitude",
  #   breaks = c(35, 45, 55),
  #   labels = c(35, 45, 55)
  # ) +
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
   filter(survey_abbrev %in% c("GOA"))
   #filter(survey_name %in% c("syn bc"))
  #filter(survey_name %in% c("NWFSC.Combo", "Triennial", "NWFSC.Slope", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1", "AFSC.Slope"))

unique(df$survey_name)
unique(df$survey_abbrev)
# rm1 <- df |> filter(survey_name == "AFSC.Slope" & year < 1997)
# rm2 <- df |> filter(survey_name == "Triennial" & year < 1995)
# rm <- bind_rows(rm1, rm2)
# df <- filter(df, !fishing_event_id %in% rm$fishing_event_id)

df |> #i don't see any dramamtic shift other than in 1993 that was run later in the year
  filter(julian >= 200) |>
ggplot() +
  geom_point(aes(longitude, latitude,
                            col = (julian),
                            #size = (julian)
  ), alpha = 0.75, size = 0.5) +
  facet_wrap(vars(year)) +
  geom_sf(data = goa_coast, colour = "grey70", fill = "grey90") +
  scale_color_viridis_c(trans = "log") +
  scale_x_continuous(
    "Longitude"
  ) +
  labs(colour = expression(paste("CPUE (kg/", km^"2", ")"))) +
  theme(axis.text = element_text(size = 6))


ggplot() +
  geom_point(data = df, aes(longitude, latitude,
    col = (cpue_kgkm2),
    size = (cpue_kgkm2)
  ), alpha = 0.25) +
  facet_wrap(vars(year)) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(0, "in"),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  ) +
  labs(
    x = "Longitude", y = "Latitude",
    # fill = expression(paste("CPUE (kg/",km^"2",")")) ,
    colour = expression(paste("CPUE (kg/", km^"2", ")")),
    size = expression(paste("CPUE (kg/", km^"2", ")"))
  ) +
  scale_size_continuous(range = c(0.5, 5)) +
  geom_sf(data = goa_coast, colour = "grey70", fill = "grey90") +
  #geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  #geom_sf(data = ws_coast, colour = "grey70", fill = "grey90") +
  scale_color_viridis_c(trans = "log") +
  scale_x_continuous(
    "Longitude"
  ) +
  labs(colour = expression(paste("CPUE (kg/", km^"2", ")"))) +
  theme(axis.text = element_text(size = 6))


ggsave("figs/SummaryPlot_rawtrawl_goa.jpg", width = 10, height = 8)
# ggsave("figs/SummaryPlot_rawtrawl_bc.jpg", width = 10, height = 8)
 ggsave("figs/SummaryPlot_rawtrawl_nwus.jpg", width = 10, height = 8)



# figure by depth, julian, time of day ------------------------------------

df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds")
# i need time in the database
# come back to

# bc data
data_surveysets <- readRDS("output/data_surveysets.rds")
sets_ll <- filter(data_surveysets, survey_abbrev %in% c("HBLL OUT S", "HBLL OUT N")) %>%
  mutate(geartype = "longline")
sets_tl <- filter(data_surveysets, survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG")) %>%
  mutate(geartype = "trawl")
bcsets <- rbind(sets_ll, sets_tl)


bcsets2 <- bcsets |>
  # mutate(date = as.Date(as.character(time_deployed), format = "%Y-%m-%d %H:%M:%S")) |>
  mutate(time = as.numeric(format(time_deployed, format = "%H"))) |>
  drop_na(time) |>
  group_by(year, survey_abbrev) |>
  mutate(
    time_mean = mean(time),
    time_max = max(time),
    time_min = min(time),
    survey_type = geartype
  ) |>
  dplyr::select(
    survey_type, survey_abbrev,
    year, time_mean, time_max, time_min
  )


# load IPHC data with time information
# don't see time on any of the databases


# load NWFSC data
catch2 <- readRDS("output/nwfsc_spinydogfish.rds")
glimpse(catch2)
range(unique(catch2$Year))

# load GOA trawl data
goa_final <- readRDS("output/goadf.rds")

goa_final2 <- goa_final |>
  mutate(date = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M:%S")) |>
  mutate(time = as.numeric(format(date, format = "%H"))) |>
  drop_na(time) |>
  group_by(year) |>
  mutate(
    time_mean = mean(time),
    time_max = max(time),
    time_min = min(time),
    survey_type = "trawl",
    survey_abbrev = "GOA"
  ) |>
  dplyr::select(
    survey_type, survey_abbrev,
    year, time_mean, time_max, time_min
  )


both <- rbind(goa_final2, bcsets2)

# figures
both$survey <- factor(both$survey_abbrev, levels = c(
  "GOA", "HBLL OUT N", "HBLL OUT S", "SYN HS", "SYN GCS",
  "SYN WCHG", "SYN WCVI"
))

ggplot() +
  facet_wrap(~ survey_abbrev + survey_type, nrow = 1) +
  geom_pointrange(
    data = both, mapping = aes(
      x = year, y = time_mean, ymin = time_min,
      ymax = time_max, color = survey_type, group = survey_abbrev
    ),
    size = 0.25, shape = 19
  ) +
  scale_colour_manual(values = c("red", "black"))


ggsave("Figures/time_variability_both.png", width = 13, height = 2)



# Figures SOM julian date and IPHC, trawl surveys -------------------------------------
df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds") |>
  mutate(survey_abbrev2 = ifelse(survey_abbrev == "NWFSC.Combo" & julian <= 226, "NWFSC.Combo.pass1",
    ifelse(survey_abbrev == "NWFSC.Combo" & julian > 226, "NWFSC.Combo.pass2",
      survey_abbrev
    )
  )) |>
  mutate(survey_name = ifelse(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"),
    "syn bc", survey_abbrev2
  ))

trawldate <- df |>
  group_by(survey_name, year) |>
  mutate(
    julian_mean = mean(julian),
    julian_max = max(julian),
    julian_min = min(julian),
    survey_type = "trawl"
  ) |>
  mutate(
    depth_mean = mean(bot_depth ),
    depth_max = max(bot_depth ),
    depth_min = min(bot_depth ),
    survey_type = "trawl"
  ) |>
  dplyr::select(survey_type, survey_abbrev, survey_name, year, julian_mean, julian_max, julian_min,
                depth_mean, depth_max, depth_min)

iphc_coast_trimmed3 <- readRDS("output/IPHC_coastdata.rds")
iphcdate <- iphc_coast_trimmed3 |>
  mutate(survey_name = ifelse(iphc.reg.area %in% c("3A", "3B", "3C"), "Alaska",
    ifelse(iphc.reg.area == "2B", "CAN", "NW US")
  )) |>
  group_by(year, survey_name) |>
  mutate(
    julian_mean = mean(julian),
    julian_max = max(julian),
    julian_min = min(julian),
    survey_type = "IPHC"
  ) |>
  mutate(
    depth_mean = mean(depth_m),
    depth_max = max(depth_m),
    depth_min = min(depth_m),
    survey_type = "IPHC"
  ) |>
  dplyr::select(survey_type, survey_name, year, julian_mean, julian_max, julian_min,
                depth_mean, depth_max, depth_min) |>
  rename(survey_abbrev = survey_name)

both <- rbind(iphcdate, trawldate)

unique(both$survey_abbrev)
ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 121, ymax = 151, alpha = .5, fill = "grey10") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 152, ymax = 181, alpha = .5, fill = "grey30") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 182, ymax = 212, alpha = .5, fill = "grey50") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 213, ymax = 243, alpha = .5, fill = "grey70") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 244, ymax = 273, alpha = .5, fill = "grey90") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 274, ymax = 304, alpha = .5, fill = "grey95") +
  facet_wrap(~ survey_abbrev + survey_type, nrow = 1) +
  geom_pointrange(
    data = both, mapping = aes(x = year, y = julian_mean, ymin = julian_min, ymax = julian_max, color = survey_type),
    size = 0.25, shape = 19
  ) +
  theme_classic() +
  scale_colour_manual(values = c("grey50", "black")) +
  scale_x_continuous("Year") +
  # scale_x_continuous("Year", breaks = c(2005, 2010, 2015), labels = c(2005, 2010, 2015)) +
  scale_y_continuous("Julian date")
ggsave("Figures/JulianDate_variability_both.png", width = 13, height = 2)

ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 121, ymax = 151, alpha = .5, fill = "grey10") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 152, ymax = 181, alpha = .5, fill = "grey30") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 182, ymax = 212, alpha = .5, fill = "grey50") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 213, ymax = 243, alpha = .5, fill = "grey70") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 244, ymax = 273, alpha = .5, fill = "grey90") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 274, ymax = 304, alpha = .5, fill = "grey95") +
  facet_wrap(~ survey_abbrev + survey_type, nrow = 1) +
  geom_pointrange(
    data = both, mapping = aes(x = year, y = julian_mean, ymin = julian_min, ymax = julian_max, color = survey_type),
    size = 0.25, shape = 19
  ) +
  theme_classic() +
  scale_colour_manual(values = c("grey50", "black")) +
  scale_x_continuous("Year") +
  # scale_x_continuous("Year", breaks = c(2005, 2010, 2015), labels = c(2005, 2010, 2015)) +
  scale_y_continuous("Depth (m)")
ggsave("Figures/Depth_variability_both.png", width = 13, height = 2)


#time of survey
bc <- readRDS("data-raw/data_surveysets.rds") |>
  drop_na(time_deployed) |>
  mutate(time = as.numeric(format(time_deployed, format = "%H"))) |>
  drop_na(time) |>
  group_by(year, survey_abbrev) |>
  mutate(
    time_mean = mean(time),
    time_max = max(time),
    time_min = min(time),
    survey_type = "trawl"
  )

#load NWFSC data
#i don't see time in here
# load("data-raw/nwfsc_haul.rda")
#
# catch_nwfsc_combo <- readRDS("data-raw/nwfsc_sets_combo.rds")
# catch_nwfsc_triennial <- readRDS("data-raw/nwfsc_sets_triennial.rds")
# catch_nwfsc_slope <- readRDS("data-raw/nwfsc_sets_slope.rds")
# catch_nwfsc_slope2 <- readRDS("data-raw/nwfsc_sets_slope_AFSC.rds")
#
# nwfsc <- bind_rows(
#   catch_nwfsc_combo,
#   catch_nwfsc_triennial,
#   catch_nwfsc_slope,
#   catch_nwfsc_slope2
# )
# nwfsctest <- nwfsc |>
#   mutate(date = as.POSIXct(Datetime_utc_iso, format = "%m/%d/%Y %H:%M:%S")) |>
#   mutate(time = as.numeric(format(date, format = "%H")))

# load GOA trawl data
goa_all_sets <- readRDS("data-raw/goa-sets.rds")
goa_all_catch <- readRDS("data-raw/goa-catch.rds")

goa_sets <-
  left_join(goa_all_sets, goa_all_catch) %>%
  mutate(
    fishing_event_id = as.character(event_id),
    survey_abbrev = case_when(
      survey_name == "Gulf of Alaska Bottom Trawl Survey" ~ "GOA",
      survey_name == "Aleutian Islands Bottom Trawl Survey" ~ "Aleutian",
      TRUE ~ "Other Alaska"
    )
  ) |>
  mutate(date = as.POSIXct(date, format = "%Y/%m/%d %H:%M:%S")) |>
  mutate(time = as.numeric(format(date, format = "%H"))) |>
  mutate(year = as.numeric(format(date, format = "%Y"))) |>
  drop_na(time) |>
  group_by(year) |>
  mutate(
    time_mean = mean(time),
    time_max = max(time),
    time_min = min(time),
    survey_type = "trawl"
  ) |>
  dplyr::select(
    survey_type, survey_abbrev,
    year, time_mean, time_max, time_min
  )

both <- rbind(goa_sets, bc)

ggplot() +
  facet_wrap(~survey_abbrev + survey_type, nrow = 1) +
  geom_pointrange(
    data = both, mapping = aes(
      x = year, y = time_mean, ymin = time_min,
      ymax = time_max, color = survey_type, group = survey_abbrev
    ),
    size = 0.25, shape = 19
  ) +
  scale_colour_manual(values = c("black"))
ggsave("Figures/time_variability_both.png", width = 13, height = 2)
