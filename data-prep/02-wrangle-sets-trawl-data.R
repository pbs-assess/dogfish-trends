# create on database of US/Can set survey data

# notes -------------------------------------------------------------------
# convert Fork Length to total length
# LPC to LText 1483 3·49 (2·94–4·00) 1·20 (1·20–1·20) 0·98
# LF to LText 876 2·17 (1·35–2·98) 1·10 (1·09–1·11) 0·98
# LTnat to LText 953 1·64 (0·97–2·31) 1·02 (1·01–1·03) 0·98
# Tribuzio et al Life history characteristics of a lightly exploited stock of squalus suckleyi

# BC Total Length, tail extended
# IPHC and AFSC Longline lengths are taken in PCL
# AFSC bottom trawl survey is supposed to be FL (despite their database saying FL or TL) and no one at AFSC will specify what type of TL (extended or natural?)

# From Gertserva et al:
# WCGBT survey: Lengths in WCGBT Survey are total length
# Triennial (2001 and 2004): lengths are total length
# AFSC Slope Survey: Fork lengths
# 1998 Triennial Survey: Fork lengths
# IPHC Survey, the samples were measured as precaudal length (LPC)


# libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(gfplot)
library(here)


# Load - BC data see 01_load-trawl-data.R------------------------------------------------------------

x <- c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG", "HS MSA")

bc <- readRDS("data-raw/data_surveysets.rds") %>%
  filter(survey_abbrev %in% x) %>%
  mutate(julian = lubridate::yday(time_retrieved)) |>
  mutate(geartype = "trawl", fishing_event_id = as.character(fishing_event_id)) |>
  mutate(
    density_kgkm2 = density_kgpm2 * 1000000,
    survey_name = "syn bc",
    logbot_depth = log(depth_m)
  ) |>
  dplyr::select(
    survey_name, year, logbot_depth, fishing_event_id,
    longitude, latitude,
    cpue_kgkm2 = density_kgkm2,
    survey_abbrev, catch_count, julian,
    tow_length_m, doorspread_m, catch_weight, speed_mpm, duration_min, species_common_name
  ) |>
  mutate(area_swept1_m2 = doorspread_m * tow_length_m) |>
  drop_na(doorspread_m) |>
  mutate(area_swept_m2 = ifelse(is.na(area_swept1_m2) == TRUE, speed_mpm * duration_min * doorspread_m, area_swept1_m2))

bc %>%
  group_by(year) %>%
  reframe(cpue_kgkm2_sum = sum(as.numeric(cpue_kgkm2))) |>
  ggplot() +
  geom_point(aes(as.numeric(year), cpue_kgkm2_sum)) +
  geom_line(aes(as.numeric(year), cpue_kgkm2_sum))

# check the values are consistent
range(bc$area_swept1_m2, na.rm = TRUE)
bc |>
  filter(is.na(area_swept1_m2) == TRUE) |>
  reframe(print(range(area_swept_m2)))

ggplot(bc, aes(year, area_swept1_m2)) +
  geom_jitter() +
  geom_jitter(data = bc, aes(year, area_swept_m2), col = "red")
# seem ok

bc |>
  filter(is.na(area_swept1_m2) == TRUE) |>
  tally()

x <- bc |> filter(is.na(doorspread_m) == TRUE)

# calculate area swept
bc <- bc |>
  mutate(bottom_temp_c = NA) |>
  dplyr::select(
    survey_name, julian, survey_abbrev, year, logbot_depth, fishing_event_id, longitude, latitude,
    cpue_kgkm2, catch_weight, catch_count, area_swept_m2, bottom_temp_c
  )

bc[duplicated(bc$fishing_event_id), ] # check for duplication

bc <- bc |> drop_na(logbot_depth)

# Load - GOA data ----
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
    ),
    # date = as_datetime(date),
    date = as.Date(date, format = "%Y-%m-%d HH:MM:SS"),
    year = as.integer(lubridate::year(date)),
    month = as.integer(lubridate::month(date)),
    day = as.integer(lubridate::day(date)),
    julian = lubridate::yday(date),
    latitude = lat_start,
    longitude = round(ifelse(lon_start > 0, -lon_start, lon_start), 4), # some are missing -ve
    longitude_end = ifelse(lon_end > 0, -lon_end, lon_end), # some are missing -ve
    latitude_end = lat_end,
    catch_count = catch_numbers,
    area_swept_m2 = effort * 10000, # was in hectares
    # area_units = "m2",
    survey_abbrev = "GOA",
    cpue_kgkm2 = catch_weight / (area_swept_m2 / 1000000),
    logbot_depth = log(depth_m)
  ) |>
  select(
    fishing_event_id,
    year,
    cpue_kgkm2,
    date, month, day, julian,
    survey_name,
    survey_abbrev,
    vessel,
    latitude,
    julian,
    longitude,
    # latitude_end,
    # longitude_end,
    logbot_depth,
    # performance, # all are non-negative values and therefore considered "satisfactory”
    catch_weight,
    catch_count,
    area_swept_m2,
    bottom_temp_c
  )

filter(goa_sets, is.na(area_swept_m2) == TRUE)

goa_sets[duplicated(goa_sets$fishing_event_id), ] ## check for duplication

saveRDS(goa_sets, "data-raw/wrangled_afsc_setsdata.rds")


# Load - NWFSC data ----
load("data-raw/nwfsc_haul.rda")

catch_nwfsc_combo <- readRDS("data-raw/nwfsc_sets_combo.rds")
catch_nwfsc_triennial <- readRDS("data-raw/nwfsc_sets_triennial.rds")
catch_nwfsc_slope <- readRDS("data-raw/nwfsc_sets_slope.rds")
catch_nwfsc_slope2 <- readRDS("data-raw/nwfsc_sets_slope_AFSC.rds")

nwfsc <- bind_rows(
  catch_nwfsc_combo,
  catch_nwfsc_triennial,
  catch_nwfsc_slope,
  catch_nwfsc_slope2
) |>
  dplyr::rename(
    fishing_event_id = Trawl_id,
    common_name = Common_name,
    catch_count = total_catch_numbers,
    catch_weight = total_catch_wt_kg
  ) |>
  mutate(common_name = tolower(common_name)) %>%
  mutate(date2 = as.Date(Date, format = "%Y%m%d")) %>%
  mutate(dmy = lubridate::ymd(date2)) %>%
  mutate(julian = lubridate::yday(dmy)) %>%
  mutate(survey_name = ifelse(survey_abbrev == "NWFSC.Combo" & julian <= 226, "NWFSC.Combo.pass1",
    ifelse(survey_abbrev == "NWFSC.Combo" & julian > 226, "NWFSC.Combo.pass2",
      survey_abbrev
    )
  )) %>%
  mutate(logbot_depth = log(Depth_m)) |>
  mutate(
    area_swept_m2 = Area_swept_ha * 10000
  )

unique(nwfsc$Project)
names(nwfsc) <- tolower(names(nwfsc))

nwfsc_haul$event_id <- as.character(nwfsc_haul$event_id)
unique(nwfsc_haul$survey_name)
nwfsc <- nwfsc |> inner_join(nwfsc_haul[, c("event_id", "bottom_temp_c")], by = c("fishing_event_id" = "event_id"))

nwfsc_sets <- nwfsc %>%
  dplyr::select(
    date2,
    survey_name, year, julian, survey_abbrev, survey_name, fishing_event_id, longitude_dd, latitude_dd, cpue_kg_km2,
    catch_weight, catch_count, area_swept_m2, logbot_depth, bottom_temp_c
  ) %>%
  rename(
    # station = pass,
    date = date2,
    cpue_kgkm2 = cpue_kg_km2,
    longitude = longitude_dd, latitude = latitude_dd
  )

nwfsc_sets[duplicated(nwfsc_sets$fishing_event_id), ] ## check for duplication

nwfsc_sets |>
  ggplot() +
  geom_point(aes(year, log(catch_weight), colour = survey_name))



# Merge Sets  ----------------------------------------------------

nwfsc_sets$date
goa_sets$date
bc$date

range(bc$cpue_kgkm2)
range(goa_sets$cpue_kgkm2)
range(nwfsc_sets$cpue_kgkm2)

range(goa_sets$area_swept_m2)
range(bc$area_swept_m2, na.rm = TRUE)
range(nwfsc_sets$area_swept_m2, na.rm = TRUE)

range(goa_sets$year)
range(bc$year)
range(nwfsc_sets$year)

range(bc$logbot_depth, na.rm = TRUE)
range(goa_sets$logbot_depth)
range(nwfsc_sets$logbot_depth)

range(nwfsc_sets$catch_weight)
range(bc$catch_weight)
range(goa_sets$catch_weight)

survey_sets <- bind_rows(bc, nwfsc_sets) |>
  bind_rows(goa_sets) |>
  filter(!is.na(catch_weight)) |>
  dplyr::select(
    fishing_event_id,
    survey_abbrev,
    cpue_kgkm2,
    year, # month, day,
    julian,
    survey_name,
    latitude,
    longitude,
    # latitude_end,
    # longitude_end,
    logbot_depth,
    catch_weight,
    catch_count,
    area_swept_m2,
    bottom_temp_c
  )

# Add depth to merged trawl survey data ----------------------------------
survey_sets <- readRDS("output/Wrangled_USCan_trawldata.rds")
survey_sets$logbot_depth_raw <- survey_sets$logbot_depth
survey_sets <- survey_sets |> dplyr::select(-logbot_depth)

max(survey_sets$longitude)
min(survey_sets$longitude)
max(survey_sets$latitude)
min(survey_sets$latitude)

b <- marmap::getNOAA.bathy(lon1 = -180, lon2 = -110, lat1 = 20, lat2 = 80, resolution = 1)
x <- survey_sets |> dplyr::select(longitude, latitude)
survey_sets2 <- x[!duplicated(x), ]

survey_sets3 <- marmap::get.depth(b, survey_sets2[, c("longitude", "latitude")], locator = FALSE) %>%
  mutate(bot_depth = (depth * -1)) %>%
  rename(longitude = lon, latitude = lat) %>%
  mutate(logbot_depth = log(bot_depth)) %>%
  right_join(survey_sets, by = c("longitude" = "longitude", "latitude" = "latitude"))
# NAs are ok, fixing here

survey_sets3[duplicated(survey_sets3), ]

survey_sets3 |>
  filter(is.na(logbot_depth) == TRUE) |>
  tally()
# replace NAs depths with the depth that was in the observational database
# it's 17 points so negligible,

survey_sets3 <- survey_sets3 |>
  mutate(logbot_depth = ifelse(is.na(logbot_depth) == TRUE,
    logbot_depth_raw, logbot_depth
  )) |>
  mutate(bot_depth = ifelse(bot_depth < 0, exp(logbot_depth_raw), bot_depth))


ggplot(survey_sets3, aes(logbot_depth_raw, logbot_depth, colour = survey_abbrev)) +
  geom_point()

saveRDS(survey_sets3, "output/Wrangled_USCan_trawldata_marmapdepth.rds")
