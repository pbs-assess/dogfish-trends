
# put US data in same format as our data for use with gfplot functions



# NOTE:
# convert Fork Length to total length
# LPC to LText 1483 3·49 (2·94–4·00) 1·20 (1·20–1·20) 0·98
# LF to LText 876 2·17 (1·35–2·98) 1·10 (1·09–1·11) 0·98
# LTnat to LText 953 1·64 (0·97–2·31) 1·02 (1·01–1·03) 0·98
# Tribuzio et al Life history characteristics of a lightly exploited stock of squalus suckleyi
# BC tail extended
# IPHC and AFSC LL lengths are taken in PCL
# AFSC BTS is supposed to be FL (despite their database saying FL or TL) and no one at AFSC will specify what type of TL (extended or natural?)

# From Gertserva et al:
# WCGBT survey: Lengths in WCGBT Survey are total length
# Triennial (2001 and 2004): legnths are total length
# AFSC Slope Survey: Fork lengths
# 1998 Triennial Survey: folk lengths
# IPHC Survey, the samples were measured as precaudal length (LPC)


# libraries ---------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(gfplot)



# Sets - load BC data see 01-load-Can-data.R------------------------------------------------------------

#data_survey_samples <- readRDS("output/data_survey_samples.rds")
#data_surveysets <- readRDS("output/data_surveysets.rds")

x <- c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG", "HS MSA")

bc <- readRDS("output/data_surveysets.rds") %>%
  filter(survey_abbrev %in% x) %>%
  mutate(julian = lubridate::yday(time_retrieved)) |>
  mutate(geartype = "trawl", fishing_event_id = as.character(fishing_event_id)) |>
  # dplyr::select(
  #   survey_abbrev, trip_id, year, depth_m, fishing_event_id, julina,
  #   longitude, latitude, density_kgpm2, tow_length_m, doorspread_m, catch_weight, catch_count, speed_mpm, duration_min, species_common_name
  # ) %>%
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
  summarize(cpue_kgkm2_sum = sum(as.numeric(cpue_kgkm2))) |>
  ggplot() +
  geom_point(aes(as.numeric(year), cpue_kgkm2_sum)) +
  geom_line(aes(as.numeric(year), cpue_kgkm2_sum))

# check the values are consistent
range(bc$area_swept1_m2, na.rm = TRUE)
x <- bc |>
  filter(is.na(area_swept1_m2) == TRUE) |>
  summarize(print(range(area_swept_m2)))
x
ggplot(bc, aes(year, area_swept1_m2)) +
  geom_jitter() +
  geom_jitter(data = bc, aes(year, area_swept_m2), col = "red")
# seem ok


bc |>
  filter(is.na(area_swept1_m2) == TRUE) |>
  tally()
x <- bc |> filter(is.na(doorspread_m) == TRUE)
unique(x$survey_abbrev) # WCHG doesn't have doorspread so use a different way to calcule the area swept

# calculate area swept
bc <- bc |>
  mutate(bottom_temp_c = NA) |>
  dplyr::select(
    survey_name, julian, survey_abbrev, year, logbot_depth, fishing_event_id, longitude, latitude,
    cpue_kgkm2, catch_weight, catch_count, area_swept_m2, bottom_temp_c
  )

bc[duplicated(bc$fishing_event_id), ] # check for duplication

range(bc$cpue_kgkm2)
range(bc$year)
bc$bottom_temp_c
glimpse(bc)
saveRDS(bc, "output/wrangled_bcdata.rds")

# Sets - load NWFSC data  ---------------------------------------------------------
# nwfsc triennial survey ran from 1980 - 2004 (1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004)
# nwfsc west coast groundfish bottom trawl (WCGFBT) here called combo survey ran from 2003 - 2021 annually except for 2020

# http://pfmc-assessments.github.io/nwfscSurvey/index.html
# ls("package:nwfscSurvey")

# remotes::install_github("pfmc-assessments/nwfscSurvey") #http://pfmc-assessments.github.io/nwfscSurvey/articles/nwfscSurvey.html

# # NWFSC
if (!file.exists("data-raw/nwfscSurvey/nwfsc_sets_slope_AFSC.rds")) {
  # remotes::install_github("pfmc-assessments/nwfscSurvey")
  catch_nwfsc_combo <- nwfscSurvey::PullCatch.fn(SurveyName = "NWFSC.Combo") %>%
    mutate(survey_abbrev = "NWFSC.Combo")
  # survey that has two passes, pass one is May, Jun, July
  dir.create("data-raw/nwfscSurvey", recursive = TRUE, showWarnings = FALSE)
  saveRDS(catch_nwfsc_combo, "data-raw/nwfscSurvey/nwfsc_sets_combo.rds")

  catch_nwfsc_triennial <- nwfscSurvey::PullCatch.fn(SurveyName = "Triennial") %>%
    mutate(survey_abbrev = "Triennial")
  saveRDS(catch_nwfsc_triennial, "data-raw/nwfscSurvey/nwfsc_sets_triennial.rds")

  catch_nwfsc_slope <- nwfscSurvey::PullCatch.fn(SurveyName = "NWFSC.Slope") %>%
    mutate(survey_abbrev = "NWFSC.Slope")
  # There are 1285 records with no area swept calculation. These record will be filled with the mean swept area across all tows.
  saveRDS(catch_nwfsc_slope, "data-raw/nwfscSurvey/nwfsc_sets_slope.rds")

  catch_nwfsc_slope2 <- nwfscSurvey::PullCatch.fn(SurveyName = "AFSC.Slope") %>%
    mutate(survey_abbrev = "AFSC.Slope")
  # There are 48712 records with no area swept calculation. These record will be filled with the mean swept area across all tows.
  saveRDS(catch_nwfsc_slope2, "data-raw/nwfscSurvey/nwfsc_sets_slope_AFSC.rds")
} else {
  catch_nwfsc_combo <- readRDS("data-raw/nwfscSurvey/nwfsc_sets_combo.rds")
  catch_nwfsc_triennial <- readRDS("data-raw/nwfscSurvey/nwfsc_sets_triennial.rds")
  catch_nwfsc_slope <- readRDS("data-raw/nwfscSurvey/nwfsc_sets_slope.rds")
  catch_nwfsc_slope2 <- readRDS("data-raw/nwfscSurvey/nwfsc_sets_slope_AFSC.rds")
}

# to get bottom temp: https://github.com/DFO-NOAA-Pacific/surveyjoin/tree/main/data
#download this from above
load("data-raw/surveyjoin/nwfsc_haul.rda") # fishing event specs
glimpse(nwfsc_haul)

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
  filter(common_name == "pacific spiny dogfish") %>%
  #filter(survey_name %in% c("NWFSC.Combo", "Triennial")) %>%
  mutate(date2 = as.Date(Date, format = "%Y%m%d")) %>%
  mutate(dmy = lubridate::ymd(date2)) %>%
  mutate(julian = lubridate::yday(dmy)) %>%
  mutate(survey_name = ifelse(survey_abbrev == "NWFSC.Combo" & julian <= 226, "NWFSC.Combo.pass1",
    ifelse(survey_abbrev == "NWFSC.Combo" & julian > 226, "NWFSC.Combo.pass2",
 survey_abbrev)
  )) %>%
  mutate(logbot_depth = log(Depth_m) ) |>
  mutate(
    cpue_kgkm2 = CPUE_kg_per_ha * 100,
    area_swept_m2 = Area_Swept_ha * 10000
  ) |> # convert to m2
  dplyr::select(-cpue_kg_km2)

unique(nwfsc$Project)
names(nwfsc) <- tolower(names(nwfsc))

nwfsc_haul$event_id <- as.character(nwfsc_haul$event_id)
unique(nwfsc_haul$survey_name)
nwfsc <- nwfsc |> inner_join(nwfsc_haul[, c("event_id", "bottom_temp_c")], by = c("fishing_event_id" = "event_id") )
# saveRDS(nwfsc_sets, "output/nwfsc_spinydogfish.rds")
# nwfsc_sets <- readRDS("output/nwfsc_spinydogfish.rds")
glimpse(nwfsc)

unique(nwfsc$survey_name2)
unique(nwfsc$survey_abbrev)
unique(nwfsc$survey_name)
unique(nwfsc$Project)

nwfsc3 <- nwfsc %>%
  #dplyr::select(-survey_name) |>
  #rename(survey_name = survey_name2) %>%
  dplyr::select(date2,
    survey_name, year, julian, survey_abbrev, survey_name,  fishing_event_id, longitude_dd, latitude_dd, cpue_kgkm2,
    catch_weight, catch_count, area_swept_m2, logbot_depth, bottom_temp_c
  ) %>%
  rename(
    # station = pass,
    date = date2,
    longitude = longitude_dd, latitude = latitude_dd
  )

range(bc$year)
range(unique(nwfsc3$year))

range(nwfsc3$area_swept_m2)
range(bc$area_swept_m2)

range(nwfsc3$catch_weight)
range(bc$catch_weight)

range(nwfsc3$cpue_kgkm2)
range(bc$cpue_kgkm2)

nwfsc3[duplicated(nwfsc3$fishing_event_id), ] ## check for duplication


saveRDS(nwfsc3, "output/wrangled_nwfsc_setsdata.rds")


# Sets - load GOA data ----
# when goa gets zeros online this is what you would do --------------------
# # query API, for now this gives me non-zero catches. I would have to query in chunks.
# # github: https://github.com/afsc-assessments/afscdata
#
# # see here for documentation on the surveys: https://www.webapps.nwfsc.noaa.gov/assets/25/8655_02272017_093722_TechMemo136.pdf
# # goa triennial survey ran from 1984 - 2019
# # if I download the GOA survey data, it starts at 1993
# # extends back to 1983: https://www.fisheries.noaa.gov/alaska/commercial-fishing/alaska-groundfish-bottom-trawl-survey-data
#
#
# # 1. ref for these data: NOAA Fisheries Alaska Fisheries Science Center. RACE Division Bottom Trawl Survey Data Query, Available at: www.fisheries.noaa.gov/foss, Accessed mm/dd/yyyy
# # from here: https://github.com/afsc-gap-products/gap_public_data#access-the-data
#
# api_link <- "https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey/"
#
# # res <- httr::GET(url = paste0(
# #   api_link, '?q={"survey":{"$like":"%25Gulf%25"}}',
# #   "?offset=0&limit=10000000000"
# # ))
#
# res <- httr::GET(url = paste0(
#   api_link, '?q={"survey":{"$like":"%25Gulf%25"}}',
#   "?offset=0&limit=10000000000",
# ))
#
# res <- httr::GET(url = api_link, query = list(year = "1996", srvy = "GOA"))
#
# data <- jsonlite::fromJSON(base::rawToChar(res$content))
# x <- data$items
# unique(x$year)
#
# # this pulls tows with Dogfish, I want all tows
# res <- httr::GET(url = paste0( # just pull tows with Dogfish, I want all tows
#   api_link, '?q={"common_name":{"$like":"%25dogfish%25"}}',
#   "?offset=0&limit=100000"
# ))
#
# data <- jsonlite::fromJSON(base::rawToChar(res$content))
# res # Test connection
# x <- data$items
# unique(x$srvy)
# goa_api <- filter(x, srvy == "GOA")
# range(goa_api$year)
# saveRDS(goa_api, "data/GOA_Groundfish_Bottom_Trawl_Survey_Data_api.rds")
#
#
# goa_api <- readRDS("data/GOA_Groundfish_Bottom_Trawl_Survey_Data_api.rds")
# goa_api$cpue_kgkm2_orig <- goa_api$cpue_kgkm2
# goa_api$cpue_kgkm2 <- as.numeric(goa_api$cpue_kgkm2)
# range(as.numeric(goa_api$cpue_kgkm2))
# dim(goa_api)
#
#
# # 2 this extends back to 1983, pull data from 1990
# # # data codes and forms https://repository.library.noaa.gov/view/noaa/31570
# # # https://www.fisheries.noaa.gov/alaska/commercial-fishing/alaska-groundfish-bottom-trawl-survey-data
# goa5 <- read.csv("data/NOAA trawl survey data Alaska/Gulf of Alaska Groundfish Bottom Trawl Survey Data raw/goa1990_1999.csv")
#
# goa5sum <- goa5 %>%
#   filter(COMMON == "spiny dogfish") %>%
#   mutate(year = as.numeric(YEAR)) %>%
#   group_by(year) %>%
#   drop_na(WTCPUE) %>%
#   mutate(cpue_kgkm2 = as.numeric(WTCPUE)) %>% # wtcpue is in hectares
#   summarize(sumcpue_kgkm2 = sum(cpue_kgkm2) * 100)
#
# ggplot(data = goa5sum, aes(year, sumcpue_kgkm2)) +
#   geom_point() +
#   geom_line()
#
# goa_1990 <- goa5 %>%
#   filter(COMMON == "spiny dogfish") %>%
#   mutate(year = as.numeric(YEAR)) %>%
#   group_by(year) %>%
#   drop_na(WTCPUE) %>%
#   mutate(cpue_kgkm2 = as.numeric(WTCPUE) * 100) %>%
#   filter(year == 1990) %>%
#   dplyr::select(-YEAR)
#
#
# # merge api with data from website to extend time series to 1990
# goa_api
# goa_apisum <- goa_api %>%
#   group_by(year) %>%
#   summarise(sum = sum(as.numeric(cpue_kgkm2)))
#
# # check that the catches are the same
# plt_api <- ggplot(data = goa_apisum, aes(as.numeric(year), sum)) +
#   geom_point(col = "red", size = 4) +
#   geom_line()
# plt_api + geom_line(
#   data = goa5sum,
#   aes(year, sumcpue_kgkm2), col = "blue", lwd = 2
# ) +
#   geom_point() # lines up
#
# names(goa_1990) <- tolower(names(goa_1990))
# names(goa_api) <- tolower(names(goa_api))
#
# # both depths in m or fathoms?
# goa_1990$bot_depth <- as.numeric(goa_1990$bot_depth)
# range(goa_1990$bot_depth)
# range(as.numeric(goa_api$depth_m)) # depth look the same, checked station depths too
# range(as.numeric(goa_api$station))
#
# goa_ws19902 <- goa_1990 %>%
#   mutate(catch_weight = NA) |>
#   dplyr::select(
#     year, cruise, haul, stratum, station, vessel,
#     datetime, latitude, longitude,
#     cpue_kgkm2, catch_weight, bot_temp, surf_temp, bot_depth
#   )
#
#
# goa_final <- goa_api %>%
#   mutate(weight_kg = as.numeric(weight_kg)) |>
#   mutate(catch_weight = weight_kg) |>
#   dplyr::select(
#     year, cruise, haul, stratum, station, vessel_id,
#     date_time, latitude_dd, longitude_dd,
#     cpue_kgkm2, catch_weight, bottom_temperature_c, surface_temperature_c, depth_m
#   ) %>%
#   rename(
#     vessel = vessel_id,
#     latitude = latitude_dd,
#     longitude = longitude_dd,
#     datetime = date_time,
#     bot_temp = bottom_temperature_c,
#     surf_temp = surface_temperature_c,
#     bot_depth = depth_m
#   ) %>%
#   rbind(goa_ws19902)
#
#
# saveRDS(goa_final, "output/goadf.rds")
# goa_final <- readRDS("output/goadf.rds")
#
#
# glimpse(goa_final)
# ggplot() +
#   geom_violin(data = goa_final, aes(as.factor(year), log(catch_weight))) +
#   geom_jitter(data = goa_final, aes(as.factor(year), log(catch_weight)))
#
# #
# glimpse(goa_final)
# juliangoa <- goa_final |>
#   mutate(date2 = as.character(datetime)) %>%
#   mutate(date3 = as.Date(date2, format = "%m/%d/%Y %H:%M:%S")) %>%
#   mutate(dmy = lubridate::ymd(date3)) %>%
#   mutate(julian = lubridate::yday(dmy))
#
# ggplot(juliangoa, aes(longitude, latitude, colour = julian)) +
#   geom_point() +
#   facet_wrap(~year)
#
# #no zeros in this df


# us this to load GOA data - it has zeros ----------------------------------------------------

#  GOA-sets.rds is missing 0s find them from here: https://github.com/DFO-NOAA-Pacific/surveyjoin/tree/main/data



# load("~/src/surveyjoin/local-data/afsc_haul.rda") # fishing event specs
# load("~/src/surveyjoin/local-data/afsc_catch.rda") # fishing event specs
#
# # load("data-raw/surveyjoin/afsc_haul.rda") # fishing event specs
# # load("data-raw/surveyjoin/afsc_catch.rda")
# # unique(afsc_haul$survey_name)
#
# goa_all_sets <- afsc_haul %>% filter(survey_name %in% c(
#   # "Aleutian Islands Bottom Trawl Survey",
#   "Gulf of Alaska Bottom Trawl Survey"
# ))
#
# goa_all_catch <- afsc_catch %>%
#   filter( # catch_weight > 0 &
#     scientific_name %in% c("Squalus suckleyi")
#   ) %>%
#   mutate(species_common_name = "north pacific spiny dogfish")
#
# saveRDS(goa_all_sets, file = "data-raw/goa-sets.rds")
# saveRDS(goa_all_catch, file = "data-raw/goa-catch.rds")

goa_all_sets <- readRDS("data-raw/goa-sets.rds")
goa_all_catch <- readRDS("data-raw/goa-catch.rds")

goa_sets <- left_join(goa_all_sets, goa_all_catch) %>%
  mutate(
    fishing_event_id = as.character(event_id),
    survey_abbrev = case_when(
      survey_name == "Gulf of Alaska Bottom Trawl Survey" ~ "GOA",
      survey_name == "Aleutian Islands Bottom Trawl Survey" ~ "Aleutian",
      TRUE ~ "Other Alaska"
    ),
    #date = as_datetime(date),
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
    #area_units = "m2",
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
    #cruise,
    #haul,
    # pass,
    latitude,
    julian,
    longitude,
    # latitude_end,
    # longitude_end,
    logbot_depth,
    # performance, # all are non-negative values and therefore considered "satisfactory”
    # species_common_name,
    catch_weight,
    catch_count,
    area_swept_m2,
    bottom_temp_c
  )

filter(goa_sets, is.na(area_swept_m2) == TRUE)

saveRDS(goa_sets, "output/wrangled_afsc_setsdata.rds")



# Sets - merge  ----------------------------------------------------

bc <- readRDS("output/wrangled_bcdata.rds") |> drop_na(logbot_depth)
nwfsc_sets <- readRDS("output/wrangled_nwfsc_setsdata.rds")
nwfsc_sets |>
  ggplot() +
  geom_point(aes(year, log(catch_weight), colour = survey_name))

goa_sets <- readRDS("output/wrangled_afsc_setsdata.rds")
nwfsc_sets$date
goa_sets$date

glimpse(goa_sets)
glimpse(bc)
glimpse(nwfsc_sets)

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

goa_sets[duplicated(goa_sets$fishing_event_id), ] ## check for duplication

str(goa_sets)
str(bc)

survey_sets <- bind_rows(bc, nwfsc_sets) |>
  bind_rows(goa_sets) |>
  filter(!is.na(catch_weight)) |>
  dplyr::select(
    fishing_event_id,
    survey_abbrev,
    # vessel,
    cpue_kgkm2,
    year, # month, day,
    julian,
    survey_name,
    latitude,
    longitude,
    # latitude_end,
    # longitude_end,
    logbot_depth,
    # species_common_name,
    catch_weight,
    catch_count,
    # density_kgpm2,
    # density_kgkm2,
    area_swept_m2,
    bottom_temp_c
    # Subsample_count,
    # Subsample_wt_kg
  )

unique(survey_sets$survey_abbrev)
unique(survey_sets$survey_name)
# unique(survey_sets$survey_name2)

glimpse(survey_sets)
saveRDS(survey_sets, "output/Wrangled_USCanData.rds")

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), log(catch_weight))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_name, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()

ggplot(
  survey_sets,
  aes(as.factor(year), area_swept_m2)
) +
  # aes(as.factor(year), cpue)) +
  geom_jitter() +
  geom_violin() +
  facet_wrap(~survey_abbrev, ncol = 1) # , scales = "free")

dir.create("Figures", showWarnings = FALSE)

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), log(catch_weight))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_name, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()
ggsave("Figures/ExtremeCatchEvents.jpg", width = 10, height = 8)

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), logbot_depth)) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_name, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()
#ggsave("Figures/catch_weight_bydepth.jpg", width = 10, height = 8)

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), (julian))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_abbrev, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()
#ggsave("Figures/catch_weight_byjulian.jpg", width = 10, height = 8)

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), (bottom_temp_c))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_abbrev, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()

ggplot(filter(survey_sets, catch_weight != 0), aes(as.factor(year), (julian))) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_name, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()

ggplot(filter(survey_sets, catch_weight == 0), aes(as.factor(year), logbot_depth)) +
  geom_jitter(aes(size = catch_weight, colour = catch_weight)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~survey_abbrev, nrow = 4) +
  scale_colour_viridis_c(trans = "log") +
  theme_classic()

range(survey_sets$logbot_depth, na.rm = TRUE)
survey_sets <- survey_sets |>
  group_by(survey_abbrev) |>
  mutate(depthsplit = ifelse(logbot_depth < 3.5, "shallow", ifelse(logbot_depth >= 3.5 & logbot_depth < 4, "middle", "deep")))

x <- survey_sets |>
  group_by(year, depthsplit, survey_abbrev) |>
  filter(catch_weight == 0) |>
  tally()
ggplot(x, aes(year, n, colour = depthsplit)) +
  geom_point() +
  geom_line() +
  facet_wrap(~survey_abbrev, scales = "free")
#ggsave("Figures/zeros_by_depth.jpg", width = 10, height = 8)


# add depth to merged observational data ----------------------------------
survey_sets <- readRDS("output/Wrangled_USCanData.rds")
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

survey_sets3[duplicated(survey_sets3), ]
survey_sets3 |> filter(is.na(logbot_depth) == TRUE) |> tally()
#replace NAs depths with the depth that was in the observational database
#it's 17 points so negligible, these have positive logbot depths
# TODO NOTE IN PAPER
survey_sets3 <- survey_sets3 |> mutate(logbot_depth = ifelse(is.na(logbot_depth) == TRUE,
                                                            logbot_depth_raw, logbot_depth)) |>
  mutate(bot_depth = ifelse(bot_depth < 0 , exp(logbot_depth_raw), bot_depth ))

ggplot(survey_sets3, aes(logbot_depth_raw, logbot_depth, colour = survey_abbrev)) + geom_point()
saveRDS(survey_sets3, "output/Wrangled_USCanData.rds")

# Figure of time and data variability -------------------------------------
df <- readRDS("output/Wrangled_USCanData.rds")
glimpse(df)
ggplot(df, aes(year, julian)) + geom_point() + facet_wrap(~survey_abbrev)

# Samples -----------------------------------------------------------------

# NOTE:
# convert Fork Length to total length
# LPC to LText 1483 3·49 (2·94–4·00)  1·20 (1·20–1·20) 0·98
# LF to LText 876 2·17 (1·35–2·98)    1·10 (1·09–1·11) 0·98
# LTnat to LText 953 1·64 (0·97–2·31) 1·02 (1·01–1·03) 0·98
# tribuzio et al Life history characteristics of a lightly exploited stock of squalus suckleyi


# Samples - NWFSC -------------------------------------------------------------
# Samples NWFSC - pull data ---------------------------------------------------------------

if (!file.exists("data-raw/nwfscSurvey/nwfsc_samp_slope_AFSC.rds")) {
  # to download fresh
  nwfsc_samp_combo <- nwfscSurvey::PullBio.fn(
    Name = "Pacific spiny dogfish",
    SurveyName = "NWFSC.Combo"
  )
  # correct apparent typo by matching to other samples from set with matching characteristics
  nwfsc_samp_combo[nwfsc_samp_combo$Date == "1978-May-04", ]$Date <- "2012-Sep-14"
  nwfsc_samp_combo <- nwfsc_samp_combo %>% mutate(date = as.Date(Date))
  saveRDS(nwfsc_samp_combo, "data-raw/nwfscSurvey/nwfsc_samp_combo.rds")

  nwfsc_samp_triennial <- nwfscSurvey::PullBio.fn(
    Name = "Pacific spiny dogfish",
    SurveyName = "Triennial"
  )
  nwfsc_samp_triennial$Lengths <- nwfsc_samp_triennial$Lengths %>% mutate(date = as.Date(Date))
  saveRDS(nwfsc_samp_triennial, "data-raw/nwfsc_samp_triennial.rds")

  # nwfsc_samp_slope <- nwfscSurvey::PullBio.fn(Name = "Pacific spiny dogfish",
  #                                             SurveyName = "NWFSC.Slope") # only returns 4 fish!
  # # https://www.arlis.org/docs/vol1/51990129.pdf - turns out dogfish not sampled individually

  nwfsc_samp_slope_AFSC <- nwfscSurvey::PullBio.fn(
    Name = "Pacific spiny dogfish",
    SurveyName = "AFSC.Slope"
  )
  nwfsc_samp_slope_AFSC$Lengths <- nwfsc_samp_slope_AFSC$Lengths %>% mutate(date = as.Date(Date))
  saveRDS(nwfsc_samp_slope_AFSC, "data-raw/nwfsc_samp_slope_AFSC.rds")
  # nwfsc_samp_slope_AFSC$Lengths %>% group_by(Year) %>% summarise(n = n())

  # nwfsc_samp_shelf <- nwfscSurvey::PullBio.fn(Name = "Pacific spiny dogfish",
  #                                              SurveyName = "NWFSC.Shelf") # no data
} else {
  nwfsc_samp_combo <- readRDS("data-raw/nwfsc_samp_combo.rds")
  nwfsc_samp_triennial <- readRDS("data-raw/nwfsc_samp_triennial.rds")
  nwfsc_samp_slope_AFSC <- readRDS("data-raw/nwfsc_samp_slope_AFSC.rds")
}

nwfsc_samples_raw <- bind_rows(nwfsc_samp_combo, nwfsc_samp_triennial$Lengths, nwfsc_samp_slope_AFSC$Lengths) |>
  dplyr::select(-Date)
saveRDS(nwfsc_samples_raw, "output/NWFSC_biologicaldata.rds")


# Samples - NWFSC modify data -------------------------------------------------------------

nwfsc_samples_raw <- readRDS("output/NWFSC_biologicaldata.rds")
glimpse(nwfsc_samples_raw)

ggplot(nwfsc_samples_raw, aes(Age, Length_cm)) +
  facet_wrap(~Sex + Project     ) +
  geom_point()
ggplot(nwfsc_samples_raw, aes(Length_cm, Weight , col = Project)) +
  facet_wrap(~Sex ) +
  geom_point()
ggplot(nwfsc_samples_raw, aes(Length_cm)) +
  geom_density() +
  facet_wrap(~Sex + Project, ncol = 2     )
names(nwfsc_samples_raw) <- tolower(names(nwfsc_samples_raw))

# data from here: https://www.webapps.nwfsc.noaa.gov/data/map
nwfsc_samps <- nwfsc_samples_raw |>
  filter(common_name == "Pacific spiny dogfish") |> # bc_sets$species_common_name[1] %>%
  # filter(project == "Groundfish Slope and Shelf Combination Survey") %>%
  mutate(weight_grams = weight * 1000,
         sample_id = as.numeric(trawl_id)) |>
  dplyr::select(weight_kg = weight, age,
    common_name, date, depth_m, latitude_dd, longitude_dd,
    survey_name = project, sex, #station_code,
    length_cm, #length_type,
    weight_grams,
    width_cm, year, trawl_id, sample_id
  ) %>%
  #mutate(date = lubridate::ymd(date_yyyymmdd)) %>%
  mutate(julian = lubridate::yday(date)) %>%
  mutate(survey_abbrev = ifelse(julian <= 226 & survey_name == "NWFSC.Combo", "NWFSC.Combo.pass1",
                              ifelse(julian > 226 & survey_name == "NWFSC.Combo", "NWFSC.Combo.pass2",
                                     survey_name))) %>%
  rename(
    longitude = longitude_dd, latitude = latitude_dd,
    fishing_event_id = trawl_id
  ) %>%
  mutate(fishing_event_id = as.numeric(fishing_event_id)) %>%
  # filter(date != "1978-05-04") |> # date doesn't match tow start date, removed %>%
  mutate(sexMF = sex) |>
  mutate(sex = ifelse(sexMF == "M", 1, ifelse(sexMF == "F", 2, 3))) |>
  # mutate(length_Tlnat = length) |>
  # mutate(
  #   length = length_Tlnat * 1.0216, ###DOUBLE CHECK WITH THE NEW SURVEYS
  #   maturity_code = NA, usability_code = NA, maturity_convention_code = NA
  # )


  # From Gertserva et al:
  # WCGBT survey: Lengths in WCGBT Survey are total length (natural)
  # Triennial (2001 and 2004): lengths are total length
  # AFSC Slope Survey: Fork lengths
  # 1998 Triennial Survey: Fork lengths

  mutate(total_length_mmt = ifelse(survey_name == "NWFSC.Combo", "TLnat",
    ifelse(survey_name == "Triennial" & year %in% c(2001, 2004), "TLnat",
      ifelse(survey_name == "AFSC.Slope", "FL",
        ifelse(survey_name == "Triennial" & year == 1998, "FL",
          NA
        )
      )
    )
  )) |>
  mutate(length_ext_cm = ifelse(total_length_mmt == "TLnat", length_cm * 1.02,
    ifelse(total_length_mmt == "FL", length_cm * 1.10 ,
      NA
    )
  )) |>
  mutate(maturity_code = NA, usability_code = NA, maturity_convention_code = NA) |>
  dplyr::select(
    latitude, longitude, sex, length_ext_cm, year, fishing_event_id,
    survey_name, date, maturity_code, maturity_convention_code, usability_code,
    weight_grams, survey_abbrev, sample_id
  )
saveRDS(nwfsc_samps, "output/nwfsc_samps.rds")





# Samples - BC ----------------------------------------------------------------

#pull from 01_load-Can-data.R
bcsets <- readRDS("output/data_surveysets.rds")
data_survey_samples <- readRDS("output/data_survey_samples.rds")

samps_bc <- filter(data_survey_samples, survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG")) %>%
  mutate(geartype = "trawl",  length_type = "extended") %>%
  filter(!is.na(species_common_name) == TRUE) |>
  rename(length_ext_cm = length)
str(samps_bc)

sets_tl2 <- bcsets |>
  mutate(
    survey_name = "BC",
    cpue_kgkm2 = density_kgpm2 * 1000000,
    fishing_event_id = as.numeric(fishing_event_id)
  ) |>
  dplyr::select(
    fishing_event_id, year, longitude, latitude, survey_name, survey_abbrev,
    cpue_kgkm2, catch_weight
  )
glimpse(sets_tl2)


samps_bc2 <- samps_bc |>
  left_join(sets_tl2) |> ##, by = c("fishing_event_id", "year", "survey_abbrev")) |>
  rename(date = trip_start_date) |>
  mutate(survey_name = "BC",
         sample_id = as.numeric(sample_id)) |>
  dplyr::select(age,
    latitude, longitude, sex,sample_id,  length_ext_cm, year, fishing_event_id,
    survey_name, survey_abbrev, date, maturity_code, maturity_convention_code, usability_code, weight_grams = weight
  )

#some samples don't have set information bc of NAsin depth etc.
saveRDS(samps_bc2, "output/bc_samps.rds")


# Samples - GOA  ---------------------------------------------------------------
# Samples - load GOA data -----------------------------------------------------------

# sample data from here: https://github.com/afsc-gap-products/data-requests/issues/62
# set data from 07_USTrawlStich.R but downloaded from: https://github.com/afsc-gap-products/gap_public_data#access-the-data
# see here for defintion of the codes: https://repository.library.noaa.gov/view/noaa/31570

goa_sets <- readRDS("output/wrangled_afsc_setsdata.rds")

goa_samps2 <- read.csv("data-raw/surveyjoin/goa_dogfish_sex_length.csv")
names(goa_samps2) <- tolower(names(goa_samps2))

goa_samps2 <- goa_samps2 |> #haul join is event_id or fishing_event_id
  mutate(length_cm = length / 10) |>
  rename(length_mm = length) |>
  mutate(vessel_cruise_haul = as.numeric(paste0(vessel, cruise, haul)))# length in mm, weight in gms
goa_samps <- goa_samps2[rep(1:nrow(goa_samps2), goa_samps2$frequency), ] # make each length a row

# note: each tow does not have a unique row, see frequency column
unique(goa_samps$frequency)
unique(goa_samps$region)
unique(goa_samps$species_code)
unique(goa_samps$sex) # three codes #1 Male, #2 Female, #3 Undetermined
unique(goa_samps$length_type) # two types #1 fork length from tip of snout to fork of tail,
# 5 total length (doesn't say but natural position)???
unique(goa_samps$sample_type)
filter(goa_samps, sex == 3) |> tally() # 185 undetermined sex, seems like they are females given the density plot and size range
filter(goa_samps, sex == 2) |> tally()
filter(goa_samps, sex == 1) |> tally()

# expand out each tow by the frequency column
ggplot(goa_samps, aes(length_cm, group = as.factor(sex), fill = as.factor(sex))) +
  geom_density(alpha = 0.5)
dim(goa_samps)
sum(goa_samps$frequency)

# checking for differences
p1 <- ggplot(goa_samps, aes(length_cm, group = as.factor(sex), fill = as.factor(sex)), alpha = 0.5) +
  geom_density() +
  facet_wrap(~sex)
p1 + geom_density(data = goa_samps2, aes(length_cm, group = as.factor(sex), colour = as.factor(sex)), alpha = 0.5) +
  facet_wrap(~ as.factor(sex))

goa_samps3 <- goa_samps |>
  mutate(weight_grams = NA) |>
  mutate(length_mmt = ifelse(length_type == 1, "FL",
                             ifelse(length_type == 5, "TL", "NA")
  )) |> # two types #1 fork length from tip of snout to fork of tail,
  # 5 total length (doesn't say but natural position)???
  mutate(length_ext_cm = ifelse(length_mmt == "FL", length_cm * 1.10, length_cm * 1.02),
         hauljoin = as.character(hauljoin), age = NA,
         #     vessel_num = as.character(vessel_num),
              sample_id = vessel_cruise_haul) |>
  dplyr::select(cruisejoin, hauljoin ,sample_id,  age, auditjoin, cruise, haul, length_cm, length_ext_cm, sex, frequency, length_type, vessel, weight_grams)

p1 <- ggplot(goa_samps, aes(length_cm, group = as.factor(sex), fill = as.factor(sex))) +
  geom_density() +
  facet_wrap(~sex)
p1 + geom_density(data = goa_samps3, aes(length_ext_cm, group = as.factor(sex)), fill = "red", alpha = 0.5) +
  facet_wrap(~ as.factor(sex))

glimpse(goa_samps3)
glimpse(goa_sets)

x <-
  goa_sets |>  #8330, 359 of those are ones with catch_weights > 0 surveys without samples
  right_join( goa_samps3, by = c(
  "fishing_event_id" = "hauljoin")) |>
  #dplyr::select(fishing_event_id) |>
  filter(catch_weight !=0) |>
  tally()
range(x$catch_weight)
mean(x$catch_weight)

goa_join <- left_join(goa_sets, goa_samps3, by = c(
  "fishing_event_id" = "hauljoin"), multiple = "all")

dim(goa_join) # some fishing events don't have samples some samples don't have fishing events??


# get the samps database into the same format at nwfsc and bc
goa_samps3 <- goa_join |>
  drop_na(sex) |>
  mutate(
    #fishing_event_id = paste0(haul2, "_", cruise2, "_", vessel2),
    survey_name = "GOA", survey_abbrev = "GOA", maturity_code = NA, usability_code = NA, maturity_convention_code = NA
  ) |>
  mutate(date = as.Date(date, format = "%Y-%m-%d %H:%M:%S"),
         fishing_event_id = as.numeric(fishing_event_id)) |>
  dplyr::select(age,
    latitude, longitude, sex,sample_id, length_ext_cm, year, fishing_event_id,
    survey_name, survey_abbrev, date, maturity_code, maturity_convention_code, usability_code, weight_grams
  )

str(goa_samps3)
range(goa_samps3$date)
dim(goa_samps3)

saveRDS(goa_samps3, "output/goa_samps.rds")


## checks on the alaska data
# # some fishing events don't have samples some samples don't have fishing events??
# ## samples with no sets
#goa_anti <- left_join(goa_samps3, goa_sets, by = c("fishing_event_id", "longitude"))

# #experimentation shows that longitudes don't match between two data sources
#
# goa_bind <- left_join(goa_sets, goa_samples,
#                       by = c("fishing_event_id", "survey_abbrev", "species_common_name", "latitude", "year", "month", "day"),
#                       multiple = "all") #%>%
#
# goa_bind %>% filter(fishing_event_id == -14919) %>% View()
#
#
# goa_bind %>% filter(!is.na(catch_weight2)) %>% View()
#
# goa_bind$lon_diff <- goa_bind$longitude.x - goa_bind$longitude.y
# range(goa_bind$lon_diff, na.rm = TRUE)
#
#
# goa_bind$catch_diff <- goa_bind$catch_weight - goa_bind$catch_weight2
# range(goa_bind$catch_diff, na.rm = TRUE)
#
# goa_bind %>% select(vessel.x, vessel.y) %>% filter(!is.na(vessel.y)) %>% distinct()
# # vessel.x vessel.y
# # 1        Morning Star       57
# # 2         Vesteraalen       94
# # 3           Sea Storm      143
# # 4     Alaska Provider      176
# # 5    Pacific Explorer      159
# # 6           Dominator       23
# # 7           Gladiator      147
# # 8  Northwest Explorer      134
# # 9      Ocean Explorer      148
# # 10      Cape Flattery      178
# 11        Golden Dawn      100




# Samples - merge three regions ----------------------------------------------------

nwfsc_samps <- readRDS("output/nwfsc_samps.rds")
samps_bc <- readRDS("output/bc_samps.rds")
samps_goa <- readRDS("output/goa_samps.rds")

glimpse(samps_bc)
glimpse(nwfsc_samps)
glimpse(samps_goa)

dim(samps_goa)
dim(samps_bc)
dim(nwfsc_samps)

samps <- bind_rows(samps_bc, nwfsc_samps, samps_goa) |> rename(weight = weight_grams)
unique(samps$survey_name)
unique(samps$survey_abbrev)
glimpse(samps)
saveRDS(samps, "output/samps_CoastalTrawl.rds")


