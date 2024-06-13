# create on database of US/Can sample survey data
library(tidyverse)

# Samples - BC ----------------------------------------------------------------

bcsets <- readRDS("data-raw/data_surveysets.rds")
data_survey_samples <- readRDS("data-raw/data_survey_samples.rds")

samps_bc <- filter(data_survey_samples, survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG")) %>%
  mutate(geartype = "trawl", length_type = "extended") %>%
  filter(!is.na(species_common_name) == TRUE) |>
  rename(length_ext_cm = length)

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

samps_bc2 <- samps_bc |>
  left_join(sets_tl2) |> ## , by = c("fishing_event_id", "year", "survey_abbrev")) |>
  rename(date = trip_start_date) |>
  mutate(
    survey_name = "BC",
    sample_id = as.numeric(sample_id)
  ) |>
  dplyr::select(age,
                latitude, longitude, sex, sample_id, length_ext_cm, year, fishing_event_id,
                survey_name, survey_abbrev, date, maturity_code, maturity_convention_code, usability_code,
                weight_grams = weight
  )

# some samples don't have set information bc of NAs in depth etc.
saveRDS(samps_bc2, "data-raw/bc_samps.rds")


# Samples - GOA data -----------------------------------------------------------

goa_sets <- readRDS("data-raw/wrangled_afsc_setsdata.rds")
goa_samps2 <- read.csv("data-raw/goa_dogfish_sex_length.csv")
names(goa_samps2) <- tolower(names(goa_samps2))

# note: each tow does not have a unique row, see frequency column
unique(goa_samps2$frequency)
unique(goa_samps2$region)
unique(goa_samps2$species_code)
unique(goa_samps2$sex) # three codes #1 Male, #2 Female, #3 Undetermined
unique(goa_samps2$length_type) # two types #1 fork length from tip of snout to fork of tail,
# 5 total length (doesn't say but natural position)???
unique(goa_samps2$sample_type)
filter(goa_samps2, sex == 3) |> tally() # 185 undetermined sex, seems like females
filter(goa_samps2, sex == 2) |> tally()
filter(goa_samps2, sex == 1) |> tally()

# expand out each tow by the frequency column
goa_samps2 <- goa_samps2 |> # haul join is event_id or fishing_event_id
  mutate(length_cm = length / 10) |>
  rename(length_mm = length) |>
  mutate(vessel_cruise_haul = as.numeric(paste0(vessel, cruise, haul))) # length in mm, weight in gms
goa_samps <- goa_samps2[rep(1:nrow(goa_samps2), goa_samps2$frequency), ] # make each length a row

ggplot(goa_samps, aes(length_cm, group = as.factor(sex), fill = as.factor(sex))) +
  geom_density(alpha = 0.5)
dim(goa_samps)
sum(goa_samps$frequency)

# change length to TLext
goa_samps3 <- goa_samps |>
  mutate(weight_grams = NA) |>
  mutate(length_mmt = ifelse(length_type == 1, "FL",
                             ifelse(length_type == 5, "TL", "NA")
  )) |> # two types #1 fork length from tip of snout to fork of tail,
  # 5 total length (doesn't say but natural position)???
  mutate(
    length_ext_cm = ifelse(length_mmt == "FL", length_cm * 1.10, length_cm * 1.02),
    hauljoin = as.character(hauljoin), age = NA,
    sample_id = vessel_cruise_haul
  ) |>
  dplyr::select(cruisejoin, hauljoin, sample_id, age, auditjoin, cruise, haul, length_cm, length_ext_cm, sex, frequency, length_type, vessel, weight_grams)

# plot TL and TL ext
p1 <- ggplot(goa_samps, aes(length_cm, group = as.factor(sex), fill = as.factor(sex))) +
  geom_density() +
  facet_wrap(~sex)
p1 + geom_density(data = goa_samps3, aes(length_ext_cm, group = as.factor(sex)), fill = "red", alpha = 0.5) +
  facet_wrap(~ as.factor(sex))

# checks for number of samples with sets and vic versa
goa_sets |> # 8026 most of the sets caught dogfish
  right_join(goa_samps3, by = c(
    "fishing_event_id" = "hauljoin"
  )) |>
  # dplyr::select(fishing_event_id) |>
  filter(catch_weight != 0) |>
  tally()

goa_sets |> # catch_weights = 0
  left_join(goa_samps3, by = c(
    "fishing_event_id" = "hauljoin"
  )) |>
  filter(catch_weight == 0) |>
  tally()

goa_sets |> # catch_weights = 0 and no samples
  left_join(goa_samps3, by = c(
    "fishing_event_id" = "hauljoin"
  )) |>
  filter(catch_weight == 0 & is.na(length_cm) == TRUE) |>
  tally()

goa_sets |> # catch_weights = 0 but samples?
  left_join(goa_samps3, by = c(
    "fishing_event_id" = "hauljoin"
  )) |>
  filter(catch_weight == 0 & !is.na(length_cm) == TRUE) |>
  tally()

goa_sets |> # catch_weights > 0 and no samples
  left_join(goa_samps3, by = c(
    "fishing_event_id" = "hauljoin"
  )) |>
  filter(catch_weight > 0 & is.na(length_cm) == TRUE) |>
  tally() # 359 have catch weights > 0 and no samples

goa_join <- left_join(goa_sets, goa_samps3, by = c(
  "fishing_event_id" = "hauljoin"
), multiple = "all")
dim(goa_join)
length(unique(goa_join$fishing_event_id)) # matches original dataset

# get the samps database into the same format at nwfsc and bc
goa_samps3 <- goa_join |>
  drop_na(sex) |>
  mutate(
    # fishing_event_id = paste0(haul2, "_", cruise2, "_", vessel2),
    survey_name = "GOA", survey_abbrev = "GOA", maturity_code = NA, usability_code = NA, maturity_convention_code = NA
  ) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d %H:%M:%S"),
    fishing_event_id = as.numeric(fishing_event_id)
  ) |>
  dplyr::select(
    age,
    latitude, longitude, sex, sample_id, length_ext_cm, year, fishing_event_id,
    survey_name, survey_abbrev, date, maturity_code, maturity_convention_code, usability_code, weight_grams
  )

saveRDS(goa_samps3, "data-raw/goa_samps.rds")

# Samples - NWFSC data -------------------------------------------------------------

nwfsc_samples_raw <- readRDS("data-raw/NWFSC_sampledata.rds")
glimpse(nwfsc_samples_raw)
names(nwfsc_samples_raw) <- tolower(names(nwfsc_samples_raw))

ggplot(nwfsc_samples_raw, aes(age, length_cm)) +
  facet_wrap(~ sex + project) +
  geom_point()
ggplot(nwfsc_samples_raw, aes(length_cm, weight, col = project)) +
  facet_wrap(~sex) +
  geom_point()
ggplot(nwfsc_samples_raw, aes(length_cm)) +
  geom_density() +
  facet_wrap(~sex, ncol = 2)
names(nwfsc_samples_raw) <- tolower(names(nwfsc_samples_raw))

# data from here: https://www.webapps.nwfsc.noaa.gov/data/map
nwfsc_samps <- nwfsc_samples_raw |>
  filter(common_name == "Pacific spiny dogfish") |> # bc_sets$species_common_name[1] %>%
  # filter(project == "Groundfish Slope and Shelf Combination Survey") %>%
  mutate(
    weight_grams = weight * 1000,
    sample_id = as.numeric(trawl_id)
  ) |>
  dplyr::select(
    weight_kg = weight, age,
    common_name, date, depth_m, latitude_dd, longitude_dd,
    survey_name = project, sex, # station_code,
    length_cm, # length_type,
    weight_grams,
    width_cm, year, trawl_id, sample_id
  ) %>%
  mutate(julian = lubridate::yday(date)) %>%
  mutate(survey_abbrev = ifelse(julian <= 226 & survey_name == "NWFSC.Combo", "NWFSC.Combo.pass1",
                                ifelse(julian > 226 & survey_name == "NWFSC.Combo", "NWFSC.Combo.pass2",
                                       survey_name
                                )
  )) %>%
  rename(
    longitude = longitude_dd, latitude = latitude_dd,
    fishing_event_id = trawl_id
  ) %>%
  mutate(fishing_event_id = as.numeric(fishing_event_id)) %>%
  mutate(sexMF = sex) |>
  mutate(sex = ifelse(sexMF == "M", 1, ifelse(sexMF == "F", 2, 3))) |>
  # From Gertserva et al:
  # WCGBT survey: Lengths in WCGBT Survey are total length (natural)
  # Triennial (2001 and 2004): lengths are total length
  # AFSC Slope Survey: Fork lengths
  # 1998 Triennial Survey: Fork lengths
  mutate(total_length_mmt = ifelse(survey_name == "Groundfish Slope and Shelf Combination Survey", "TLnat",
                                   ifelse(survey_name == "Groundfish Triennial Shelf Survey" & year %in% c(2001, 2004), "TLnat",
                                          ifelse(survey_name == "AFSC/RACE Slope Survey", "FL",
                                                 ifelse(survey_name == "Groundfish Triennial Shelf Survey" & year <= 1998, "FL",
                                                        NA
                                                 )
                                          )
                                   )
  )) |>
  mutate(length_ext_cm = ifelse(total_length_mmt == "TLnat", length_cm * 1.02,
                                ifelse(total_length_mmt == "FL", length_cm * 1.10,
                                       NA
                                )
  )) |>
  mutate(maturity_code = NA, usability_code = NA, maturity_convention_code = NA) |>
  dplyr::select(
    latitude, longitude, sex, length_ext_cm, year, fishing_event_id,
    survey_name, date, maturity_code, maturity_convention_code, usability_code,
    weight_grams, survey_abbrev, sample_id
  )

saveRDS(nwfsc_samps, "data-raw/nwfsc_samps.rds")


# Samples - merge three regions ----------------------------------------------------

nwfsc_samps <- readRDS("data-raw/nwfsc_samps.rds")
samps_bc <- readRDS("data-raw/bc_samps.rds")
samps_goa <- readRDS("data-raw/goa_samps.rds")

glimpse(samps_bc)
glimpse(nwfsc_samps)
glimpse(samps_goa)

dim(samps_goa)
dim(samps_bc)
dim(nwfsc_samps)

samps <- bind_rows(samps_bc, nwfsc_samps, samps_goa) |> rename(weight = weight_grams)
unique(samps$survey_name)
unique(samps$survey_abbrev)

saveRDS(samps, "output/samps_CoastalTrawl.rds")