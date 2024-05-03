# create on database of US/Can sample survey data

# Samples - BC ----------------------------------------------------------------

bcsets <- readRDS("data-raw/data_surveysets.rds")
data_survey_samples <- readRDS("data-raw/data_survey_samples.rds")

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

#some samples don't have set information bc of NAs in depth etc.
saveRDS(samps_bc2, "output/bc_samps.rds")


# Samples - load GOA data -----------------------------------------------------------

goa_sets <- readRDS("output/wrangled_afsc_setsdata.rds")

goa_samps2 <- read.csv("data-raw/goa_dogfish_sex_length.csv")
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
goa_sets |>  #8026, 359 of those are ones with catch_weights > 0 surveys without samples
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



# Samples - NWFSC data -------------------------------------------------------------

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


