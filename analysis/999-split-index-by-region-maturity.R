# loop over maturity class and region to create index for each maturity class and a depth plot
data("maturity_assignment")
data("maturity_short_names") # males maturity code = 90, female maturity code is >= 77
View(maturity_assignment)
View(maturity_short_names)
# 94 cm tl for females is mature based on DFO assessment
# born at 26 and 27 cm.
# suggest growth of 1.5 cm per year.
# 15 year old dogfish would be about ~50 cm
# Males 70 cm mature
# select 30 or higher for males and 55 or higher for females

# Load libraries ----------------------------------------------------------
library(here)
library(readr)
library(sdmTMB)
library(tidyverse)
library(ggsidekick)
library(sf)
library(terra)
library(raster)
# remotes::install_github("pbs-assess/gfplot")
library(gfplot)
# install.packages("gfplot")
library(tidyverse)
library(TMB)




# Source code   --------
source("source_functions.R")


# Load coast maps ---------------------------------------------------------------------

map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
goa_coast <- sf::st_crop(
  map_data,
  c(xmin = -175, ymin = 50, xmax = -130, ymax = 65)
)
goa_coast_proj <- sf::st_transform(goa_coast, crs = 32607)


map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
coast <- st_crop(
  map_data,
  c(xmin = -170, ymin = 35, xmax = -120, ymax = 60)
)

coast_proj <- sf::st_transform(coast, crs = 32611)
ggplot(coast_proj) +
  geom_sf()


coastwide <- st_crop(
  map_data,
  c(xmin = -170, ymin = 25, xmax = -100, ymax = 70)
)

coast_proj2 <- sf::st_transform(coastwide, crs = 32609)
ggplot(coast_proj2) +
  geom_sf()





# load raw data ---------------------------------------------------------------

# 01-wrangle-US-data.R
survey_samples <- readRDS("output/samps_CoastalTrawl.rds") |>
  drop_na(length_ext_cm) |>
  # some earlier surveys I didn't know the conversion for
  mutate(survey_name2 = ifelse(survey_name == "NWFSC.Combo",
                               survey_abbrev,
                               survey_abbrev
  )) |>
  mutate(fishing_event_id = as.numeric(fishing_event_id)) |>
  mutate(julian = lubridate::yday(date))

survey_sets <- readRDS("output/Wrangled_USCanData.rds") |>
  mutate(survey_name2 = ifelse(survey_abbrev == "NWFSC.Combo",
                               survey_name,
                               survey_abbrev
  )) |>
  mutate(fishing_event_id = as.numeric(fishing_event_id)) |>
  filter(year != 1993)



# exploratory plots -----------------------------------------------------------

# interesting, a shift in the TRIENNIAL size distribution but could be variability in the sampling dates
# the survey dates are fairly consistent but the sampling dates aren't
survey_samples |>
  filter(survey_name2 == "Triennial") |>
  ggplot() +
  geom_density(aes(length_ext_cm)) +
  facet_wrap(~year) +
  geom_vline(xintercept = c(60, 80))

tri <- survey_samples |>
  filter(survey_name2 == "Triennial") |>
  mutate(julian = lubridate::yday(date))

x <- tri |> filter(year == 1998)

tri |>
  filter(julian %in% c(161:197)) |>
  ggplot() +
  geom_point(aes(year, julian))

tri |>
  filter(julian %in% c(161:180)) |>
  ggplot() +
  geom_density(aes(length_ext_cm)) +
  facet_wrap(~year) +
  geom_vline(xintercept = c(60, 80))
# end of triennial

survey_samples |>
  filter(survey_name2 == "GOA") |>
  ggplot() +
  geom_point(aes(longitude, latitude))

test <- survey_samples |>
  filter(survey_name2 == "GOA") |>
  filter(year %in% c(1993, 1996))



# remove sets with no samples (but non-zero catches) ----------------------------------------------

## early GOA years have no samples
# see fishing id 881890 for example
# I need to drop those from this database as
# how many (% wise) fishing events get dropped for each year.
# some have zero catches
sampsFEI <- survey_samples |>
  distinct(survey_samples$fishing_event_id, .keep_all = TRUE)

dim(filter(
  survey_sets,
  fishing_event_id %in% sampsFEI$fishing_event_id
)) # 6,000 fishing events with samples

dim(filter(
  survey_sets,
  !(fishing_event_id %in% sampsFEI$fishing_event_id)
)) # 28000 without but some of these will be zero catches

# distinguish sets with samps/no catch; no samps/no catch; no samps/catch; no samps/catch
survey_sets_withsamps2 <- survey_sets |>
  mutate(nosamps = ifelse(fishing_event_id %in% sampsFEI$fishing_event_id, "samps", "nosamps")) |>
  mutate(catch = ifelse(catch_weight != 0, "catch", "no_catch")) |>
  mutate(samps_catch = ifelse(nosamps == "nosamps" & catch == "catch", "nosamps_catch",
                              ifelse(nosamps == "nosamps" & catch == "no_catch", "nosamps_nocatch",
                                     ifelse(nosamps == "samps" & catch == "catch", "samps_catch",
                                            "samps_nocatch"
                                     )
                              )
  ))
unique(survey_sets_withsamps2$samps_catch)

x <- survey_sets_withsamps2 |>
  group_by(year, survey_abbrev) |>
  mutate(setcount = n()) |>
  filter(samps_catch == "nosamps_catch") |>
  group_by(year, survey_abbrev, setcount) |>
  summarize(count = n()) |>
  mutate(percent = count / setcount * 100)

survey_sets_withsamps <- filter(
  survey_sets_withsamps2,
  samps_catch == "nosamps_nocatch" |
    samps_catch == "samps_catch"
)

survey_sets_NOsamps <- survey_sets_withsamps2 |>
  filter(survey_name %in% c("syn bc", "GOA", "NWFSC")) |>
  filter(survey_abbrev != "HS MSA") |>
  filter(samps_catch == "nosamps_catch")
dim(survey_sets_NOsamps)

survey_sets_NOsamps |>
  group_by(survey_abbrev) |>
  tally()

survey_sets_NOsamps |>
  tally() # 2,158 surveys with catch but NO SAMPS

# range and mean weight of catches of those without samps
range(survey_sets_NOsamps$catch_weight)
mean(survey_sets_NOsamps$catch_weight)
range(survey_sets_NOsamps$year)

# how does the index change without the FEI (fishing event ids) without samps but zero catches still kept
x <- survey_sets |>
  group_by(year, survey_name) |>
  summarize(sum = sum(catch_weight))

test2 <- survey_sets_withsamps |>
  group_by(year, survey_name) |>
  summarize(sum = sum(catch_weight))

p <- ggplot() +
  geom_line(data = test2, aes(year, sum)) +
  geom_point(data = test2, aes(year, sum)) +
  facet_wrap(~survey_name)
p +
  geom_line(data = x, aes(year, sum), col = "red") + geom_point(data = x, aes(year, sum)) +
  facet_wrap(~survey_name, scales = "free")
# triennial is different, early syn bc is different, rest of it looks ok

# drop the first two years of the GOA survey as there aren't enough samples
years <- c(1996, 1999)
survey_sets_withsamps <- filter(survey_sets_withsamps, !(year %in% years))
survey_sets_withsamps |>
  distinct(year, fishing_event_id) |>
  tally()
x <- survey_sets |>
  group_by(year, survey_name) |>
  summarize(sum = sum(catch_weight))

test2 <- survey_sets_withsamps |>
  group_by(year, survey_name) |>
  summarize(sum = sum(catch_weight))
unique(test2$survey_name)

p <- ggplot() +
  geom_line(data = test2, aes(year, sum)) +
  geom_point(data = test2, aes(year, sum)) +
  facet_wrap(~survey_name, scales = "free")
p +
  geom_line(data = x, aes(year, sum), col = "red") + geom_point(data = x, aes(year, sum)) +
  facet_wrap(~survey_name, scales = "free")

survey_sets <- survey_sets_withsamps
saveRDS(survey_sets, "output/Wrangled_USCanData_nosampssetsremoved.rds")


# summaries and diagnostics -----------------------------------------------

survey_sets <- readRDS("output/Wrangled_USCanData_nosampssetsremoved.rds")

## no samples were collected by "NWFSC.Slope" so use "AFSC.Slope" ratios for splitting this catch
## check distribution of years for each survey
survey_sets %>%
  group_by(year, survey_abbrev) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = survey_abbrev, values_from = n) %>%
  View()

survey_samples %>%
  group_by(sex, year, survey_name2) %>%
  tally() |>
  print(n = 131)

# survey_samples %>%
#   group_by(sex, year, survey_abbrev) %>%
#   summarise(n_by_sex = n()) %>%
#   ungroup() %>%
#   group_by(year, survey_abbrev) %>%
#   summarize(n = n()) %>%
#   pivot_wider(names_from = survey_abbrev, values_from = n) %>%
#   View()

raw_sample_ratios <- survey_samples %>%
  group_by(sex, year, survey_abbrev) %>%
  summarise(n_by_sex = n()) %>%
  ungroup() %>%
  group_by(year, survey_abbrev) %>%
  mutate( # summarize
    n = n(),
    F_n = mean(ifelse(sex == 2, n_by_sex, NA), na.rm = TRUE),
    M_n = mean(ifelse(sex == 1, n_by_sex, NA), na.rm = TRUE),
    ratioF = F_n / (F_n + M_n)
  )

# saveRDS(raw_sample_ratios, "output/raw_sex_ratios_across_all_samples.rds")
# raw_sample_ratios %>%
#   pivot_wider(names_from = survey_abbrev, values_from = ratioF) %>% View()



# clean data (sets with no samples (but non-zero catches)) ---------------------------------------------------

survey_samples <- readRDS("output/samps_CoastalTrawl.rds") |>
  drop_na(length_ext_cm) |>
  # some earlier surveys I didn't know the conversion for
  mutate(survey_name2 = ifelse(survey_name == "NWFSC.Combo",
                               survey_abbrev,
                               survey_abbrev
  )) |>
  mutate(fishing_event_id = as.numeric(fishing_event_id)) |>
  mutate(julian = lubridate::yday(date))

survey_sets <- readRDS("output/Wrangled_USCanData_nosampssetsremoved.rds")

#nwfsc slope survey in 1998 has a very different julian date
#I didnt' change anything about that but could by using this code
survey_sets <- survey_sets %>%
  mutate(
    survey_abbrev_true = survey_abbrev,
    # # if using all data, this worked, but all the pre 1998 AFSC samples were from julian > 300
    # survey_abbrev = ifelse(year <= 1998 & survey_abbrev == "NWFSC.Slope", "AFSC.Slope",
    #                        ifelse(year > 1998 & survey_abbrev == "AFSC.Slope", "NWFSC.Slope", survey_abbrev))
    survey_abbrev = ifelse(survey_abbrev == "NWFSC.Slope", "AFSC.Slope", survey_abbrev)
  )

survey_samples <- survey_samples %>%
  mutate(trip_start_date = date) |>
  mutate(julian = lubridate::yday(date))


survey_sets <- survey_sets |>
  dplyr::select(-survey_abbrev) |>
  rename(
    area_swept = area_swept_m2,
    survey_abbrev = survey_name2
  )

survey_samples <- survey_samples |>
  dplyr::select(-survey_abbrev) |>
  rename(
    survey_abbrev = survey_name2,
    length = length_ext_cm
  ) |>
  # weight = weight_kg) |>
  mutate(
    specimen_id = seq(1, n(), 1),
    species_common_name = "spiny dogfish"
  )

# impute weights, calculate ratios, apply ratios   -----------------------------

surveys <- survey_sets |>
  dplyr::select(
    year, survey_abbrev, fishing_event_id, catch_weight, survey_name, area_swept,
    logbot_depth, longitude, latitude
  ) |>
  mutate(survey_name = ifelse(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"), "BC",
                              survey_abbrev
  ))

survey_sets <- survey_sets |>
  mutate(
    survey_name =
      ifelse(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"), "BC",
             survey_abbrev
      )
  )

survey_samples <- survey_samples |>
  mutate(survey_name = ifelse(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"), "BC",
                              survey_abbrev
  ))

# imput missing weights
count <- survey_samples |>
  mutate(survey_id = fishing_event_id, species_code = NA) #|>

# put weight in
count2 <- predict_weight(count)

count2 <- count2 |>
  mutate(weightcomplete = case_when(
    is.na(weight) == TRUE ~ weight_predicted,
    is.na(weight) == FALSE ~ weight
  )) |>
  mutate(survey_name = case_when(
    survey_abbrev == "GOA" ~ "GOA",
    survey_abbrev == "NWFSC.Combo.pass1" ~ "NWFSC.Combo.pass1",
    survey_abbrev == "NWFSC.Combo.pass2" ~ "NWFSC.Combo.pass2",
    survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG") ~ "BC",
    !(survey_abbrev %in% c("GOA", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG")) ~
      survey_abbrev
  ))

# determine maturity length cutoffs
m <- gfplot::fit_mat_ogive(survey_samples,
                           type = "length",
                           sample_id_re = TRUE,
                           custom_maturity_at = c(NA, 55)
)
gfplot::plot_mat_ogive(m)
ggsave("Figures/maturityogives.jpg", width = 4, height = 3)

x <- m$pred_data
x1 <- filter(x, female == 1 & glmm_fe > 0.94)
range(x1$age_or_length)
xx <- filter(x, female == 1 & glmm_fe < 0.05)
range(xx$age_or_length)

# tally by length group and calculate weight of each group
fmat <- filter(x, female == 1 & glmm_fe >= 0.95)
min(fmat$age_or_length)
fimm <- filter(x, female == 1 & glmm_fe < 0.05)
max(fimm$age_or_length)
mmat <- filter(x, female == 0 & glmm_fe > 0.94)
mimm <- filter(x, female == 0 & glmm_fe < 0.05)

count3 <- count2 |>
  filter(sex != 0) |>
  mutate(lengthgroup = case_when(
    length >= max(fimm$age_or_length) & length < min(fmat$age_or_length) & sex == 2 ~ "maturingf",
    length >= max(mimm$age_or_length) & length < min(mmat$age_or_length) & sex == 1 ~ "maturingm",
    length >= min(fmat$age_or_length) & sex == 2 ~ "mf",
    length >= min(mmat$age_or_length) & sex == 1 ~ "mm",
    length < max(fimm$age_or_length) & sex == 2 ~ "imm",
    length < max(mimm$age_or_length) & sex == 1 ~ "imm"
  )) |>
  group_by(year, lengthgroup, survey_name, fishing_event_id) |>
  mutate(sumweightsamps = sum(weightcomplete)) |>
  distinct(year, lengthgroup, .keep_all = TRUE) |>
  dplyr::select(-length, -weight, -sex, -sample_id) |>
  group_by(year, survey_name, fishing_event_id) |>
  mutate(sumtotalweight = sum(sumweightsamps)) |>
  mutate(ratio = sumweightsamps / sumtotalweight * 100) |>
  dplyr::select(-sumtotalweight) |>
  distinct(year, lengthgroup, sumweightsamps, .keep_all = TRUE)

ggplot(count3, aes(year, log(ratio), group = lengthgroup, colour = lengthgroup)) +
  geom_line() +
  facet_wrap(~survey_abbrev)

count3 |>
  filter(survey_abbrev == "GOA") |>
  group_by(year) |>
  summarize(sum = sum(sumweightsamps) / 1000) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum))

# join total catch weight for each year and divide year year by the above ratios
unique(surveys$survey_abbrev)
unique(sort(surveys$year))

sets_summed <- surveys |>
  mutate(survey_name = ifelse(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"), "BC",
                              survey_abbrev)) |>
  group_by(year, survey_abbrev, fishing_event_id, area_swept) |>
  summarize(sum_catch_weight_survey = sum(catch_weight))
unique(sort(sets_summed$year))

sets_summed |>
  filter(survey_abbrev == "GOA") |>
  group_by(year) |>
  summarize(sum = sum(sum_catch_weight_survey)) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum))

# now join
df <- left_join(survey_sets, count3, by = c("year", "survey_abbrev", "fishing_event_id")) |>
  mutate(catch_weight_ratio = catch_weight * (ratio / 100))
unique(df$survey_abbrev)
unique(sort(df$year))

df2 <- df |>
  dplyr::select(year, survey_abbrev, lengthgroup, catch_weight_ratio, fishing_event_id) |>
  mutate(catch_weight_ratio = replace_na(catch_weight_ratio, 0)) |>
  mutate(survey_name = ifelse(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"), "BC",
                              survey_abbrev
  ))

df2 |>
  group_by(year, survey_name, lengthgroup) |>
  summarize(sum = sum(catch_weight_ratio)) |>
  ggplot() +
  geom_line(aes(year, (sum), group = lengthgroup, colour = lengthgroup), size = 2) +
  facet_wrap(~survey_name, scales = "free")

# test GOA
ratio <- df |>
  filter(survey_abbrev_true == "GOA") |>
  dplyr::select(year, fishing_event_id, catch_weight_ratio) |>
  group_by(year) |>
  drop_na(catch_weight_ratio) |>
  mutate(sumratio = sum(catch_weight_ratio))

ggplot(ratio, aes(year, sumratio)) +
  geom_point() +
  geom_line()

cw <- df |>
  filter(survey_abbrev_true == "GOA") |>
  dplyr::select(year, fishing_event_id, catch_weight) |>
  distinct() |>
  group_by(year) |>
  drop_na(catch_weight) |>
  mutate(sumcw = sum(catch_weight))
ggplot(cw, aes(year, sumcw)) +
  geom_point() +
  geom_line(col = "red")
# end of test

length(unique(df2$survey_name))

vect <- unique(df2$survey_name)

dfcomp <- function(df, num) {
  x <- filter(df, survey_name == vect[num])
  y <- as.integer(unique(x$year))
  gr <- unique(x$lengthgroup)
  fe <- unique(x$fishing_event_id)
  d1 <- expand.grid(unique(x$survey_name), unique(y), gr) |>
    rename(
      "year" = "Var2",
      "survey_name" = "Var1",
      "lengthgroup" = "Var3"
    )
  return(d1)
}
cc <- dfcomp(df2, 1)
c1 <- dfcomp(df2, 2)
c2 <- dfcomp(df2, 3)
c3 <- dfcomp(df2, 4)
c4 <- dfcomp(df2, 5)
c5 <- dfcomp(df2, 6)
c6 <- dfcomp(df2, 7)
c7 <- dfcomp(df2, 8)
c8 <- dfcomp(df2, 9)
cf <- rbind(cc, c1, c2, c3, c4, c5, c6, c7, c8) |> drop_na()

uniq <- survey_sets |>
  dplyr::select(year, survey_name, fishing_event_id) |>
  distinct()
cf2 <- left_join(cf, uniq)
unique(sort(uniq$year))

final <- left_join(cf2, surveys)
unique(sort(final$year))

final <- left_join(final, df2) |>
  mutate(catch_weight_ratio = ifelse(is.na(catch_weight_ratio) == TRUE, 0, catch_weight_ratio)) |>
  mutate(survey_name = ifelse(survey_abbrev %in% c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"), "BC",
                              survey_abbrev
  ))

final |>
  group_by(year, survey_name, lengthgroup) |>
  summarize(sum = sum(catch_weight_ratio)) |>
  ggplot() +
  geom_line(aes(year, (sum), group = lengthgroup, colour = lengthgroup), size = 2) +
  facet_wrap(~survey_name, scales = "free")

df2 <- full_join(final, sets_summed, by = c("year", "area_swept", "fishing_event_id", "survey_abbrev"))
unique(sort(df2$year))
saveRDS(df2, "output/splitbymaturityregion_df.rds")
df <- readRDS("output/splitbymaturityregion_df.rds")



