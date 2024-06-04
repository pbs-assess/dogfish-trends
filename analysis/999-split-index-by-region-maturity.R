# loop over maturity class and region to create index for each maturity class and a depth plot

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





# notes -------------------------------------------------------------------
data("maturity_assignment")
data("maturity_short_names") # males maturity code = 90, female maturity code is >= 77
View(maturity_assignment)
View(maturity_short_names)
# select 30 or higher for males and 55 or higher for females


# function code   --------


# assumptions male/female are coded as 1 and 2 respectively
# assumptions weight is collected in g
# specimens with no sex information are given a predicted weight that is the average of the male and female predictions.

predict_weight <- function(x) {
  # 1. check if this is necessary
  if (!anyNA(x$weight)) {
    stop("All weights are included in your dataset, no need to predict weights.", call. = FALSE)
  }

  # 2 . check sex codes are correct
  y <- c(0, 1, 2)
  if (!any(unique(x$sex) %in% y)) {
    stop("codes do not match 0,1,2", call. = FALSE)
  }

  # 3. convert 0 to males
  xy <- 0
  if (any(unique(x$sex) %in% xy)) {
    print(paste(nrow(filter(x, sex == 0)), "samples are missing sex information (coded as 0), the predicted weight for these individuals will be the average of male and female predicted weights."))
    xy <- filter(x, sex == 0 | sex == 1)
    xy$sex <- replace(xy$sex, xy$sex == 0, 1) # males are 1
    maletw <- fit_length_weight(
      xy,
      sex = "male",
      downsample = Inf,
      min_samples = 50L,
      method = c("tmb"),
      usability_codes = NULL,
      scale_weight = 1 / 1000
    )

    trawl_m <- filter(x, sex == 0)
    trawl_m$weight_predicted_m <- exp(maletw$pars$log_a +
      maletw$pars$b * log(trawl_m$length)) * 1000

    xx <- filter(x, sex == 2 | sex == 0)
    xx$sex <- replace(xx$sex, xx$sex == 0, 2) # females are 2
    femaletw <- fit_length_weight(
      xx,
      sex = ("female"),
      downsample = Inf,
      min_samples = 50L,
      method = c("tmb"), # method = c("tmb", "rlm", "lm"),
      # df = 3,
      # too_high_quantile = 1,
      usability_codes = NULL,
      scale_weight = 1 / 1000 # grams to kgs
    )

    trawl_f <- filter(x, sex == 0)
    trawl_f$weight_predicted_f <- exp(femaletw$pars$log_a +
      femaletw$pars$b * log(trawl_f$length)) * 1000

    predicted_weight_tw <- inner_join(trawl_m, trawl_f)
    predicted_weight_tw <- predicted_weight_tw %>% mutate(weight_predicted = rowSums(across(weight_predicted_f:weight_predicted_f)) / 2)
    predicted_weight_nosex <- dplyr::select(predicted_weight_tw, year, survey_abbrev, fishing_event_id, survey_id, species_code, sample_id, sex, length, weight, specimen_id, weight_predicted)
  }

  # check if there are codes for male and female, if only one sex then run one, otherwise run both.
  mf <- c(1, 2)
  if (any(unique(x$sex) %in% mf)) {
    # 2. run fit_length_weight for males and females separately.
    print(paste(nrow(filter(x, sex == 1)), "samples are male (coded as 1)"))
    print(paste(nrow(filter(x, sex == 2)), "samples are female (coded as 2)"))
    xx <- filter(x, sex == 2)
    femaletw <- fit_length_weight(
      xx,
      sex = ("female"),
      downsample = Inf,
      min_samples = 50L,
      method = c("tmb"), # method = c("tmb", "rlm", "lm"),
      # df = 3,
      # too_high_quantile = 1,
      usability_codes = NULL,
      scale_weight = 1 / 1000 # grams to kgs
    )

    xy <- filter(x, sex == 1)
    maletw <- fit_length_weight(
      xy,
      sex = ("male"),
      downsample = Inf,
      min_samples = 50L,
      method = c("tmb"), # method = c("tmb", "rlm", "lm"),
      # df = 3,
      # too_high_quantile = 1,
      usability_codes = NULL,
      scale_weight = 1 / 1000
    )

    trawl_f <- filter(xx, sex == 2)
    trawl_f$weight_predicted <- exp(femaletw$pars$log_a +
      femaletw$pars$b * log(trawl_f$length)) * 1000

    trawl_m <- filter(xy, sex == 1)
    trawl_m$weight_predicted <- exp(maletw$pars$log_a +
      maletw$pars$b * log(trawl_m$length)) * 1000


    predicted_weight_tw <- rbind(trawl_m, trawl_f)
    predicted_weight_tw2 <- dplyr::select(predicted_weight_tw, year, survey_abbrev, fishing_event_id, survey_id, species_code, sample_id, sex, length, weight, specimen_id, weight_predicted)
  }
  predicted_weight_tw3 <- rbind(predicted_weight_nosex, predicted_weight_tw2)
  return(predicted_weight_tw3)
}

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

# load raw data ---------------------------------------------------------------

survey_samples <- readRDS("output/samps_CoastalTrawl.rds") |>
  drop_na(length_ext_cm) |>
  # some earlier surveys I didn't know the conversion for
  mutate(fishing_event_id = as.numeric(fishing_event_id)) |>
  mutate(julian = lubridate::yday(date)) |>
  mutate(survey_name = survey_abbrev) |>
  mutate(
    survey_abbrev =
      case_when(
        survey_name == "Groundfish Slope and Shelf Combination Survey" & julian <= 226 ~ "NWFSC.Combo.pass1",
        survey_name == "Groundfish Slope and Shelf Combination Survey" & julian > 226 ~ "NWFSC.Combo.pass2",
        survey_name %in% c("HS MSA", "SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG") ~ survey_name,
        survey_name == "AFSC/RACE Slope Survey" ~ "NWFSC.slope",
        survey_name == "Groundfish Triennial Shelf Survey" ~ "Triennial",
        survey_name == "GOA" ~ "GOA"
      )
  ) |>
  mutate(
    survey_name = # this could be simplied, just need bc changed
      case_when(
        survey_name == "Groundfish Slope and Shelf Combination Survey" & julian <= 226 ~ "NWFSC.Combo.pass1",
        survey_name == "Groundfish Slope and Shelf Combination Survey" & julian > 226 ~ "NWFSC.Combo.pass2",
        survey_name %in% c("HS MSA", "SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG") ~ "BC",
        survey_name == "AFSC/RACE Slope Survey" ~ "NWFSC.slope",
        survey_name == "Groundfish Triennial Shelf Survey" ~ "Triennial",
        survey_name == "GOA" ~ "GOA"
      )
  )

survey_sets <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds") |>
  mutate(
    survey_name =
      case_when(
        survey_name == "Gulf of Alaska Bottom Trawl Survey" ~ "GOA",
        survey_name %in% c("syn bc") ~ "BC",
        survey_name %in% c("NWFSC.Combo.pass2", "NWFSC.Combo.pass1", "Triennial", "NWFSC.Slope") ~ survey_name
      )
  ) |>
  mutate(
    survey_abbrev =
      case_when(
        survey_abbrev %in% c("HS MSA", "SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG") ~ survey_abbrev,
        survey_abbrev == "NWFSC.Combo" & julian <= 226 ~ "NWFSC.Combo.pass1",
        survey_abbrev == "NWFSC.Combo" & julian > 226 ~ "NWFSC.Combo.pass2",
        survey_abbrev %in% c("GOA", "Triennial", "NWFSC.Slope") ~ survey_abbrev
      )
  ) |>
  mutate(fishing_event_id = as.numeric(fishing_event_id)) |>
  filter(year != 1993)


# remove sets with no samples and non-zero catches ----------------------------------------------

## early GOA years have no samples
# see fishing id 881890 for example
# I need to drop those from this database as
# how many (% wise) fishing events get dropped for each year.
# some have zero catches
sampsFEI <- survey_samples |>
  distinct(survey_samples$fishing_event_id, .keep_all = TRUE)

# distinguish sets with samps/no catch; no samps/no catch; no samps/catch; no samps/catch
survey_sets_withsamps <- survey_sets |>
  mutate(nosamps = ifelse(fishing_event_id %in% sampsFEI$fishing_event_id, "samps", "nosamps")) |>
  mutate(catch = ifelse(catch_weight != 0, "catch", "no_catch")) |>
  mutate(
    samps_catch =
      case_when(
        nosamps == "nosamps" & catch == "catch" ~ "nosamps_catch",
        nosamps == "nosamps" & catch == "no_catch" ~ "nosamps_nocatch",
        nosamps == "samps" & catch == "catch" ~ "samps_catch",
        nosamps == "samps" & catch == "no_catch" ~ "samps_nocatch"
      )
  )

survey_sets_withsamps2 <- survey_sets_withsamps |>
  filter( # get rid of nosamps_catch, and samps_nocatch
    samps_catch %in% c("nosamps_nocatch", "samps_catch")
  )

survey_sets_NOsamps <- survey_sets_withsamps |>
  filter(survey_name %in% c("BC", "Gulf of Alaska Bottom Trawl Survey", "NWFSC.Combo.pass1", "NWFSC.Combo.pass2")) |>
  filter(survey_abbrev != "HS MSA") |>
  filter(samps_catch == "nosamps_catch")

# drop the first two years of the GOA survey as there aren't enough samples
years <- c(1996, 1999)
survey_sets_withsamps <- filter(survey_sets_withsamps, !(year %in% years))

#saveRDS(survey_sets_withsamps2, "output/Wrangled_USCanData_nosampssetsremoved.rds")


# clean data (sets with no samples and non-zero catches) ---------------------------------------------------

#survey_sets <- readRDS("output/Wrangled_USCanData_nosampssetsremoved.rds")

# nwfsc slope survey in 1998 has a very different julian date
# I didnt' change anything about that but could by using this code

survey_samples <- survey_samples %>%
  mutate(trip_start_date = date) |>
  mutate(julian = lubridate::yday(date)) |>
  rename(
    length = length_ext_cm
  ) |>
  mutate(
    specimen_id = seq(1, n(), 1),
    species_common_name = "spiny dogfish"
  )

survey_sets <- survey_sets_withsamps |>
  rename(
    area_swept = area_swept_m2
  )



# impute weights, calculate ratios, apply ratios   -----------------------------

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
    survey_abbrev %in% c("GOA", "Triennial", "NWFSC.Slope") ~ survey_abbrev
  ))


# determine maturity length cutoffs
m <- gfplot::fit_mat_ogive(survey_samples,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
gfplot::plot_mat_ogive(m)
saveRDS(m, "output/survey_samples_codedmaturity.rds")

m$pred_data

fm <- m$pred_data |>
  mutate(min2 = abs(m$pred_data$glmm_fe - 0.95)) |>
  filter(female == 1) |>
  slice_min(min2, n = 1, with_ties = FALSE)
fi <- m$pred_data |>
  mutate(min2 = abs(m$pred_data$glmm_fe - 0.05)) |>
  filter(female == 1) |>
  slice_min(min2, n = 1, with_ties = FALSE)
mi <- m$pred_data |>
  mutate(min2 = abs(m$pred_data$glmm_fe - 0.05)) |>
  filter(female == 0) |>
  slice_min(min2, n = 1, with_ties = FALSE)
mm <- m$pred_data |>
  mutate(min2 = abs(m$pred_data$glmm_fe - 0.95)) |>
  filter(female == 0) |>
  slice_min(min2, n = 1, with_ties = FALSE)

# tally by length group and calculate weight of each group
count3 <- count2 |>
  filter(sex != 0) |>
  mutate(lengthgroup = case_when(
    length >= (as.numeric(fi$age_or_length)) & length < (as.numeric(fm$age_or_length)) & sex == 2 ~ "maturingf",
    length >= (as.numeric(mi$age_or_length)) & length < (as.numeric(mm$age_or_length)) & sex == 1 ~ "maturingm",
    length >= (as.numeric(fm$age_or_length)) & sex == 2 ~ "mf",
    length >= (as.numeric(mm$age_or_length)) & sex == 1 ~ "mm",
    length < (as.numeric(fi$age_or_length)) & sex == 2 ~ "imm",
    length < (as.numeric(mi$age_or_length)) & sex == 1 ~ "imm"
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

surveys <- survey_sets |>
  dplyr::select(
    year, survey_abbrev, fishing_event_id, catch_weight, survey_name, area_swept,
    logbot_depth, longitude, latitude
  )

sets_summed <- surveys |>
  group_by(year, survey_abbrev, fishing_event_id, area_swept) |>
  summarize(sum_catch_weight_survey = sum(catch_weight))


# now join
df <- left_join(survey_sets, count3, by = c("year", "survey_abbrev", "fishing_event_id")) |>
  mutate(catch_weight_ratio = catch_weight * (ratio / 100))

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

vect <- unique(df2$survey_name)

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


final <- left_join(cf2, surveys)


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
saveRDS(df2, "output/splitbymaturityregion_df.rds")
