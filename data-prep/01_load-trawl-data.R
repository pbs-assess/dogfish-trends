# load trawl survey data from Alaska, United States West Coast, and Canada


# library -----------------------------------------------------------------
library(surveyjoin)
library(gfdata)
library(here)
library(tidyverse)
remotes::install_github("pbs-assess/gfdata")
remotes::install_github("DFO-NOAA-Pacific/surveyjoin")
remotes::install_github("pfmc-assessments/nwfscSurvey")


# Sets and samps - Canada data ----------------------------------------------------

if (!file.exists("data-raw/data_survey_samples.rds")) {
  data_survey_samples <- get_survey_samples(species = "north pacific spiny dogfish")
  saveRDS(data_survey_samples, "data-raw/data_survey_samples.rds")
}

if (!file.exists("data-raw/data_surveysets.rds")) {
  data_surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
  saveRDS(data_surveysets, "data-raw/data_surveysets.rds")
}


# Sets - US West Coast data  ---------------------------------------------------------

# nwfsc triennial survey ran from 1980 - 2004 (1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004)
# nwfsc west coast groundfish bottom trawl (WCGFBT) here called combo survey ran from 2003 - 2021 annually except for 2020

# http://pfmc-assessments.github.io/nwfscSurvey/index.html
# http://pfmc-assessments.github.io/nwfscSurvey/articles/nwfscSurvey.html

if (!file.exists("data-raw/nwfsc_sets_slope_AFSC.rds")) {
  catch_nwfsc_combo <- nwfscSurvey::PullCatch.fn(SurveyName = "NWFSC.Combo") %>%
    mutate(survey_abbrev = "NWFSC.Combo") |>
    filter(Common_name == "Pacific spiny dogfish")
  saveRDS(catch_nwfsc_combo, "data-raw/nwfsc_sets_combo.rds")

  catch_nwfsc_triennial <- nwfscSurvey::PullCatch.fn(SurveyName = "Triennial") %>%
    mutate(survey_abbrev = "Triennial") |>
    filter(Common_name == "Pacific spiny dogfish")
  saveRDS(catch_nwfsc_triennial, "data-raw/nwfsc_sets_triennial.rds")

  catch_nwfsc_slope <- nwfscSurvey::PullCatch.fn(SurveyName = "NWFSC.Slope") %>%
    mutate(survey_abbrev = "NWFSC.Slope") |>
    filter(Common_name == "Pacific spiny dogfish")
  saveRDS(catch_nwfsc_slope, "data-raw/nwfsc_sets_slope.rds")

  catch_nwfsc_slope2 <- nwfscSurvey::PullCatch.fn(SurveyName = "AFSC.Slope") %>%
    mutate(survey_abbrev = "AFSC.Slope") |>
    filter(Common_name == "Pacific spiny dogfish")
  saveRDS(catch_nwfsc_slope2, "data-raw/nwfsc_sets_slope_AFSC.rds")
} else {
  catch_nwfsc_combo <- readRDS("data-raw/nwfsc_sets_combo.rds")
  catch_nwfsc_triennial <- readRDS("data-raw/nwfsc_sets_triennial.rds")
  catch_nwfsc_slope <- readRDS("data-raw/nwfsc_sets_slope.rds")
  catch_nwfsc_slope2 <- readRDS("data-raw/nwfsc_sets_slope_AFSC.rds")
}


# Samps - US West Coast data  ----------------------------------------------------------

if (!file.exists("data-raw/nwfsc_samp_slope_AFSC.rds")) {
  nwfsc_samp_combo <- nwfscSurvey::PullBio.fn(
    Name = "Pacific spiny dogfish",
    SurveyName = "NWFSC.Combo"
  )
  # correct apparent typo by matching to other samples from set with matching characteristics
  nwfsc_samp_combo[nwfsc_samp_combo$Date == "1978-May-04", ]$Date <- "2012-Sep-14"
  nwfsc_samp_combo <- nwfsc_samp_combo %>% mutate(date = as.Date(Date))
  saveRDS(nwfsc_samp_combo, "data-raw/nwfsc_samp_combo.rds")

  nwfsc_samp_triennial <- nwfscSurvey::PullBio.fn(
    Name = "Pacific spiny dogfish",
    SurveyName = "Triennial"
  )
  nwfsc_samp_triennial$Lengths <- nwfsc_samp_triennial$Lengths %>% mutate(date = as.Date(Date))
  saveRDS(nwfsc_samp_triennial, "data-raw/nwfsc_samp_triennial.rds")

  # nwfsc_samp_slope <- nwfscSurvey::PullBio.fn(Name = "Pacific spiny dogfish", SurveyName = "NWFSC.Slope") # only returns 4 fish!
  # https://www.arlis.org/docs/vol1/51990129.pdf - Dogfish not sampled

  nwfsc_samp_slope_AFSC <- nwfscSurvey::PullBio.fn(
    Name = "Pacific spiny dogfish",
    SurveyName = "AFSC.Slope"
  )
  nwfsc_samp_slope_AFSC$Lengths <- nwfsc_samp_slope_AFSC$Lengths %>% mutate(date = as.Date(Date))
  saveRDS(nwfsc_samp_slope_AFSC, "data-raw/nwfsc_samp_slope_AFSC.rds")
  # nwfsc_samp_slope_AFSC$Lengths %>% group_by(Year) %>% summarise(n = n())
  # nwfsc_samp_shelf <- nwfscSurvey::PullBio.fn(Name = "Pacific spiny dogfish", SurveyName = "NWFSC.Shelf") # no data
} else {
  nwfsc_samp_combo <- readRDS("data-raw/nwfsc_samp_combo.rds")
  nwfsc_samp_triennial <- readRDS("data-raw/nwfsc_samp_triennial.rds")
  nwfsc_samp_slope_AFSC <- readRDS("data-raw/nwfsc_samp_slope_AFSC.rds")
}

nwfsc_samples_raw <- bind_rows(nwfsc_samp_combo, nwfsc_samp_triennial$Lengths, nwfsc_samp_slope_AFSC$Lengths) |>
  dplyr::select(-Date)
saveRDS(nwfsc_samples_raw, "data-raw/NWFSC_sampledata.rds")


# Sets - Alaskan data ----

# from surveyjoin github
# https://github.com/DFO-NOAA-Pacific/surveyjoin/tree/main/data

load("data-raw/surveyjoin/afsc_haul.rda")
load("data-raw/surveyjoin/afsc_catch.rda")

goa_all_sets <- afsc_haul %>% filter(survey_name %in% c(
  # "Aleutian Islands Bottom Trawl Survey", #excluded due to few catches
  "Gulf of Alaska Bottom Trawl Survey"
))

goa_all_catch <- afsc_catch %>%
  filter( # catch_weight > 0 &
    scientific_name %in% c("Squalus suckleyi")
  ) %>%
  mutate(species_common_name = "north pacific spiny dogfish")

saveRDS(goa_all_sets, "data-raw/goa-sets.rds")
saveRDS(goa_all_catch, "data-raw/goa-catch.rds")

# Samps - Alaskan data ----

# sample data from: https://github.com/afsc-gap-products/data-requests/issues/62
# set data from: https://github.com/afsc-gap-products/gap_public_data#access-the-data
# see here for definition of the codes: https://repository.library.noaa.gov/view/noaa/31570

goa_samps <- read.csv("data-raw/surveyjoin/goa_dogfish_sex_length.csv")
