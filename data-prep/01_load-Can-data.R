#load Canadian survey data


# library -----------------------------------------------------------------

library(gfdata)
remotes::install_github("pbs-assess/gfdata")

# load Can survey data ----------------------------------------------------
# data_survey_samples <- get_survey_samples(species = "north pacific spiny dogfish")
# data_survey_samples
#
# data_surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
# data_surveysets
#
# saveRDS(data_surveysets, "output/data_surveysets.rds")
# saveRDS(data_survey_samples, "output/data_survey_samples.rds")

data_survey_samples <- readRDS("output/data_survey_samples.rds")
data_surveysets <- readRDS("output/data_surveysets.rds")

x <- dplyr::filter(data_survey_samples, survey_abbrev == "SYN WCVI")
mean = mean(x$weight, na.rm = TRUE)
