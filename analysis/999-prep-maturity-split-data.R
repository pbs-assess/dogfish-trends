prep_maturity_split_data <- function() {
  d <- readRDS("output/splitbymaturityregion_df.rds")
  d <- dplyr::filter(d, !is.na(longitude) & !is.na(latitude))
  d <- dplyr::filter(d, survey_name != "Triennial")
  d <- sdmTMB::add_utm_columns(d, c("longitude", "latitude"), units = "km", utm_crs = coast_crs)
  d$offset_km <- log(d$area_swept/(1000*1000))
  d$catch_weight_t <- d$catch_weight_ratio/1000 # !! catch_weight_ratio is in kg
  d$depth_m <- exp(d$logbot_depth)
  d
}
