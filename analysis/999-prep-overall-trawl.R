# prep data and grid for modelling

library(ggplot2)
library(dplyr)
library(tidyr)
library(sdmTMB)

coast_crs  <- paste0(
  "+proj=aea +lat_0=48 +lon_0=-133 +lat_1=38.5 ",
  "+lat_2=56 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
)

prep_trawl_dat_and_grid <- function() {
  df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds") |>
    drop_na(area_swept_m2) |>
    mutate(offset_km2 = log(area_swept_m2 / (1000 * 1000))) |>
    mutate(survey_name = ifelse(
      survey_name == "Gulf of Alaska Bottom Trawl Survey", "GOA",
      survey_name
    )) |>
    drop_na(longitude, latitude)
  # leaving this here so that the final year of msa gets included in coastwide model
  # because it fills in a nice gaps in that year
  df$survey_name[df$year < 2003 & df$survey_name == "syn bc"] <- "msa bc"

  df$depth_m <- exp(df$logbot_depth)
  range(df$depth_m)
  df2 <- add_utm_columns(df, units = "km", utm_crs = coast_crs) %>%
    rename("UTM.lon" = "X", "UTM.lat" = "Y")

  group_by(df2, survey_name) |>
    summarise(
      prop_zero = sum(catch_weight == 0) / n(),
      mean = mean(exp(offset_km2))
    )

  group_by(df2, survey_name) |>
    summarise(
      prop_zero = sum(catch_weight == 0) / n(),
      mean = mean(catch_weight / exp(offset_km2))
    )

  df2$year_scaled <- (df2$year - 2010) / 10

  grid2 <- readRDS("output/prediction_grid_coastaltl_new.rds") |>
    distinct() |>
    filter(bot_depth < 1308) |>
    dplyr::select(longitude, latitude, bot_depth, area_km, region) |>
    distinct(.keep_all = TRUE) |>
    drop_na(bot_depth) |>
    mutate(
      logbot_depth = log(bot_depth)
    ) |>
    filter(bot_depth < 1096 & bot_depth > 11) |>
    drop_na()
  range(grid2$bot_depth)

  grid <- add_utm_columns(grid2, units = "km", utm_crs = coast_crs) %>%
    rename("UTM.lon" = "X", "UTM.lat" = "Y") |>
    mutate(
      UTM.lon = round(UTM.lon, 3)
    )

  years <- seq(min(df$year), max(df$year), 1)
  grid <- purrr::map_dfr(unique(df2$year), ~ tibble(grid, year = .x))
  grid$year_scaled <- (grid$year - 2010) / 10
  range(grid$area_km)
  if (FALSE) {
    ggplot(grid, aes(UTM.lon, UTM.lat, colour = area_km)) +
      geom_point()
  }
  grid$region <- toupper(grid$region)

  ggplot(df2, aes(UTM.lon, UTM.lat, colour = depth_m)) +
    geom_point()

  df2$catch_weight_t <- df2$catch_weight / 1000
  grid$depth_m <- grid$bot_depth
  grid <- mutate(grid, X = UTM.lon, Y = UTM.lat)

  list(dat = df2, grid = grid)
}

x <- prep_trawl_dat_and_grid()
dat <- x$dat
grid <- x$grid
rm(x)
