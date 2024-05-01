# built in data/raw/pull-raw-data.R
s <- readRDS("data/raw/survey-sets.rds")
d <- dplyr::filter(s, grepl("HBLL OUT", survey_abbrev))
d <- sdmTMB::add_utm_columns(d, utm_crs = 32609)
sum(is.na(d$depth_m))
d <- dplyr::filter(d, !is.na(depth_m))
d$log_hook_count <- log(d$hook_count)

mesh <- make_mesh(d, c("X", "Y"), cutoff = 12)
plot(mesh)
mesh$mesh$n

fit_nb2 <- sdmTMB(
  catch_count ~ 1 + poly(log(depth_m), 2L),
  family = nbinom2(link = "log"),
  data = d,
  mesh = mesh,
  offset = "offset_hk", # hook competition offset
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = FALSE,
  anisotropy = FALSE,
  extra_time = seq(min(d$year), max(d$year))
)
