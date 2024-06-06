library(gfdata)

# get catch and retain trips with no catches
# extracts catch and effort data since 1996

cpuetl <- gfdata::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)
cpuetl <- cpuetl |> filter(species_code == "044")
cpuetl <- cpuetl |>
  mutate(date2 = as.Date(best_date, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(year = lubridate::year(dmy)) |>
  mutate(month = lubridate::month(dmy)) |>
  mutate(julian = lubridate::yday(dmy))
saveRDS(cpuetl, "data-raw/cpue-trawl-bc.rds")
