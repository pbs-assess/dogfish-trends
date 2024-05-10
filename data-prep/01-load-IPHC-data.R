# Load IPHC data

# downloaded from here:
# https://www.iphc.int/data/fiss-data-query

iphc_stations <- read.csv("data-raw/IPHC data download from IPHC website/Map select_standardgrid.csv")
iphc_coast <- read.csv("data-raw/IPHC data download from IPHC website/Non-Pacific halibut data_raw.csv")
iphc_latlongs <- read.csv("data-raw/IPHC data download from IPHC website/Set and Pacific halibut data_raw.csv") %>%
  dplyr::select(IPHC.Reg.Area, Date, Eff, Ineffcde, BeginLat, BeginLon, AvgDepth..fm., Stlkey)
