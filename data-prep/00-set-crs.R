## this is Albers based on means and quantiles from the trawl data

# df <- readRDS("output/Wrangled_USCan_trawldata_marmapdepth.rds")
# quantile(df$latitude, 1/6)
# quantile(df$latitude, 5/6)
# mean(df$latitude)
# mean(df$longitude)

coast_crs <- paste0(
  "+proj=aea +lat_0=48 +lon_0=-133 +lat_1=38.5 ",
  "+lat_2=56 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
)

## previously this was used
#
# coast_crs <- 32607
#
