
cols_region <- c("grey30", RColorBrewer::brewer.pal(3L, "Set2"))
names(cols_region) <- c("Coastwide", "Gulf of Alaska", "British Columbia", "US West Coast")

# pal <- PNWColors::pnw_palette("Cascades", 3)

cols_region2 <- c(
  "GOA" = "#516823",
  "BC" = "#FCA636FF", # "#e2e260",
  "NWFSC" = "#88a2b9"
)
cols_region3 <- c(
  "Coastwide" = "grey30",
  "Gulf of Alaska" = "#516823",
  "British Columbia" = "#FCA636FF", # "#e2e260",
  "US West Coast" = "#88a2b9"
)

if (FALSE) {
  pal <- viridisLite::plasma(3, begin = 0.25, end = 0.80)
  plot(1:3, cex = 19, col = pal, pch = 19)
}
pal <- viridisLite::plasma(3, begin = 0.25, end = 0.8)

cols_region2 <- c(
  "GOA" = pal[1],
  "BC" = pal[2],
  "NWFSC" = pal[3]
)
cols_region3 <- c(
  "Coastwide" = "grey30",
  "Gulf of Alaska" = pal[1],
  "British Columbia" = pal[2],
  "US West Coast" = pal[3]
)

cols_region <- cols_region3

# cols_maturities <- rev(RColorBrewer::brewer.pal(5L, "Paired")[c(3, 4, 1, 2, 5)])
cols_maturities <- c(
  "Immature" = "#FB9A99", # "#E31A1C"
  "Mature males" = "#1F78B4",
  "Maturing males" = "#A6CEE3",
  "Mature females" = "#33A02C",
  "Maturing females" = "#B2DF8A"
)

clean_region_names <- function(x) {
  x$region <- gsub("^Coast$", "Coastwide", x$region)
  x$region <- gsub("GOA", "Gulf of Alaska", x$region)
  x$region <- gsub("BC", "British Columbia", x$region)
  x$region <- gsub("NWFSC", "US West Coast", x$region)
  x <- x |>
    mutate(region = factor(region,
      levels = rev(c("Coastwide", "Gulf of Alaska", "British Columbia", "US West Coast"))))
  x
}

add_maturity_group_clean_column <- function(x) {
  lu <- data.frame(
    group = c("mm", "mf", "maturingm", "maturingf", "imm"),
    group_clean = c("Mature males", "Mature females", "Maturing males", "Maturing females", "Immature"),
    stringsAsFactors = FALSE
  )
  x <- left_join(x, lu)
  lvls <- rev(c("Immature", "Maturing males", "Mature males", "Maturing females", "Mature females"))
  x$group_clean <- factor(x$group_clean, levels = lvls)
  x
}
