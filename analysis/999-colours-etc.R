
cols_region <- c("grey30", RColorBrewer::brewer.pal(3L, "Set2"))
names(cols_region) <- c("Coastwide", "Gulf of Alaska", "British Columbia", "US West Coast")

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
