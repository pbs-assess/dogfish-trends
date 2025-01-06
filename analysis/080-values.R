library(dplyr)
mround <- function(x, digits) {
  sprintf(paste0("%.", digits, "f"), round(x, digits))
}
write_tex <- function(x, macro, append = TRUE) {
  paste0("\\newcommand{\\", macro, "}{", x, "}") |>
    readr::write_lines("output/values.tex", append = append)
}

glmdf <- readRDS("output/glmdf-overall-trends.rds")
decades <- (2023-2003)/10
s <- glmdf |> select(region, slope, lwr, upr) |> distinct() |>
  mutate(slope_dec = 100 * (1 - exp(slope * decades)), lwr_dec = 100 * (1 - exp(lwr * decades)), upr_dec = 100 * (1 - exp(upr * decades))) |>
  mutate(slope = exp(slope), lwr = exp(lwr), upr = exp(upr)) |>
  mutate(slope = mround(slope, 2), lwr = mround(lwr, 2), upr = mround(upr, 2)) |>
  mutate(slope_dec = mround(slope_dec, 0), lwr_dec = mround(lwr_dec, 0), upr_dec = mround(upr_dec, 0))
s <- mutate(s, prop = paste0(slope, " (95\\% CI: ", lwr, "--", upr, ")"))
s <- mutate(s, propAbs = paste0(slope_dec, "\\% (95\\% CI: ", upr_dec, "\\%--", lwr_dec, "\\%)"))
s <- mutate(s, tag = region)
s <- mutate(s, tag = gsub("Coastwide", "coast", tag))
s <- mutate(s, tag = gsub("Gulf of Alaska", "goa", tag))
s <- mutate(s, tag = gsub("British Columbia", "bc", tag))
s <- mutate(s, tag = gsub("US West Coast", "wc", tag))
s

unlink("output/values.tex")
for (i in seq(nrow(s))) {
  x <- s[i,,drop=FALSE]
  write_tex(x$prop, macro = paste0(x$tag, "Prop"))
  write_tex(x$propAbs, macro = paste0(x$tag, "PropAbs"))
}

svc <- readRDS("output/svc-spatial-data.rds") #<- from 021-plot-svc-models.R

write_tex(mround(100 * mean(exp(svc$svc) < 0.2), 0), "SVCeighty")

x <- group_by(prs, region) |>
  summarise(p = mround(100 * mean(exp(svc) < 0.3), 0))

write_tex(x$p[x$region == "BC"], "bcSVCseventy")
write_tex(x$p[x$region == "GOA"], "goaSVCseventy")
write_tex(x$p[x$region == "NWFSC"], "wcSVCseventy")
