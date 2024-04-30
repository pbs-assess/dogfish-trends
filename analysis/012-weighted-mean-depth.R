library(ggplot2)
library(dplyr)
library(sdmTMB)
source("analysis/999-prep-overall-trawl.R")
fits <- readRDS("output/fit-trawl-by-maturity-poisson-link.rds")
regions <- unique(grid$region)
grid <- rename(grid, X = UTM.lon, Y = UTM.lat)

mats <- unlist(lapply(fits, \(x) x$data$lengthgroup[1]))
names(fits) <- mats

if (FALSE) {
  # make predictions from cpp template: ---------------------------------------
  run_mean_depth_by_maturity <- function(i) {
    cat(names(fits)[i], "\n")
    out <- run_mean_depth_by_region <- function(r) {
      cat(r, "\n")
      nd <- filter(grid, region %in% r, year %in% fits[[i]]$data$year)
      pred <- predict(fits[[i]], newdata = nd)
      pred |>
        mutate(biomass = exp(est1) * exp(est2)) |>
        group_by(year) |>
        mutate(temp = (bot_depth * biomass) / sum(biomass)) |>
        summarise(mean_depth = sum(temp)) |>
        mutate(region = r)
    }
    out <- lapply(regions, run_mean_depth_by_region)
    out <- do.call(rbind, out)
    out$maturity_group <- mats[i]
    out
  }
  ret1 <- lapply(seq_along(mats), run_mean_depth_by_maturity)
  ret1 <- do.call(rbind, ret1)
  ret1$region <- factor(ret1$region, levels = c("GOA", "BC", "NWFSC"))

  ret1 |>
    ggplot(aes(year, -mean_depth, colour = maturity_group)) +
    geom_line() +
    facet_wrap(~region)
}

# do it with MVN draws for uncertainty --------------------------------------

run_mean_depth_by_maturity_mvn <- function(i) {
  cat("#", names(fits)[i], "\n")
  out <- run_mean_depth_by_region <- function(r) {
    cat(r, "\n")
    nd <- filter(grid, region %in% r, year %in% fits[[i]]$data$year)
    .nsim <- 200L
    cat("Making predictions\n")
    pred <- predict(fits[[i]], newdata = nd, nsim = .nsim)
    p <- reshape2::melt(pred) |> rename(year = Var1, iter = Var2, est = value)
    p$cell_id <- rep(seq_len(nrow(nd)), .nsim)
    nd$cell_id <- seq_len(nrow(nd))
    p <- left_join(p, select(nd, cell_id, bot_depth), by = join_by(cell_id))
    cat("Summarizing samples\n")
    p |>
      mutate(biomass = exp(est)) |>
      group_by(year, iter) |>
      mutate(weighted_depth = sum((bot_depth * biomass) / sum(biomass))) |>
      group_by(year) |>
      summarise(
        mean_depth = mean(weighted_depth),
        sd_depth = sd(weighted_depth),
        mean_log_depth = mean(log(weighted_depth)),
        sd_log_depth = sd(log(weighted_depth)),
      ) |>
      mutate(region = r)
  }
  out <- lapply(regions, run_mean_depth_by_region)
  out <- do.call(rbind, out)
  out$maturity_group <- mats[i]
  gc()
  out
}
ret <- lapply(seq_along(mats), run_mean_depth_by_maturity_mvn)
ret <- do.call(rbind, ret)
ret$region <- factor(ret$region, levels = c("GOA", "BC", "NWFSC"))

lu <- data.frame(
  maturity_group = c("mm", "mf", "maturingm", "maturingf", "imm"),
  group_clean = c("Mature males", "Mature females", "Maturing males", "Maturing females", "Immature"),
  stringsAsFactors = FALSE
)
ret2 <- left_join(ret, lu)
lvls <- rev(c("Immature", "Maturing females", "Maturing males", "Mature males", "Mature females"))
ret2$group_clean <- factor(ret2$group_clean, levels = lvls)

lu <- data.frame(
  region = c("GOA", "BC", "NWFSC"),
  region_clean = c("Gulf of Alaska", "British Columbia", "US West Coast"),
  stringsAsFactors = FALSE
)
ret2 <- left_join(ret2, lu)
ret2$region_clean <- factor(ret2$region_clean, levels = c("Gulf of Alaska", "British Columbia", "US West Coast"))

pal <- "Paired"
g <- ret2 |>
  ggplot(aes(year, -mean_depth, colour = group_clean, fill = group_clean)) +
  geom_line() +
  geom_ribbon(aes(ymin = -(mean_depth - sd_depth), ymax = -(mean_depth + sd_depth)),
    colour = NA, alpha = 0.2
  ) +
  facet_wrap(~region_clean) +
  scale_colour_brewer(palette = pal) +
  scale_fill_brewer(palette = pal) +
  labs(fill = "Group", colour = "Group", x = "Year", y = "Biomass-weighted mean depth (m)") +
  theme(legend.position = "inside", legend.position.inside = c(0.11, 0.24))

ggsave("figs/biomass-weighted-depth.png", width = 7, height = 3.5)
ggsave("figs/biomass-weighted-depth.pdf", width = 7, height = 3.5)
