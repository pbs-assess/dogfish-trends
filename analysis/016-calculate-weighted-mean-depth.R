library(dplyr)
library(sdmTMB)
source("analysis/999-prep-overall-trawl.R")

dat |> group_by(survey_name) |> drop_na(bottom_temp_c) |> summarize(min = min(bottom_temp_c), max = max(bottom_temp_c))

fits <- readRDS("output/fit-trawl-by-maturity-poisson-link.rds") #<- st model
#fits <- readRDS(file = "output/fit-trawl-by-maturity-poisson-link-sp-only.rds") #avg density model
grid <- readRDS("output/pred-coastal-temp-grid.rds") #<- from 999-prep-temp-grid.R
grid <- grid$data
unique(grid$region)
grid$bottom_temp_c <- exp(grid$est)
grid <- grid |> filter(year >=2003)

regions <- unique(grid$region)

mats <- unlist(lapply(fits, \(x) x$data$lengthgroup[1]))
names(fits) <- mats


if (FALSE) {
  # make predictions from cpp template: ---------------------------------------
  #run_mean_depth_by_maturity <- function(i) {
  run_mean_var_by_maturity <- function(i) {
    cat(names(fits)[i], "\n")
    out <- run_mean_var_by_region <- function(r) {
      cat(r, "\n")
      nd <- filter(grid, region %in% r, year %in% fits[[i]]$data$year)
      pred <- predict(fits[[i]], newdata = nd)
      pred |>
        mutate(biomass = exp(est1) * exp(est2)) |>
        mutate(biomass = biomass * nd$area_km) |> # area expansion
        group_by(year) |>
        mutate(temp = (bot_depth * biomass) / sum(biomass)) |>
        mutate(temp2 = (bottom_temp_c * biomass) / sum(biomass)) |>
        summarise(mean_depth = sum(temp), mean_temp = sum(temp2)) |>
        mutate(region = r)
    }
    out <- lapply(regions, run_mean_var_by_region)
    out <- do.call(rbind, out)
    out$maturity_group <- mats[i]
    out
  }
  ret1 <- lapply(seq_along(mats), run_mean_var_by_maturity)
  ret1 <- do.call(rbind, ret1)
  ret1$region <- factor(ret1$region, levels = c("GOA", "BC", "NWFSC"))

  ret1 |>
    ggplot(aes(year, -mean_depth, colour = maturity_group)) +
    geom_line() +
    facet_wrap(~region)
}

# do it with MVN draws for uncertainty --------------------------------------

run_mean_var_by_maturity_mvn <- function(i) {
  cat("\n#", names(fits)[i], "\n")
  out <- run_mean_var_by_region <- function(r) {
    cat(r, "\n")
    if (r != "Coastwide") {
      nd <- filter(grid, region %in% r, year %in% fits[[i]]$data$year)
    } else {
      nd <- filter(grid, year %in% fits[[i]]$data$year)
    }
    .nsim <- 200
    cat("Making predictions\n")
    pred <- predict(fits[[i]], newdata = nd, nsim = .nsim)
    pred <- exp(pred + log(nd$area_km)) # area expansion
    p <- reshape2::melt(pred) |> rename(year = Var1, iter = Var2, biomass = value)
    p$cell_id <- rep(seq_len(nrow(nd)), .nsim)
    nd$cell_id <- seq_len(nrow(nd))
    p <- p |> group_by(cell_id, iter) |> #<- new
    mutate(avg.density = mean(biomass))
    p <- left_join(p, select(nd, cell_id, bot_depth, bottom_temp_c), by = join_by(cell_id))
    cat("Summarizing samples\n")
    p |>
      group_by(year, iter) |>
      mutate(weighted_var = sum((bot_depth * biomass) / sum(biomass))) |>
      mutate(weighted_temp = sum((bottom_temp_c * biomass) / sum(biomass))) |>
      mutate(weighted_temp_constantden = sum((bottom_temp_c * avg.density) / sum(avg.density))) |>
      group_by(year) |>
      summarise(
        mean_depth = mean(weighted_var),
        lwr025 = quantile(weighted_var, probs = 0.025),
        lwr05 = quantile(weighted_var, probs = 0.05),
        lwr10 = quantile(weighted_var, probs = 0.10),
        lwr25 = quantile(weighted_var, probs = 0.25),
        med = quantile(weighted_var, probs = 0.50),
        upr75 = quantile(weighted_var, probs = 0.75),
        upr90 = quantile(weighted_var, probs = 0.90),
        upr95 = quantile(weighted_var, probs = 0.95),
        upr975 = quantile(weighted_var, probs = 0.975),
        sd_depth = sd(weighted_var),
        mean_log_depth = mean(log(weighted_var)),
        sd_log_depth = sd(log(weighted_var)),
        mean_temp = mean(weighted_temp),
        lwr025_temp = quantile(weighted_temp, probs = 0.025),
        lwr05_temp = quantile(weighted_temp, probs = 0.05),
        lwr10_temp = quantile(weighted_temp, probs = 0.10),
        lwr25_temp = quantile(weighted_temp, probs = 0.25),
        med_temp = quantile(weighted_temp, probs = 0.50),
        upr75_temp = quantile(weighted_temp, probs = 0.75),
        upr90_temp = quantile(weighted_temp, probs = 0.90),
        upr95_temp = quantile(weighted_temp, probs = 0.95),
        upr975_temp = quantile(weighted_temp, probs = 0.975),
        sd_depth_temp = sd(weighted_temp),
        mean_log_depth_temp = mean(log(weighted_temp)),
        sd_log_depth_temp = sd(log(weighted_temp))
      ) |>
      mutate(region = r)
  }
  out <- lapply(c(regions, "Coastwide"), run_mean_var_by_region)
  #out <- lapply(regions, run_mean_var_by_region)
  out <- do.call(rbind, out)
  out$maturity_group <- mats[i]
  gc()
  out
}
ret <- lapply(seq_along(mats), run_mean_var_by_maturity_mvn)
ret <- do.call(rbind, ret)

#saveRDS(ret, file = "output/biomass-weighted-depth.rds")
#saveRDS(ret, file = "output/biomass-weighted-depth-temp.rds")
saveRDS(ret, file = "output/biomass-weighted-depth-temp-sp-only.rds")


