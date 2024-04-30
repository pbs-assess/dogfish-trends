library(dplyr)
library(ggplot2)

source("analysis/999-colours-etc.R")
ind <- readRDS("output/trawl-coast-indexes.rds")

ind <- ind |>
  group_by(region, model) |>
  mutate(geo_mean = exp(mean(log_est[year >= 2003]))) |>
  mutate(
    est = est / geo_mean,
    lwr = lwr / geo_mean,
    upr = upr / geo_mean
  )

ind <- ind |>
  mutate(region = factor(region,
    levels = c("Coastwide", "Gulf of Alaska", "British Columbia", "US West Coast")))

glmdf <- ind |> filter(year >= 2006, model == "Combined") |>
  group_by(region) |>
  group_split() |>
  purrr::map_dfr(\(x) {
    x$decade <- x$year / 10
    m <- glm(est ~ decade, data = x, family = Gamma(link = "log"))
    nd <- data.frame(year = seq(min(x$year), max(x$year)))
    nd$decade = nd$year / 10
    p <- predict(m, newdata = nd, type = "response")
    ret <- data.frame(nd, glm_pred = p)
    row.names(ret) <- NULL
    ret$region <- x$region[1]
    ret$slope <- coef(m)[[2]]
    ret
  })

blues <- RColorBrewer::brewer.pal(3, "Blues")

ind |>
  ggplot(aes(year, est, group = model)) +
  facet_wrap(~region, scales = "free_y", ncol = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr, colour = model),
    position = position_jitter(width = 0.2, height = 0), size = 0.2, pch = 21) +
  # geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), alpha = 0.2) +
  # geom_point(aes(colour = model)) +
  # geom_line(aes(colour = model)) +
  # geom_smooth(se = FALSE) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, xlim = c(1990, 2023)) +
  geom_line(aes(x = year, y = glm_pred), inherit.aes = FALSE, data = glmdf, lwd = 1, colour = "grey30") +
  scale_colour_manual(values = c("Region-specific" = "grey60", "Combined" = blues[3])) +
  theme(legend.position.inside = c(0.25, 0.86), legend.position = "inside", axis.title.x = element_blank()) +
  labs(x = "Year", y = "Biomass index", colour = "Model")

ggsave("figs/fig1.pdf", width = 2.8, height = 7)
