library(dplyr)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())

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

lab_pos <- ind |> group_by(region) |>
  summarise(max_y = max(upr)) |>
  mutate(region_lab = paste0("(", letters[1:4], ") ", region))
  # mutate(region_lab = paste0("(", letters[1:4], ") "))

# theme_set(theme_light())
theme_set(ggsidekick::theme_sleek())
gg_trawl <- filter(ind, model == "Combined") |>
  ggplot(aes(year, est, group = model, colour = region, ymin = lwr, ymax = upr)) +
  facet_wrap(~region, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = cols_region) +
  geom_pointrange(data = filter(ind, model != "Combined"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey40", alpha = 0.6) +
  geom_pointrange(
    # position = position_jitter(width = 0.2, height = 0),
    size = 0.2, pch = 21) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, xlim = c(1996, 2023)) +
  geom_line(aes(x = year, y = glm_pred), inherit.aes = FALSE, data = glmdf, lwd = .9, colour = "grey35") +
  # scale_colour_manual(values = c("Region-specific" = "grey60", "Combined" = blues[3])) +
  theme(legend.position.inside = c(0.25, 0.86), legend.position = "inside", axis.title.x = element_blank()) +
  guides(colour = "none") +
  labs(x = "Year", y = "Trawl survey biomass index", colour = "Model") +
  geom_text(data = lab_pos, mapping = aes(y = max_y * 0.9, label = region_lab), x = 2022,
    inherit.aes = FALSE, vjust = 0.5, hjust = 1, size = 3) +
  theme(strip.text.x = element_blank(), strip.background.x = element_blank(), panel.spacing.y = unit(-0.1, "lines"))
gg_trawl
# ggsave("figs/fig1.pdf", width = 2.8, height = 7)

# pull in IPHC:

ind_ll <- readRDS("output/indexes-iphc-nb2-coastwide.rds")
ind_ll$region <- gsub("^Coast$", "Coastwide", ind_ll$region)
ind_ll <- ind_ll |>
  mutate(region = factor(region,
    levels = c("Coastwide", "Gulf of Alaska", "British Columbia", "US West Coast")))
ind_ll <- mutate(ind_ll, est = est / 1000, lwr = lwr / 1000, upr = upr / 1000)

glmdf_ll <- ind_ll |> #filter(year >= 2006) |>
  group_by(region) |>
  group_split() |>
  purrr::map_dfr(\(x) {
    x$decade <- x$year / 10
    # x$weights <- 1 / x$se
    # x$weights <- x$weights / mean(x$weights)
    # m <- glm(est ~ decade, data = x, family = Gamma(link = "log"))
    m <- mgcv::gam(est ~ s(decade, k = 6), data = x, family = Gamma(link = "log"))
    # m <- sdmTMB::sdmTMB(est ~ s(decade), data = x, family = lognormal(link = "log"), spatial = "off")
    nd <- data.frame(year = seq(min(x$year), max(x$year), length.out = 200))
    nd$decade = nd$year / 10
    p <- predict(m, newdata = nd)
    ret <- data.frame(nd, glm_pred = exp(p))
    row.names(ret) <- NULL
    ret$region <- x$region[1]
    ret$slope <- coef(m)[[2]]
    ret
  })


lab_pos <- ind_ll |> group_by(region) |>
  summarise(max_y = max(upr)) |>
  mutate(region_lab = paste0("(", letters[5:8], ") ", region))
  # mutate(region_lab = paste0("(", letters[5:8], ") "))

gg_iphc <- ind_ll |>
  ggplot(aes(year, est, colour = region)) +
  scale_colour_manual(values = cols_region) +
  facet_wrap(~region, scales = "free_y", ncol = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.2, pch = 21) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, xlim = c(1996, 2023)) +
  geom_line(aes(x = year, y = glm_pred), inherit.aes = FALSE, data = glmdf_ll, lwd = .9, colour = "grey35") +
  geom_text(data = lab_pos, mapping = aes(y = max_y * 0.9, label = region_lab), x = 2022,
    inherit.aes = FALSE, vjust = 0.5, hjust = 1, size = 3) +
  theme(legend.position.inside = c(0.25, 0.86), legend.position = "inside", axis.title.x = element_blank()) +
  guides(colour = "none") +
  labs(x = "Year", y = "IPHC longline survey abundance index", colour = "Model") +
  theme(strip.text.x = element_blank(), strip.background.x = element_blank(), panel.spacing.y = unit(-0.1, "lines"))
gg_iphc

g <- cowplot::plot_grid(gg_trawl, gg_iphc, ncol = 2, align = "h")
print(g)

ggsave("figs/overall-survey-trends.pdf", width = 4.8, height = 5.5)
ggsave("figs/overall-survey-trends.png", width = 4.8, height = 5.5)
