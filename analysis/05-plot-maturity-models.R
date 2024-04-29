library(ggplot2)
library(dplyr)

indexes <- readRDS("output/index-trawl-by-maturity-poisson-link.rds")
# fits <- readRDS("output/fit-trawl-by-maturity-poisson-link.rds")
indexes
# fits

indexes$region <- factor(indexes$region, levels = c("Coast", "GOA", "BC", "NWFSC"))

glmdf <- indexes |> filter(year >= 2006) |>
  group_by(region, group) |>
  group_split() |>
  purrr::map_dfr(\(x) {
    x$decade <- x$year / 10
    m <- glm(est ~ decade, data = x, family = Gamma(link = "log"))
    nd <- data.frame(year = seq(min(x$year), max(x$year)))
    nd$decade = nd$year / 10
    p <- predict(m, newdata = nd, type = "response")
    ret <- data.frame(nd, glm_pred = p)
    row.names(ret) <- NULL
    ret$group = x$group[1]
    ret$region <- x$region[1]
    ret$slope <- coef(m)[[2]]
    ci <- confint(m)
    ret$lwr <- ci[2, 1]
    ret$upr <- ci[2, 2]
    ret
  })

glmdf |> select(group, region, slope, lwr, upr) |> distinct() |>
  ggplot(aes(slope, region, xmin = lwr, xmax = upr, colour = group)) +
  geom_pointrange(position = position_dodge(width = 0.2)) +
  geom_vline(xintercept = 0, lty = 2)

lu <- data.frame(
  group = c("mm", "mf", "maturingm", "maturingf", "imm"),
  group_clean = c("Mature males", "Mature females", "Maturing males", "Maturing females", "Immature"),
  stringsAsFactors = FALSE
)
indexes <- left_join(indexes, lu)
glmdf <- left_join(glmdf, lu)

lvls <- rev(c("Immature", "Maturing females", "Maturing males", "Mature males", "Mature females"))
indexes$group_clean <- factor(indexes$group_clean, levels = lvls)
glmdf$group_clean <- factor(glmdf$group_clean, levels = lvls)

glmdf |> select(group_clean, region, slope, lwr, upr) |> distinct() |>
  mutate(slope = exp(slope), lwr = exp(lwr), upr = exp(upr)) |>
  ggplot(aes(slope, group_clean, xmin = lwr, xmax = upr, colour = region)) +
  geom_pointrange(position = position_dodge(width = 0.3), pch = 21) +
  geom_vline(xintercept = 1, lty = 2) +
  scale_colour_manual(values = c("grey30", RColorBrewer::brewer.pal(3, "Set2")), guide = guide_legend(reverse = TRUE)) +
  # scale_colour_brewer(palette = "Set2") +
  ggsidekick::theme_sleek() +
  scale_x_log10() +
  theme(axis.title.y.left = element_blank()) +
  xlab("Multiplicative population\nchange per decade") +
  labs(colour = "Region")

indexes$group_clean <- forcats::fct_rev(indexes$group_clean)
glmdf$group_clean <- forcats::fct_rev(glmdf$group_clean)
ggplot(indexes, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_line() + geom_ribbon(alpha = 0.2) +
  # facet_grid(group~region, scales = "free_y")
  facet_grid(region ~ group_clean, scales = "free_y") +
  ggsidekick::theme_sleek() +
  coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  geom_line(aes(x = year, y = glm_pred), inherit.aes = FALSE, data = glmdf, lwd = 0.7, colour = "red")

indexes |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = group, fill = group)) +
  geom_line() +
  # scale_y_log10() +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  geom_ribbon(alpha = 0.2, colour = NA) +
  facet_wrap(~region, scales = "free_y", nrow = 4) +
  ggsidekick::theme_sleek() +
  coord_cartesian(ylim = c(0, NA), expand = FALSE)

