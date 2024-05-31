# Code is for plot of length densities in SOM


# library -----------------------------------------------------------------

library(TMB)
library(sp)
library(sdmTMB)
library(here)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggsidekick)
library(kableExtra)
library(sf)
theme_set(ggsidekick::theme_sleek())

source("analysis/999-colours-etc.R")

# notes -------------------------------------------------------------------

# NOTE: did this in 01-wrange US data.R
# convert Fork Length to total length
# LPC to LText 1483 3·49 (2·94–4·00) 1·20 (1·20–1·20) 0·98
# LF to LText 876 2·17 (1·35–2·98) 1·10 (1·09–1·11) 0·98
# LTnat to LText 953 1·64 (0·97–2·31) 1·02 (1·01–1·03) 0·98
# tribuzio et al Life history characteristics of a lightly exploited stock of squalus suckleyi

# BC tail extended
# From Gertserva et al:
# WCGBT survey: Lengths in WCGBT Survey are total length (natural)
# Triennial (2001 and 2004): lengths are total length
# AFSC Slope Survey: Fork lengths
# 1998 Triennial Survey: Fork lengths

# maturity information
library(gfplot)
data("maturity_assignment")
data("maturity_short_names") # males maturity code = 90, female maturity code is >= 77
maturity_assignment
maturity_short_names

# 94 cm tl for females is mature based on DFO assessment
# born at 26 and 27 cm.
# suggest growth of 1.5 cm per year.
# 15 year old dogfish would be about ~50 cm
# Males 70 cm mature


# load data ---------------------------------------------------------------

samps <- readRDS("output/samps_CoastalTrawl.rds") |>
  drop_na(length_ext_cm) |>
  # some earlier surveys I didn't know the conversion for
  mutate(survey_name2 = ifelse(survey_name == "NWFSC.Combo",
    survey_abbrev,
    survey_name
  ))


# SOM Figure for manuscript: density figure of IPHC and trawl, regions combined -------------------------------

breakpts <- c(c(1996, 2002), seq(2003, 2023, 10))

sampsTL <- readRDS("output/samps_CoastalTrawl.rds") |>
  drop_na(length_ext_cm) |>
  mutate(survey_name2 = ifelse(survey_name == "NWFSC.Combo",
    survey_abbrev,
    survey_name
  )) |>
  filter(!survey_name %in% c("Triennial", "AFSC.Slope"))

samps_hist <- sampsTL %>%
  mutate(year_group = findInterval(year, breakpts)) |>
  drop_na(length_ext_cm) |>
  filter(!sex %in% c(0, 3)) |>
  mutate(survey_name2 = ifelse(survey_name2 == "BC", "British Columbia",
    ifelse(survey_name == "GOA", "Gulf of Alaska",
      "US West Coast"
    )
  )) |>
  mutate(survey_name3 = ifelse(survey_name2 == "BC", "BC",
    ifelse(survey_name == "GOA", "GOA",
      ifelse(survey_abbrev == "NWFSC.Combo.pass1", "nwpass1",
        "nwpass2"
      )
    )
  )) |>
  dplyr::select(year, length_ext_cm, sex, survey_name2, survey_name3, year_group) |>
  mutate(survey_group = "trawl")



samps_hist$survey_name2 <- factor(samps_hist$survey_name2,
  levels = c("Gulf of Alaska", "British Columbia", "US West Coast")
)

year.labs <- c("'96-02", "'03-'13", "'14-'22")
names(year.labs) <- c("1", "3", "4")


survey.labs <- c("US West Coast", "British Columbia", "Gulf of Alaska")
names(survey.labs) <- c("US West Coast", "British Columbia", "Gulf of Alaska")

sex.labs <- c("Males", "Females")
names(sex.labs) <- c("1", "2")

samps_hist$mature <- ifelse(samps_hist$sex == 2, 95.5, 76.7) # see 999-split-index_by-region-maturity.R for maturity ogive
samps_hist$immature <- ifelse(samps_hist$sex == 2, 76.7, 64.9)


x <- PNWColors::pnw_palette("Cascades", 3)
samps_hist$survey_group <- factor(samps_hist$survey_group,
  levels = c("trawl", "iphc")
)

line_dat_mature <- dplyr::select(samps_hist, mature) |>
  distinct()
line_dat_immature <- dplyr::select(samps_hist, immature) |>
  distinct() |>
  rename(mature = immature)
line_dat <- bind_rows(line_dat_mature, line_dat_immature) |>
  filter(mature != 76.7)

hist <- ggplot(samps_hist, aes(length_ext_cm, group = as.factor(survey_name2), fill = as.factor(survey_name2)),
  colour = as.factor(survey_name2)
) +
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  geom_vline(
    data = line_dat,
    aes(xintercept = mature), colour = "grey40"
  ) +
  facet_grid(
    rows = vars(survey_group),
    cols = vars(sex), # , scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs, survey_name2 = survey.labs)
  ) +
  coord_cartesian(expand = FALSE, ylim = c(0, 0.05), xlim = c(0, 126)) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)",
  ) +
  scale_y_continuous(
    breaks = c(0, 0.02, 0.04, 0.06, 0.08),
    labels = c(0, 0.02, 0.04, 0.06, 0.08), name = "Density"
    # expand = expansion(mult = c(0, 0.03))
  ) +
  scale_fill_manual("Region", values = cols_region3) +
  scale_colour_manual(values = cols_region3) +
  ggsidekick::theme_sleek() +
  theme(legend.position.inside = c(0.2, 0.8), legend.position = "inside")
hist
ggsave("figs/length-distributions-trawl.pdf", width = 5, height = 4)
ggsave("figs/length-distributions-trawl.png", width = 5, height = 4)


# maturity ogive ----------------------------------------------------------
# pull output from 999-split-index-by-region-maturity.R
object <- readRDS("output/survey_samples_codedmaturity.rds")
gfplot::plot_mat_ogive(m)

x <- m$pred_data
n_re <- length(unique(nd_re$sample_id)) / 5
n_re2 <- ifelse(n_re < 15, 15, n_re)

# tally by length group and calculate weight of each group
m$pred_data <- m$pred_data |>
  mutate(cat = ifelse(female == 1 & glmm_fe >= 0.95, 1,
    ifelse(female == 0 & glmm_fe >= 0.95, 2,
      ifelse(female == 1 & glmm_fe < 0.05, 3,
        ifelse(female == 0 & glmm_fe < 0.05, 4,
          NA
        )
      )
    )
  ))
m$pred_data <- m$pred_data |>
  group_by(cat) |>
  mutate(matlength = ifelse(cat %in% c(1, 2), min(age_or_length),
    ifelse(cat %in% c(3, 4), max(age_or_length),
      ifelse(is.na(cat == TRUE), NA, NA)
    )
  ))
data <- m$pred_data |>
  group_by(female, cat) |>
  summarize(unique = unique(matlength))
data
ann_text <- data.frame(
  age_or_length = c(40, 40, 40, 40), glmm_re = c(0.75, 0.68, 0.60, 0.53),
  lab = c("F05 = 76.7", "F95 = 95.6", "M05 = 64.9", "M95 = 76.7"),
  female = factor(c("1", "1", "0", "0"), levels = c("0", "1"))
)

g <- ggplot() +
  geom_line(
    data = nd_re, aes_string("age_or_length",
      "glmm_re",
      group = "paste(sample_id, female)",
      colour = "female"
    ), inherit.aes = FALSE, alpha = 1 / n_re2,
    show.legend = FALSE
  ) +
  geom_line(size = 1) +
  geom_rug(
    data = filter(m$pred_data, glmm_re < 0.5), aes(age_or_length, glmm_re,
      colour = female
    ),
    sides = "b",
    alpha = 0.05, lty = 1, show.legend = FALSE
  ) +
  geom_rug(
    data = filter(m$pred_data, glmm_re >= 0.5), aes(age_or_length, glmm_re,
      colour = female
    ),
    sides = "t",
    alpha = 0.05, lty = 1, show.legend = FALSE
  ) +
  labs(x = "Length (cm)", y = "Probability mature") +
  geom_vline(
    data = m$pred_data, aes(xintercept = matlength, colour = female),
    show.legend = FALSE
  ) +
  facet_wrap(~female) +
  theme(strip.text.x = element_blank())#
#          theme(plot.margin = unit(c(0,-3,0,-3), "cm")) )

g + geom_text(data = ann_text, aes(age_or_length, glmm_re, label = lab))
ggsave("Figures/maturityogive.png", width = 7, height = 3)


p <- ggplot() +
  geom_line(
    data = nd_re, aes_string("age_or_length",
                             "glmm_re",
                             group = "paste(sample_id, female)",
                             colour = "female"
    ), inherit.aes = FALSE, alpha = 1 / n_re2,
    show.legend = FALSE
  ) +
  geom_line(size = 1) +
  geom_rug(
    data = filter(m$pred_data, glmm_re < 0.5), aes(age_or_length, glmm_re,
                                                   colour = female
    ),
    sides = "b",
    alpha = 0.05, lty = 1, show.legend = FALSE
  ) +
  geom_rug(
    data = filter(m$pred_data, glmm_re >= 0.5), aes(age_or_length, glmm_re,
                                                    colour = female
    ),
    sides = "t",
    alpha = 0.05, lty = 1, show.legend = FALSE
  ) +
  labs(x = "Length (cm)", y = "Probability mature") +
  geom_vline(
    data = m$pred_data, aes(xintercept = matlength, colour = female),
    show.legend = FALSE
  ) +
  #facet_wrap(~female) +
  theme(strip.text.x = element_blank())

p + geom_text(data = ann_text, aes(age_or_length, glmm_re, label = lab))
ggsave("Figures/maturityogive_nofacet.png", width = 4, height = 3)


# cowplot both figures ----------------------------------------------------

p
hist
g

cowplot::plot_grid(p, hist,
                   labels=c("(a)","(b)"),
                   rel_heights = c(1, 1),
                   nrow = 1,
                   ncol = 2,
                   label_size = 12,
                   rel_widths = c(1,2),
                   label_fontfamily = "sans")
