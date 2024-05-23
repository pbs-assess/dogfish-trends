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

samps_hist<- sampsTL %>%
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


samps_hist$mature <- ifelse(samps_hist$sex == 2, 95.5, 76.7) # see split-index_byregionsandmaturity.R for maturity ogive
samps_hist$immature <- ifelse(samps_hist$sex == 2, 77, 65.1)



x <- PNWColors::pnw_palette("Cascades", 3)
samps_hist$survey_group <- factor(samps_hist$survey_group,
  levels = c("trawl", "iphc")
)

ggplot(samps_hist, aes(length_ext_cm, group = as.factor(survey_name2), fill = as.factor(survey_name2)),
  colour = as.factor(survey_name2)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature), colour = "grey70"
  ) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = immature), colour = "grey70"
  ) +
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    rows = vars(survey_group),
    cols = vars(sex), # , scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs, survey_name2 = survey.labs)
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)"
  ) +
  scale_y_continuous(
    breaks = c(0, 0.02, 0.04, 0.06, 0.08),
    labels = c(0, 0.02, 0.04, 0.06, 0.08), name = "Density"
  ) +
  scale_fill_manual("Region", values = cols_region3) +
  scale_colour_manual(values = cols_region3) +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 12, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 12, colour = c("grey20")),
    axis.title.x = element_text(size = 12, colour = "grey20"),
    axis.title.y = element_text(size = 12, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 12),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )
ggsave("Figures/length_summary_trawl.png", width = 5, height = 4)


