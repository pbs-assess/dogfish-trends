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


#do we end up using lengths from IPHC and
# iphcsamps <- read.csv("data-raw/Cindy - IPHC sampling information/IPHC_dogfish_lengths2021.csv") |> # need to remove expansion set etc.
#   mutate(
#     length_pcl = length # lengths are in precaudal length (PCL)
#   ) |>
#   mutate(length = length_pcl * 1.20)


samps <- readRDS("output/samps_CoastalTrawl.rds") |>
  drop_na(length_ext_cm) |>
  # some earlier surveys I didn't know the conversion for
  mutate(survey_name2 = ifelse(survey_name == "NWFSC.Combo",
    survey_abbrev,
    survey_name
  ))



# SOM figure for manuscript: trawl AND iphc density plots by sex and region ----------------------------------------------------

breakpts <- c(c(1986, 1996, 2002), seq(2003, 2023, 10))
breakpts <- seq(1996, 2022, 7)
samps_hist <- samps %>%
  mutate(year_group = findInterval(year, breakpts)) |>
  drop_na(length_ext_cm) |>
  filter(!sex %in% c(0, 3))

# ggplot(samps_hist, aes(length_ext_cm, group = as.factor(year_group), fill = as.factor(year_group)),
#   colour = as.factor(year_group)
# ) + # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
#   geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
#   facet_grid(~ survey_name2 + sex)


samps <- filter(samps, !survey_name %in% c("AFSC.Slope", "Triennial"))
breakpts <- seq(1996, 2022, 7)

# samps_hist |>
#   mutate(length_ext_cm = as.numeric(length_ext_cm)) |>
#   filter(length_ext_cm > 12) |>
#   group_by(survey_name) |>
#   summarize(sex_length_mean = mean(length_ext_cm)) |>
#   ungroup()
#
# samps |>
#   filter(sex %in% c(1, 2)) |>
#   filter(length_ext_cm <= 12)

# samps |>
#   filter(sex %in% c(1, 2)) |>
#   filter(length_ext_cm > 12) |>
#   group_by(sex) |>
#   drop_na(length_ext_cm) |>
#   summarise(min = min(length_ext_cm), max = max(length_ext_cm))

# samps |>
#   filter(sex %in% c(1, 2)) |>
#   group_by(sex) |>
#   drop_na(length_ext_cm) |>
#   summarise(min = min(length_ext_cm), max = max(length_ext_cm))

# samps |>
#   filter(sex %in% c(1, 2)) |>
#   filter(length_ext_cm > 12) |>
#   group_by(survey_name2, sex) |>
#   drop_na(length_ext_cm) |>
#   summarise(
#     min = min(length_ext_cm), max = max(length_ext_cm), mean(length_ext_cm),
#     median(length_ext_cm)
#   )

dissamps_hist <- samps %>%
  mutate(year_group = findInterval(year, breakpts)) |>
  drop_na(length_ext_cm) |>
  filter(!sex %in% c(0, 3))

# means <- samps_hist |>
#   mutate(length_ext_cm = as.numeric(length_ext_cm)) |>
#   group_by(year_group, survey_name2, sex) |>
#   summarize(sex_length_mean = mean(length_ext_cm)) |>
#   ungroup()

samps_hist$survey_name2 <- factor(samps_hist$survey_name2,
  levels = c("GOA", "BC", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1")
)

year.labs <- c("'96-02", "'03-'09", "'10-'16", "'17-'22")
names(year.labs) <- c("1", "2", "3", "4")


# label names
unique(samps_hist$survey_name2)
samps_hist$survey_name2 <- factor(samps_hist$survey_name2,
  levels = c("GOA", "BC", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1")
)

survey.labs <- c("NWFSC 1", "NWFSC 2", "BC", "GOA")
names(survey.labs) <- c("NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "BC", "GOA")

sex.labs <- c("Males", "Females")
names(sex.labs) <- c("1", "2")

samps_hist$mature <- ifelse(samps_hist$sex == 2, 97, 70) # mature code 55

# install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)
x <- wes_palette("Moonrise2", n = 4, type = c("continuous"))

# install.packages("PNWColors")
x <- PNWColors::pnw_palette(name = "Mushroom", n = 6, type = "discrete")

ggplot(samps_hist, aes(length_ext_cm, group = as.factor(year_group), fill = as.factor(year_group)),
  colour = as.factor(year_group)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature)
  ) +
  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    rows = vars(survey_name2), cols = vars(sex), scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs, survey_name2 = survey.labs)
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)"
  ) +
  scale_y_continuous(breaks = c(0, 0.03, 0.06), labels = c(0, 0.03, 0.06), name = "Density") +
  # scale_fill_viridis_d( ) +
  # scale_colour_manual(values = "black") +
  scale_fill_manual("Years", values = x, labels = c("'96-02", "'03-'09", "'10-'16", "'17-'22")) +
  scale_colour_manual(values = x) +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 10, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 10, colour = c("grey20")),
    axis.title.x = element_text(size = 10, colour = "grey20"),
    axis.title.y = element_text(size = 10, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )

ggsave("Figures/length_summary_all.png", width = 6, height = 4)





# SOM figure: density plot IPHC, by sex and region ----------------------------------------------------

iphcsamps <- read.csv("data-raw/Cindy - IPHC sampling information/IPHC_dogfish_lengths2021.csv") |> # need to remove expansion set etc.
  mutate(
    length_pcl = length # lengths are in precaudal length (PCL)
  ) |>
  mutate(length_ext_cm = length_pcl * 1.20) |>
  filter(fmp != "BSAI") # need to remove expansion set etc.

range(iphcsamps$length_ext_cm, na.rm = TRUE)
iphcsamps |>
  filter(sex %in% c("M", "F")) |>
  group_by(fmp, sex) |>
  drop_na(length_ext_cm) |>
  summarise(min = min(length_ext_cm), max = max(length_ext_cm))

iphcsamps |>
  filter(sex %in% c("M", "F")) |>
  group_by(fmp, sex) |>
  drop_na(length_ext_cm) |>
  summarise(min = min(length_ext_cm), max = max(length_ext_cm))

breakpts <- seq(2011, 2022, 5)
breakpts <- c(breakpts[1], breakpts[2], 2022)

unique(iphcsamps$sex)
samps_hist <- iphcsamps %>%
  mutate(year_group = findInterval(year, breakpts), sexMF = sex, survey_name = "IPHC") |>
  drop_na(length_ext_cm) |>
  mutate(sex = ifelse(sexMF == "F", 2, ifelse(sexMF == "M", 1, 0))) |>
  filter(sex %in% c(1, 2))

samps_hist$fmp <- factor(samps_hist$fmp,
  levels = c("GOA", "CAN", "WC")
)


samps_hist$mature <- ifelse(samps_hist$sex == 2, 97, 70) # mature code 55

range(samps_hist$length_ext_cm)

means <- samps_hist |>
  mutate(length_ext_cm = as.numeric(length_ext_cm)) |>
  filter(length_ext_cm > 12) |>
  group_by(year_group, survey_name, sex) |>
  summarize(sex_length_mean = mean(length_ext_cm)) |>
  ungroup()
means
unique(means$year_group)
year.labs <- c("'11-'16", "'17-'21")
names(year.labs) <- c("1", "2")


# label names
sex.labs <- c("Males", "Females")
names(sex.labs) <- c("1", "2")


# install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)
wes_palette("FantasticFox1", 3, type = c("continuous"))
x <- wes_palette("Moonrise2", n = 4, type = c("continuous"))
x2 <- c(x[2], x[1])

ggplot(samps_hist, aes(length_ext_cm, group = as.factor(year_group), fill = as.factor(year_group)),
  colour = as.factor(year_group)
) +
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature)
  ) +
  facet_grid(
    rows = vars(fmp), cols = vars(sex), scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs)
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)"
  ) +
  scale_y_continuous(breaks = c(0, 0.03, 0.06), labels = c(0, 0.03, 0.06), name = "Density") +
  scale_fill_manual("Years", values = x2, labels = c("'11-'15", "'16-'21")) +
  scale_colour_manual(values = x2) +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 10, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 10, colour = c("grey20")),
    axis.title.x = element_text(size = 10, colour = "grey20"),
    axis.title.y = element_text(size = 10, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )

ggsave("Figures/length_summary_iphc.png", width = 6, height = 4)





# iphc size class figures by region ---------------------------------------
unique(samps_hist$fmp)
samps_hist <- samps_hist |> filter(fmp %in% c("WC", "CAN", "GOA"))


# SOM Figure for manuscript: density figure of IPHC and trawl, regions combined -------------------------------

breakpts <- c(c(1996, 2002), seq(2003, 2023, 10))

sampsTL <- readRDS("output/samps_CoastalTrawl.rds") |>
  drop_na(length_ext_cm) |>
  mutate(survey_name2 = ifelse(survey_name == "NWFSC.Combo",
    survey_abbrev,
    survey_name
  )) |>
  filter(!survey_name %in% c("Triennial", "AFSC.Slope"))

unique(sampsTL$survey_name)

samps_histtl <- sampsTL %>%
  mutate(year_group = findInterval(year, breakpts)) |>
  drop_na(length_ext_cm) |>
  filter(!sex %in% c(0, 3)) |>
  mutate(survey_name2 = ifelse(survey_name2 == "BC", "BC",
    ifelse(survey_name == "GOA", "GOA",
      "WC"
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

samps_histip <- iphcsamps %>%
  mutate(year_group = findInterval(year, breakpts), sexMF = sex) |>
  drop_na(length) |>
  mutate(sex = ifelse(sexMF == "F", 2, ifelse(sexMF == "M", 1, 0))) |>
  filter(sex %in% c(1, 2)) |>
  mutate(
    survey_name2 = ifelse(fmp == "CAN", "BC", fmp),
    survey_name3 = survey_name2
  ) |>
  dplyr::select(year, length_ext_cm = length, sex, survey_name2, survey_name3, year_group) |>
  mutate(survey_group = "iphc")

samps_hist <- rbind(samps_histip, samps_histtl)
unique(samps_hist$survey_name2)
unique(samps_hist$survey_name3)

samps_hist$survey_name2 <- factor(samps_hist$survey_name2,
  levels = c("GOA", "BC", "WC")
)

year.labs <- c("'96-02", "'03-'13", "'14-'22")
names(year.labs) <- c("1", "3", "4")


survey.labs <- c("WC", "BC", "GOA")
names(survey.labs) <- c("WC", "BC", "GOA")

sex.labs <- c("Males", "Females")
names(sex.labs) <- c("1", "2")

samps_hist$mature <- ifelse(samps_hist$sex == 2, 95.5, 76.7) # see split-index_byregionsandmaturity.R for maturity ogive
samps_hist$immature <- ifelse(samps_hist$sex == 2, 77, 65.1)

# install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)
x2 <- wes_palette("FantasticFox1", 3, type = c("continuous"))
# x <- wes_palette("Moonrise2", n = 4, type = c("continuous"))
# x2 <- c(x[3], x[2], x[1])

ggplot(samps_histtl, aes(length_ext_cm, group = as.factor(survey_name2), fill = as.factor(survey_name2)),
  colour = as.factor(survey_name2)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature)
  ) +

  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    cols = vars(sex)
  )

ggplot(samps_hist, aes(length_ext_cm, group = as.factor(survey_name3), fill = as.factor(survey_name3)),
  colour = as.factor(survey_name3)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature)
  ) +

  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    rows = vars(survey_group), cols = vars(sex), # , scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs, survey_name = survey.labs)
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)"
  ) +
  scale_y_continuous(breaks = c(0, 0.05, 0.09), labels = c(0, 0.05, 0.09), name = "Density") +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 10, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 10, colour = c("grey20")),
    axis.title.x = element_text(size = 10, colour = "grey20"),
    axis.title.y = element_text(size = 10, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )

ggsave("Figures/length_summary_trawliphc_pass1pass2.png", width = 6, height = 4)


ggplot(samps_hist, aes(length_ext_cm, group = as.factor(survey_name2), fill = as.factor(survey_name2)),
  colour = as.factor(survey_name2)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature)
  ) +

  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    rows = vars(survey_group), cols = vars(sex), # , scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs, survey_name = survey.labs)
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)"
  ) +
  scale_y_continuous(breaks = c(0, 0.05, 0.09), labels = c(0, 0.05, 0.09), name = "Density") +
  scale_fill_manual("Region", values = x2) + # , labels = c("'96-02", "'03-'13", "'14-'22")) +
  # scale_colour_manual(values = "black") +
  scale_fill_grey("Region") +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 10, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 10, colour = c("grey20")),
    axis.title.x = element_text(size = 10, colour = "grey20"),
    axis.title.y = element_text(size = 10, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )

ggsave("Figures/length_summary_trawliphc.png", width = 6, height = 4)

unique(samps_hist$mature)


x <- PNWColors::pnw_palette("Cascades", 3)
samps_hist$survey_group <- factor(samps_hist$survey_group,
  levels = c("trawl", "iphc")
)

# ggplot(filter(samps_hist, survey_group == "trawl"), aes(length_ext_cm, group =
#                                                           as.factor(survey_name2),
#                                                         fill = as.factor(survey_name2)),
#        colour = as.factor(survey_name2)
# ) +
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
  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
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
  scale_fill_manual("Region", values = x) + # , labels = c("'96-02", "'03-'13", "'14-'22")) +
  # scale_colour_manual(values = "black") +
  # scale_fill_grey("Region") +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
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





# erase all of this??
# Scale counts by catch weight and catch sampled - ignore? --------------------------

# create weighting
# 1. avg weight fish = tow weight/avg weight of fish sampled
# 2. est catch count/how many sampled = weight

glimpse(samps)
glimpse(sets)


# Exploratory figures both - maps and trends by sex and maturity ----------------------------------
# sets <- readRDS("output/sets_CoastalTrawl.rds")
sets <- readRDS("output/Wrangled_USCanData.rds") |>
  mutate(fishing_event_id = as.numeric(fishing_event_id))
samps <- readRDS("output/samps_CoastalTrawl.rds") |>
  mutate(fishing_event_id = as.numeric(fishing_event_id))


tl_sexsumM <- samps %>%
  left_join(sets[c("fishing_event_id", "longitude", "latitude", "year")]) %>%
  group_by(year, fishing_event_id, sex) %>%
  filter(sex == 1) %>%
  filter(length_ext_cm > 80) %>%
  summarize(count = n())

# ggplot() +
#   geom_jitter(
#     data = tl_sexsumM,
#     aes(longitude, latitude, colour = log(count)), size = 1.5
#   ) +
#   facet_wrap(~year, ncol = 2) +
#   scale_colour_viridis_c()
#
years <- data.frame(
  year = seq(2003, 2022, 1),
  group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 9, 9)
)

samps_yeargroup <- left_join(samps, years)
samps_yeargroup %>%
  group_by(year, sex, survey_name) |>
  filter(sex != 3) |>
  summarize(count = n()) |>
  ggplot() +
  geom_line(aes(year, count, group = as.factor(sex), colour = as.factor(sex)),
    size = 1
  ) +
  facet_wrap(~survey_name) +
  theme_classic()


goa_samps3 %>%
  group_by(year, sex, survey_name) |>
  filter(sex != 3) |>
  summarize(count = n()) |>
  ggplot() +
  geom_line(aes(year, count, group = as.factor(sex), colour = as.factor(sex)),
    size = 1
  ) +
  facet_wrap(~survey_name) +
  theme_classic()

samps_yeargroup %>%
  group_by(year, sex) |>
  filter(length_ext_cm >= 70 & sex != 3) |>
  summarize(count = n()) |>
  ggplot() +
  geom_line(aes(year, count, group = as.factor(sex), colour = as.factor(sex)),
    size = 1
  ) +
  theme_classic()

p1 <- samps_yeargroup %>%
  group_by(year, sex, survey_name) |>
  filter(length_ext_cm >= 70 & sex != 3) |>
  summarize(count = n()) |>
  ggplot() +
  geom_line(aes(year, count, group = as.factor(sex), colour = as.factor(sex)),
    size = 1
  ) +
  facet_wrap(~survey_name) +
  theme_classic()

samps_yeargroup %>%
  group_by(year, sex, survey_name) %>%
  filter(length_ext_cm < 70 & sex != 3) %>%
  summarize(count = n()) |>
  ggplot() +
  geom_line(aes(year, count, colour = as.factor(sex)),
    size = 1
  ) +
  facet_wrap(~survey_name)

x3 <- samps_yeargroup %>%
  group_by(year) %>%
  filter(sex != 3) %>%
  summarize(count = n())

p1 + geom_line(
  data = x3, aes(year, count),
  colour = "grey50", size = 1
) +
  facet_wrap(~survey_name)


p1 <- samps_yeargroup %>%
  group_by(year, sex) |>
  filter(sex != 3) |>
  summarize(count = n()) |>
  ggplot() +
  geom_line(aes(year, count, group = as.factor(sex), colour = as.factor(sex)),
    size = 1
  ) +
  theme_classic()



# 10cm bins for males and females
range(na.omit(samps_yeargroup$length_ext_cm))
breakpts <- seq(20, 120, 10)

unique(x3$length_group)
labels <- as_labeller(c(
  `1` = "20-30cm", `2` = "30-40cm", `3` = "40-50cm", `4` = "50-60cm",
  `5` = "60-70cm", `6` = "70-80cm",
  `7` = "80-90cm", `8` = "90-100cm",
  `9` = "100-110cm", `10` = "110-120cm"
))
x3 <- samps_yeargroup %>%
  mutate(length_group = findInterval(length_ext_cm, breakpts)) |>
  drop_na(length_ext_cm) |>
  filter(!sex %in% c(0, 3)) %>%
  filter(year >= 2003) |>
  # left_join(labels ) |>
  group_by(sex, year, length_group) %>%
  mutate(count = n()) |>
  ungroup() |>
  group_by(sex, length_group) |>
  mutate(count2003 = count[which.min(year)]) |>
  distinct(sex, year, .keep_all = TRUE) |>
  dplyr::select(year, sex, count, count2003, length_group) |>
  mutate(count_st = (count / count2003))


ggplot() +
  geom_line(data = filter(x3, length_group > 3), aes(year, count_st, colour = as.factor(sex)), size = 1.5) +
  theme_classic() +
  facet_wrap(~length_group, labeller = labels) +
  geom_point(data = filter(x3, length_group > 3), aes(year, count_st, colour = as.factor(sex)), size = 2) +
  scale_color_manual(values = c("#C7B6C8", "#74B6BA", "red"))

test <- x3 |>
  group_by(sex, length_group) |>
  mutate(lastyear = count[which(year == 2022)][1]) |>
  mutate(firstyr = count[which(year == 2012)][1]) |>
  mutate(yr2003 = count[which(year == 2003)][1]) |>
  summarize(
    perdiff10yr = round(((lastyear - firstyr) / firstyr) * 100, 1),
    perdiff20yr = round(((lastyear - yr2003) / yr2003) * 100, 1)
  ) |>
  distinct()


# create weigting
# 1. avg weight fish = tow weight/avg weight of fish sampled
# 2. est catch count/how many sampled = weight

ggplot() +
  geom_line(data = x3, aes(year, log(count), colour = as.factor(sex)), size = 1) +
  theme_classic() +
  facet_wrap(~length_group, labeller = labels, nrow = 1) +
  geom_point(data = x3, aes(year, log(count), colour = as.factor(sex)), size = 2) +
  scale_color_manual(values = c("#C7B6C8", "#74B6BA")) +
  geom_text(
    data = filter(test, sex == 1), aes(x = 2010, y = 0.05, label = paste0(
      perdiff20yr, "%,",
      perdiff10yr, "%"
    )),
    inherit.aes = FALSE,
    col = "#C7B6C8"
  ) +
  geom_text(
    data = filter(test, sex == 2), aes(x = 2010, y = 0.5, label = paste0(
      perdiff20yr, "%,",
      perdiff10yr, "%"
    )),
    inherit.aes = FALSE,
    col = "#74B6BA"
  )


x4 <- filter(x3, year %in% c(2003, 2007, 2011, 2016, 2022))
ggplot() +
  geom_line(data = x4, aes(length_group, (count), colour = as.factor(sex)), size = 1) +
  theme_classic() +
  facet_wrap(~year, nrow = 1) #+
geom_point(data = x4, aes(length_group, (count), colour = as.factor(sex)), size = 2) +
  scale_color_manual(values = c("#C7B6C8", "#74B6BA"))

# end of


# Exploratory figures both - Split by sex and length ---------------------------------------------------

# calculate the proportion of all female tows
# nwfsc_sets$trawl_id <- as.character(nwfsc_sets$event_id)
# samps_tl$fishing_event_id <- as.character(samps_tl$fishing_event_id)

years <- data.frame(
  year = seq(2003, 2022, 1),
  group = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4)
)

samps_sum <- samps %>%
  group_by(year, fishing_event_id) %>%
  mutate(totalsampled = n()) %>%
  filter(sex == 2) %>%
  mutate(
    countfemales = n(),
    proportionfemale = countfemales / totalsampled,
    avglength = mean(length)
  ) %>%
  distinct(year, fishing_event_id, .keep_all = TRUE) %>%
  left_join(years) %>%
  mutate(prop_group = ifelse(proportionfemale < 0.35, 0,
    ifelse(proportionfemale > 0.65, 1, 0.5)
  ))

samps_sum <- samps %>%
  group_by(year, fishing_event_id) %>%
  mutate(totalsampled = n(), avglength = mean(length)) %>%
  filter(sex == 2) %>%
  mutate(countfemales = n(), proportionfemale = countfemales / totalsampled) %>%
  distinct(year, fishing_event_id, .keep_all = TRUE) %>%
  left_join(years) %>%
  mutate(prop_group = ifelse(proportionfemale < 0.35, 0,
    ifelse(proportionfemale > 0.65, 1, 0.5)
  ))

# trends in average size and sex ratio of dogfish caught in trawl survey. dots are tows, fewer tows with large females
# upper right quadrant,
unique(sort(samps$year))
years <- data.frame(year = (c(seq(2003, 2022, 1))), group = c(
  1, 1, 1, 1, 1,
  2, 2, 2, 2, 2,
  3, 3, 3, 3, 3,
  4, 4, 4, 4, 4
))

samps_sum <- samps %>%
  left_join(years) %>%
  filter(year >= 2003) |>
  group_by(group, fishing_event_id, survey_name) %>%
  summarize(totalsampled = n(), avglength = median(length))

propfemale <- samps %>%
  left_join(years) %>%
  group_by(group, fishing_event_id) %>%
  mutate(totalsampled = n(), avglength = mean(length)) %>%
  filter(sex == 2) %>%
  summarise(countfemales = n(), proportionfemale = countfemales / totalsampled) %>%
  distinct(group, fishing_event_id, .keep_all = TRUE) %>%
  mutate(prop_group = ifelse(proportionfemale < 0.35, 0,
    ifelse(proportionfemale > 0.65, 1, 0.5)
  ))
samps_sum <- left_join(propfemale, samps_sum) |>
  drop_na(group)

ggplot(samps_sum, aes(proportionfemale, avglength, col = survey_name)) +
  geom_jitter(width = 0.1) +
  facet_wrap(~group, nrow = 5) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 60)


# plot of the ratio of mature males to mature females
x <- samps %>% filter(sex == 1 & length > 70)
y <- samps %>% filter(sex == 2 & length > 90)
xy <- rbind(x, y)
ratio <- xy %>%
  group_by(year) %>%
  mutate(totalsampled = n()) %>%
  filter(sex == 2) %>%
  summarise(
    countfemales = n(), proportionfemale = countfemales / totalsampled,
    proportionmale = 1 - proportionfemale
  ) |>
  mutate("ratio (Males:Females)" = proportionmale / proportionfemale) %>%
  distinct(year, .keep_all = TRUE) %>%
  left_join(years)

# plot of m:f ratio
ggplot(filter(ratio, year >= 2003), aes(year, `ratio (Males:Females)`)) +
  geom_point() +
  geom_line() +
  theme_classic()

sets_tlprop <- inner_join(sets, samps_sum,
  by = c(
    "year" = "year",
    "fishing_event_id" = "fishing_event_id"
  )
)
ggplot(sets_tlprop, aes(longitude, latitude, colour = proportionfemale)) +
  geom_point() +
  facet_wrap(~group) +
  scale_colour_viridis_c()
ggplot(sets_tlprop, aes(longitude, latitude, colour = as.factor(prop_group))) +
  geom_point() +
  facet_wrap(~group) +
  scale_colour_viridis_d()
ggplot(sets_tlprop, aes(longitude, latitude, colour = (avglength))) +
  geom_point() +
  facet_wrap(~group) +
  scale_colour_viridis_c(trans = "log")
ggplot(sets_tlprop, aes(as.factor(year), proportionfemale)) +
  geom_boxplot() +
  scale_colour_viridis_c(trans = "log")


# maybe: calculate the depth and the distance from shore

# end of both
