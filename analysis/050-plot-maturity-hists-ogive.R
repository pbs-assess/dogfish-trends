# Code is for plot of length densities

# library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(ggsidekick::theme_sleek())
devtools::install_github("eliocamp/tagger")
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




# density figure of iphc --------------------------------------------------
#come back and clean up

# iphcsamps <- read.csv("data-raw/Cindy - IPHC sampling information/IPHC_dogfish_lengths2021.csv") |> # need to remove expansion set etc.
#   mutate(
#     length_pcl = length # lengths are in precaudal length (PCL)
#   ) |>
#   mutate(length_ext_cm = length_pcl * 1.20) |>
#   filter(fmp != "BSAI") # need to remove expansion set etc.
#
# range(iphcsamps$length_ext_cm, na.rm = TRUE)
# iphcsamps |>
#   filter(sex %in% c("M", "F")) |>
#   group_by(fmp, sex) |>
#   drop_na(length_ext_cm) |>
#   summarise(min = min(length_ext_cm), max = max(length_ext_cm))
#
# iphcsamps |>
#   filter(sex %in% c("M", "F")) |>
#   group_by(fmp, sex) |>
#   drop_na(length_ext_cm) |>
#   summarise(min = min(length_ext_cm), max = max(length_ext_cm))
#
# breakpts <- seq(2011, 2022, 5)
# breakpts <- c(breakpts[1], breakpts[2], 2022)
#
# unique(iphcsamps$sex)
# samps_hist <- iphcsamps %>%
#   mutate(year_group = findInterval(year, breakpts), sexMF = sex, survey_name = "IPHC") |>
#   drop_na(length_ext_cm) |>
#   mutate(sex = ifelse(sexMF == "F", 2, ifelse(sexMF == "M", 1, 0))) |>
#   filter(sex %in% c(1, 2))
#
# samps_hist$fmp <- factor(samps_hist$fmp,
#                          levels = c("GOA", "CAN", "WC")
# )
#
#
# samps_hist$mature <- ifelse(samps_hist$sex == 2, 97, 70) # mature code 55
#
# range(samps_hist$length_ext_cm)
#
# means <- samps_hist |>
#   mutate(length_ext_cm = as.numeric(length_ext_cm)) |>
#   filter(length_ext_cm > 12) |>
#   group_by(year_group, survey_name, sex) |>
#   summarize(sex_length_mean = mean(length_ext_cm)) |>
#   ungroup()
# means
# unique(means$year_group)
# year.labs <- c("'11-'16", "'17-'21")
# names(year.labs) <- c("1", "2")
#
#
# # label names
# sex.labs <- c("Males", "Females")
# names(sex.labs) <- c("1", "2")
#
#
# # install.packages("wesanderson")
# library(wesanderson)
# names(wes_palettes)
# wes_palette("FantasticFox1", 3, type = c("continuous"))
# x <- wes_palette("Moonrise2", n = 4, type = c("continuous"))
# x2 <- c(x[2], x[1])
#
# ggplot(samps_hist, aes(length_ext_cm, group = as.factor(year_group), fill = as.factor(year_group)),
#        colour = as.factor(year_group)
# ) +
#   geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
#   geom_vline(
#     data = samps_hist,
#     aes(xintercept = mature)
#   ) +
#   facet_grid(
#     rows = vars(fmp), cols = vars(sex), scales = "free",
#     labeller = ggplot2::labeller(sex = sex.labs)
#   ) +
#   scale_x_continuous(
#     breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
#     limits = c(0, 125), name = "Length (cm)"
#   ) +
#   scale_y_continuous(breaks = c(0, 0.03, 0.06), labels = c(0, 0.03, 0.06), name = "Density") +
#   scale_fill_manual("Years", values = x2, labels = c("'11-'15", "'16-'21")) +
#   scale_colour_manual(values = x2) +
#   theme(
#     plot.background = element_rect(fill = "NA", colour = "NA"),
#     # text = element_text(family= "Gill Sans MT"),
#     axis.line.x = element_line(colour = "grey60"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     strip.background = element_blank(),
#     plot.margin = margin(1, 0, 1, 0.5, "cm"),
#     panel.background = element_rect(fill = "white", colour = "grey60"),
#     axis.text.x = element_text(size = 10, vjust = 1, colour = "grey20"),
#     axis.text.y = element_text(size = 10, colour = c("grey20")),
#     axis.title.x = element_text(size = 10, colour = "grey20"),
#     axis.title.y = element_text(size = 10, colour = "grey20"),
#     axis.ticks.length = unit(0.15, "cm"),
#     strip.text = element_text(size = 10),
#     axis.ticks.x = element_line(colour = "grey60"),
#     axis.ticks.y = element_line(colour = "grey60")
#   )
#
# ggsave("Figures/length_summary_iphc.png", width = 6, height = 4)



# Density figure of trawl, regions combined -------------------------------

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

m <- readRDS("output/survey_samples_codedmaturity.rds")
fm <- m$pred_data |>
  mutate(min2 = abs(m$pred_data$glmm_fe - 0.95)) |>
  filter(female == 1) |>
  slice_min(min2, n = 1, with_ties = FALSE)
fi <- m$pred_data |>
  mutate(min2 = abs(m$pred_data$glmm_fe - 0.05)) |>
  filter(female == 1) |>
  slice_min(min2, n = 1, with_ties = FALSE)
mi <- m$pred_data |>
  mutate(min2 = abs(m$pred_data$glmm_fe - 0.95)) |>
  filter(female == 0) |>
  slice_min(min2, n = 1, with_ties = FALSE)
mi$age_or_length
mm <- m$pred_data |>
  mutate(min2 = abs(m$pred_data$glmm_fe - 0.05)) |>
  filter(female == 0) |>
  slice_min(min2, n = 1, with_ties = FALSE)
mm$age_or_length

samps_hist$mature <- ifelse(samps_hist$sex == 2, as.numeric(fm$age_or_length), as.numeric(mm$age_or_length)) # see 999-split-index_by-region-maturity.R for maturity ogive
samps_hist$immature <- ifelse(samps_hist$sex == 2, as.numeric(fi$age_or_length), as.numeric(mi$age_or_length))


x <- PNWColors::pnw_palette("Cascades", 3)
# samps_hist$survey_group <- factor(samps_hist$survey_group,
#   levels = c("trawl", "iphc")
# )

line_dat_mature <- dplyr::select(samps_hist, mature) |>
  distinct()
line_dat_immature <- dplyr::select(samps_hist, immature) |>
  distinct() |>
  rename(mature = immature)

line_dat <- data.frame(
  sex = c(1, 1, 2, 2),
  mature = c(
    as.numeric(line_dat_immature[1, ]),
    as.numeric(line_dat_mature[1, ]),
    as.numeric(line_dat_immature[2, ]),
    as.numeric(line_dat_mature[2, ])
  ),
  maturepos = c(
    as.numeric(line_dat_immature[1, ]),
    as.numeric(line_dat_mature[1, ]),
    as.numeric(line_dat_immature[2, ]),
    as.numeric(line_dat_mature[2, ])
  ),
  survey_name2 = "GOA",
  label = c(
    "0.95\nprob.of\nmaturity", "0.05\nprob.of\nmaturity",
    "0.05\nprob.of\nmaturity", "0.95\nprob.of\nmaturity"
  )
)

data_text <- data.frame(
  label = c("Males", "Females"), # Create data for text
  sex = names(table(samps_hist$sex)),
  survey_name2 = "GOA",
  x = c(18, 18),
  y = c(0.055, 0.055)
)

# data_textm <- data.frame(label = c("malesimm",  "malesm", 'femalesimm', 'femalesm'),  # Create data for text
#                         sex = c(1,1,2,2),
#                         survey_name2 = "GOA",
#                         x = c(64.9,76.7,64.9, 95.5 ),
#                         y = c(0.06, 0.06, 0.06, 0.06))


ghist <-
  ggplot(samps_hist, aes(length_ext_cm, group = as.factor(survey_name2), fill = as.factor(survey_name2)),
    colour = as.factor(survey_name2)
  ) +
  geom_density(alpha = 0.6, linewidth = 0.25) + # , bins = 50) +
  geom_vline(
    data = line_dat,
    aes(xintercept = mature), colour = "grey40"
  ) +
  facet_wrap(
    ~sex
  ) +
  # theme(plot.margin = unit(c(5, 1, 1, 1), "lines")) +
  coord_cartesian(expand = FALSE, ylim = c(0, 0.06), xlim = c(0, 126)) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)",
  ) +
  scale_y_continuous(
    breaks = c(0, 0.02, 0.04, 0.06),
    labels = c(0, 0.02, 0.04, 0.06), name = "Density"
    # expand = expansion(mult = c(0, 0.03))
  ) +
  scale_fill_manual("Region", values = cols_region3) +
  scale_colour_manual(values = cols_region3) +
  theme(
    legend.position.inside = c(0.13, 0.67),
    legend.position = "inside",
    strip.text.x = element_blank()
  )

ghist


# # maturity ogive ----------------------------------------------------------
# # pull output from 999-split-index-by-region-maturity.R
# m <- readRDS("output/survey_samples_codedmaturity.rds")
# raw <- m$data
# m$data <- m$data |> mutate(mature_num = ifelse(mature == "FALSE", 0.5, 0.5))
#
# # blue <- RColorBrewer::brewer.pal(3, "Blues")[3]
# # red <- RColorBrewer::brewer.pal(3, "Reds")[3]
#
# grey <- "grey60"
# black <- "black"
#
# source("analysis/999-plot-mat-ogives.R")
# g_mat <- plot_mat_ogive(m, col = c("M" = grey, "F" = black)) +
#   guides(colour = "none", linetype = "none")
#
# # nd_re <- m$pred_data
# # n_re <- length(unique(nd_re$sample_id)) / 5
# # n_re2 <- ifelse(n_re < 15, 15, n_re)
#
# # tally by length group and calculate weight of each group
# fm <- m$pred_data |>
#   mutate(min2 = abs(m$pred_data$glmm_fe - 0.95)) |>
#   filter(female == 1) |>
#   slice_min(min2, n = 1, with_ties = FALSE)
# fi <- m$pred_data |>
#   mutate(min2 = abs(m$pred_data$glmm_fe - 0.05)) |>
#   filter(female == 1) |>
#   slice_min(min2, n = 1, with_ties = FALSE)
# mi <- m$pred_data |>
#   mutate(min2 = abs(m$pred_data$glmm_fe - 0.95)) |>
#   filter(female == 0) |>
#   slice_min(min2, n = 1, with_ties = FALSE)
# mi$age_or_length
# mm <- m$pred_data |>
#   mutate(min2 = abs(m$pred_data$glmm_fe - 0.05)) |>
#   filter(female == 0) |>
#   slice_min(min2, n = 1, with_ties = FALSE)
# mm$age_or_length
#
# m$pred_data <- m$pred_data |>
#   mutate(cat = ifelse(female == 1 & age_or_length >= as.numeric(fm$age_or_length), 1,
#     ifelse(female == 0 & age_or_length >= as.numeric(mm$age_or_length), 2,
#       ifelse(female == 1 & age_or_length <= as.numeric(fi$age_or_length), 3,
#         ifelse(female == 0 & age_or_length <= as.numeric(mi$age_or_length), 4,
#           NA
#         )
#       )
#     )
#   ))
#
# m$pred_data <- m$pred_data |>
#   group_by(cat) |>
#   mutate(matlength = ifelse(cat %in% c(1, 2), min(age_or_length),
#     ifelse(cat %in% c(3, 4), max(age_or_length),
#       ifelse(is.na(cat == TRUE), NA, NA)
#     )
#   ))
#
# # data <- m$pred_data |>
# #   group_by(female, cat) |>
# #   summarize(unique = unique(matlength))
# # data
# # ann_text <- data.frame(
# #   age_or_length = c(40, 40, 40, 40), glmm_re = c(0.75, 0.68, 0.60, 0.53),
# #   lab = c("F05 = 77.0", "F95 = 95.6", "M05 = 65.1", "M95 = 76.7"),
# #   female = factor(c("1", "1", "0", "0"), levels = c("0", "1"))
# # )
# #
# # string <- data.frame(
# #   length = c(
# #     as.numeric(fm$age_or_length), as.numeric(fi$age_or_length) + 1,
# #     as.numeric(mi$age_or_length), as.numeric(mm$age_or_length - 1)
# #   ),
# #   female = c(1, 1, 0, 0)
# # )
#
#

# maturity plot - f and m seperate ----------------------------------------
#<- LD added
#pull output from 999-split-index-by-region-maturity.R
m <- readRDS("output/survey_samples_codedmaturity.rds")
raw <- m$data
m$data <- m$data |> mutate(mature_num = ifelse(mature == "FALSE", 0.5, 0.5))
#m$pred_data <- filter(m$pred_data, female == 1)  #<- plot females only
nd_re <- m$pred_data
n_re <- length(unique(nd_re$sample_id)) / 5
n_re2 <- ifelse(n_re < 15, 15, n_re)

ann_text <- data.frame(
  age_or_length = c(40, 40, 40, 40), glmm_re = c(0.75, 0.68, 0.75, 0.68),
  lab = c("F05 = 77.0", "F95 = 95.6", "M05 = 65.1", "M95 = 76.7"),
  female = factor(c("1", "1", "0", "0"), levels = c("0", "1"))
)

string <- data.frame(
  length = c(
    as.numeric(fm$age_or_length), as.numeric(fi$age_or_length) + 1,
    as.numeric(mi$age_or_length), as.numeric(mm$age_or_length - 1)
  ),
  female = c(1, 1, 0, 0)
)

p <-
  ggplot() +
  geom_line(
    data = nd_re, aes(age_or_length,
      as.numeric(glmm_re),
      group = paste(sample_id, female),
      #fill = as.character(female),
      colour = as.character(female)
      #linetype = as.character(female)
    ),
    #colour = c("red", "grey"),
    inherit.aes = FALSE,
    alpha = 1 / n_re2,
    show.legend = FALSE
  ) +
  # scale_linetype_manual(values = c("dotdash", "solid")) +
  scale_colour_manual(values = c("grey10", "grey50")) +
  scale_x_continuous(limits = c(0,125)) +
  geom_rug(
    data = filter(m$data, mature == "FALSE"), aes(
      x = age_or_length,
      y = mature_num
      #colour = "female"
      #linetype = as.character(female)
    ),
    #colour = c("red", "grey"),
    sides = "b",
    length = unit(c(0.04), "npc"),
    alpha = c(0.05),
    lty = 1,
    show.legend = FALSE
  ) +
  geom_rug(
    data = filter(m$data, mature == "TRUE"), aes(
      x = age_or_length,
      y =mature_num
      #colour = "female"
      #linetype = as.character(female)
    ),
    #colour = c("red", "grey"),
    sides = "t",
    alpha = 0.05,
    # lty = 1,
    show.legend = FALSE
  ) +
  labs(x = "Length (cm)", y = "Probability mature") +
  theme(plot.margin = unit(c(5, 1, 1, 1), "lines")) +
  geom_vline(
    data = string,
    aes(
      xintercept = length,
      #colour = female
      #linetype = as.character(female)
    ),
    colour = c("grey10", "grey10", "grey50", "grey50"),
    show.legend = FALSE,
  ) +
   facet_wrap(~female, nrow = 1) +
  theme(strip.text.x = element_blank())

p <- p + geom_text(data = ann_text, aes(age_or_length, glmm_re, label = lab),
                   colour = c("grey10", "grey10", "grey10", "grey10"),
                   show.legend = FALSE)
p
g_mat <- p

#ggsave("figs/maturityogive_facet.png", width = 4, height = 3)

# library(patchwork)
# layout <- "
# ABB
# "

layout <- "
AA
BB
"

th <- theme(tagger.panel.tag.text = element_text(color = "grey30", size = 10))

g_mat + tagger::tag_facets(tag = "panel",
                           tag_prefix = "", position = "tl", tag_suffix = "",
                           #tag_pool = c("(a) Maturity ogives")
                           tag_pool = c("(a) Males", "(b) Females")
) + th +
  ghist +tagger::tag_facets(tag = "panel",
                            tag_prefix = "", tag_suffix = "", position = "tl",
                            tag_pool = c("(c) Males", "(d) Females")
  ) + th +
  plot_layout(design = layout)

ggsave("figs/maturity-ogives-new.png", width = 6, height = 6)
ggsave("figs/maturity-ogives-new.pdf", width = 6, height = 6)

# # cowplot both figures ----------------------------------------------------
#
# # p
# # hist
#
# # cowplot::plot_grid(g_mat, ghist,
# #   labels = c("(a) ", " (b)"),
# #   # rel_heights = c(5, 1),
# #   label_x = 0, label_y = 0.85,
# #   align = "v",
# #   axis =
# #   nrow = 1,
# #   ncol = 2,
# #   label_size = 12,
# #   rel_widths = c(1, 2),
# #   label_fontfamily = "sans"
# # )
# #
#
# library(patchwork)
# layout <- "
# ABB
# "
# th <- theme(tagger.panel.tag.text = element_text(color = "grey30", size = 10))
#
# g_mat + tagger::tag_facets(tag = "panel",
#   tag_prefix = "", position = "tl", tag_suffix = "", tag_pool = c("(a) Maturity ogives")
# ) + th +
#   ghist +tagger::tag_facets(tag = "panel",
#     tag_prefix = "", tag_suffix = "", position = "tl", tag_pool = c("(b) Males", "(c) Females")
#   ) + th +
#   plot_layout(design = layout)
#
# ggsave("figs/maturity-ogives.png", width = 9, height = 2.7)
# ggsave("figs/maturity-ogives.pdf", width = 9, height = 2.7)
