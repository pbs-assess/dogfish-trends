
library(tidyverse)
library(readxl)
library(gfplot)
library(here)

#get the age at length
#this is to see what the age at length is
#use this for stock assessment.
#Use this for values on histograms

#from 01-wrangle-US-data.R
samps <- readRDS("output/data_survey_samples.rds")



# NW US data --------------------------------------------------------------
#data from NW US survey in 2010
#see paper Taylor et al Spine based ageing
datnwus <- readxl::read_excel("data-raw/nwfscAGES/Dogfish_2021_age_data_foranalysis.xlsx")|>
  dplyr::select(SampleYear, Sex, NaturalLength, Age_Ketchen, Age_Exponential) |>
  drop_na(Age_Exponential, Age_Ketchen, NaturalLength,SampleYear, Sex) |>
  filter(Sex %in% c(1,2)) |>
  mutate(length = 1.02 * NaturalLength, # convert to TL extended (see Tribuzio and Gerseva for refs)
         sex = ifelse(Sex == 1, "Male", "Female")) |>
  rename(Age = Age_Exponential)


ggplot(datnwus, aes(Age, length)) + facet_wrap(~sex, scales = "free") + geom_smooth() + geom_point()
ggplot(datnwus, aes(Age_Ketchen, length)) + facet_wrap(~sex, scales = "free") + geom_smooth() + geom_point()

dat <- readxl::read_excel("C:/Dogfish_OMS_2023/dogfish-assess/data/raw/Age.Length.Data.xlsx") %>%
  filter(!is.na(Age),
         sex != 7,
         !is.na(length)) %>%
  mutate(length = 0.1 * length, # convert to cm
         sex = ifelse(sex == 1, "Male", "Female"))

ggplot(dat, aes(Age, length)) + facet_wrap(~sex) + geom_point()

# fit vb to NW us data ----------------------------------------------------
vb_f <-
  dat %>%
  filter(sex == "Female") %>%
  #dplyr::select(-c(Sex, sex)) |>
  mutate(
    specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "female",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

vb_m <-
  dat %>%
  filter(sex == "Male") %>%
  dplyr::select(-Sex)|>
  mutate(sex = 1,
         specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "male",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

f <-vb_f$predictions #these seem off, length 40 age of 0?
write.csv(f, "output/vb_f_predictions.csv")
m <-vb_m$predictions

fg <- gfplot::plot_growth(object_f = vb_f,
                          object_m = vb_m,
                          lab_x = 0.6,
                          lab_x_gap = 0.20,
                          pt_alpha = 0.8,
                          col = c(Female = "black", Male = "black"),
                          french = FALSE,
                          jitter = TRUE) +
  facet_wrap(vars(sex)) +
  guides(col = "none", lty = "none")
fg

#### Plot data
# By Area
g <- ggplot(dat, aes(Age, length, colour = sex)) +
  geom_point(alpha = 0.5) +
  gfplot::theme_pbs() +
  expand_limits(y = 0) +
  labs(x = "Age", y = "Length (cm)", colour = "Sex")
g

#calculate proportion mature by each lengths
m <- gfplot::fit_mat_ogive(samps,
                           type = "length",
                           sample_id_re = TRUE,
                           custom_maturity_at = c(NA, 55))
gfplot::plot_mat_ogive(m)
x <- m$pred_data
unique(x$female)
saveRDS(x, "output/matogive_fit.rds")
x <- readRDS("output/matogive_fit.rds")

#merge ages and length
glimpse(x)
unique(x$female)
x$length <- round(x$age_or_length, 1)
x$sex <- ifelse(x$female == 0, "Male", "Female")
unique(x$sex)
glimpse(x)

glimpse(dat)
dat$length <- round(dat$length, 1)
dat_mean <- dat |>
  group_by(sex, length) |>
  summarize(Age_Exponential_max = max(Age), Age_Ketchen_max = max(Age_Ketchen))

xx <- x |>
  left_join(dat_mean)

xx <- x |>
  left_join(dat_mean, by = join_by(sex == sex,
                                   closest(length <= length)))

#x <- m$pred_data
imm <- filter(xx, female == 1 & glmm_fe <= 0.05)
range(imm$age_or_length, na.rm = TRUE)
range(imm$Age_Exponential_max, na.rm = TRUE)
range(imm$Age_Ketchen_max, na.rm = TRUE)

mature<- filter(xx, female == 1 & glmm_fe >= 0.95)
range(mature$age_or_length, na.rm = TRUE)
range(mature$Age_Exponential_max, na.rm = TRUE)
range(mature$Age_Ketchen_max, na.rm = TRUE)

imm <- filter(xx, female == 0 & glmm_fe <= 0.05)
range(imm$age_or_length, na.rm = TRUE)
range(imm$Age_Exponential_max, na.rm = TRUE)
range(imm$Age_Ketchen_max, na.rm = TRUE)

mm <- filter(xx, female == 0 & glmm_fe >= 0.95)
range(mm$age_or_length, na.rm = TRUE)
range(mm$Age_Exponential_max, na.rm = TRUE)
range(mm$Age_Ketchen_max, na.rm = TRUE)


#### Plot residual by area
glimpse(xx)
ggplot(xx, aes(length.y, age, group = sex, colour = sex)) +
  geom_point()

# BC data -----------------------------------------------------------------
#from BC, however I found the range of lengths to be quite short
#smallest size is 40 cm and after I estimate ages that legnth is given a age 0
dat <- readxl::read_excel("C:/Dogfish_OMS_2023_old/dogfish-assess/data/raw/Age.Length.Data.xlsx") %>%
  filter(!is.na(Age),
         sex != 7,
         !is.na(length)) %>%
  mutate(length = 0.1 * length, # convert to cm
         sex = ifelse(sex == 1, "Male", "Female"))

ggplot(dat, aes(Age, length)) + facet_wrap(~sex) + geom_point()

#### Plot data
# By Area
g <- ggplot(dat, aes(Age, length, colour = sex)) +
  geom_point(alpha = 0.5) +
  gfplot::theme_pbs() +
  #facet_wrap(vars(Area)) +
  expand_limits(y = 0) +
  labs(x = "Age", y = "Length (cm)", colour = "Sex")
g
#ggsave("figs/length-age-area.png", g, height = 4, width = 6)

# By Year
g <- ggplot(dat, aes(Age, length, colour = sex)) +
  geom_point(alpha = 0.5) +
  gfplot::theme_pbs() +
  facet_wrap(vars(`Sample year`)) +
  expand_limits(y = 0) +
  labs(x = "Age", y = "Length (cm)", colour = "Sex")
g
#ggsave("figs/length-age-year.png", g, height = 4, width = 6)

# Samples by area and year
dat %>%
  group_by(`Sample year`, Area) %>%
  summarise(n = n()) %>%
  reshape2::acast(list("`Sample year`", "Area"), fill = 0)

#### Fit growth model
#do this for BC data
#model already fit for the NW data
vb_f <- dat %>%
  filter(sex == "Female") %>%
  mutate(sex = 2,
         specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "female",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

vb_m <- dat %>%
  filter(sex == "Male") %>%
  mutate(sex = 1,
         specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "male",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

f <-vb_f$predictions #these seem off, length 40 age of 0?
write.csv(f, "output/vb_f_predictions.csv")
m <-vb_m$predictions

fg <- gfplot::plot_growth(object_f = vb_f,
                          object_m = vb_m,
                          lab_x = 0.6,
                          lab_x_gap = 0.20,
                          pt_alpha = 0.8,
                          col = c(Female = "black", Male = "black"),
                          french = FALSE,
                          jitter = TRUE) +
  facet_wrap(vars(sex)) +
  guides(col = "none", lty = "none")
fg
#ggsave("figs/length-age-vb.png", g, height = 3, width = 6)


#calculate proportion mature by each lengths
m <- gfplot::fit_mat_ogive(samps,
                           type = "length",
                           sample_id_re = TRUE,
                           custom_maturity_at = c(NA, 55))
gfplot::plot_mat_ogive(m)
x <- m$pred_data
write.csv(x, "output/matogive_fit.csv")

#merge ages and length
#f and x
glimpse(x)
xx <- filter(x, female == 1)
xx$age_or_length <- round(xx$age_or_length, 0)
glimpse(xx)

glimpse(f)
f$length <- round(f$length, 0)

merge <- left_join(xx, f, by = c("age_or_length" = "length"))

x <- m$pred_data
x1<- filter(x, female == 1 & glmm_fe > 0.94)
range(x1$age_or_length)
xx <- filter(x, female == 1 & glmm_fe < 0.05)
range(xx$age_or_length)
range(xx$age_or_length)


#### Plot residual by area
dat <- dat %>%
  mutate(linf = ifelse(sex == "Female", 97.4, 83.7),
         k = ifelse(sex == "Female", 0.05, 0.08),
         t0 = ifelse(sex == "Female", -10.63, -11.07),
         sigma = ifelse(sex == "Female", exp(-1.693), exp(-1.914)),
         pred = linf * (1 - exp(-k * (Age - t0))) * exp(-0.5 * sigma * sigma),
         resid = log(length/pred))
g <- dat %>%
  filter(resid > -1) %>%
  ggplot(aes(Age, resid, colour = sex)) +
  geom_point(alpha = 0.5) +
  gfplot::theme_pbs() +
  facet_wrap(vars(Area)) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Age", y = "Residual", colour = "Sex")
g
ggsave("figs/length-age-residual-area.png", g, height = 4, width = 6)

