# Plot catchability coefs



# plot coef for coastwide catachability coef ------------------------------
#pulled these from 010-overall-trawl-model.R
fitq <- readRDS(paste0("output/fit-trawl-coast-lognormal-mix-poisson-link-q-", min_edge, "-", max_edge, ".rds"))
# fit1 <- readRDS(paste0("output/fit-trawl-coast-lognormal-mix-",min_edge,"-", max_edge,".rds"))
fit4 <- readRDS(paste0("output/fit-trawl-coast-lognormal-mix-poisson-link-", min_edge, "-", max_edge, ".rds")) # this is the one that was used for the index generation. delta lognormal mix with a poisson link

#m1_df <- broom::tidy(fit4)
m1_df <- tidy(fit4, conf.int = TRUE, model = 1) #no surveys
m1_df

m2_df <- tidy(fitq, conf.int = TRUE, model = 1)
m2_df <- m2_df[c(1, 4:6), ]
m2_df$model <- "model1"

m2_df2 <- tidy(fitq, conf.int = TRUE, model = 2)
m2_df2 <- m2_df2[c(1, 4:6), ]
m2_df2$model <- "model2"
m2 <- bind_rows(m2_df, m2_df2)
m2

m2 <- bind_rows(m2_df, m2_df2)
m2 <- m2 |>
  mutate(term2 = c("Intercept", "GOA", "WCGBT pass 1", "WCGBT pass 2",
                   "Intercept", "GOA", "WCGBT pass 1", "WCGBT pass 2"))

m2 <- m2 %>%
  mutate(term2 = factor(term2, levels = c("WCGBT pass 2", "WCGBT pass 1", "GOA", "Intercept")))
ggplot(m2, aes(estimate,term2)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  facet_wrap(~model) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  xlab("Estimate") + ylab("Coastwide")

ggsave("figs/coastwide-catchability-coeff-plot.png", width = 5, height = 3)



# plot coeff for NWFSC catchability model ---------------------------------
#from 012-individual-trawl-models.R
# plot out different analyses for the longest time series possible

out <- readRDS(file = "output/fit-trawl-by-region-lognormal-poisson-link-w-catchabilities.rds")
out$fit

m1_df <- tidy(out$fit, conf.int = TRUE, model = 1)
m1_df$model <- "model1"
m1_df <- m1_df[c(1:7), ]
m2_df <- tidy(out$fit, conf.int = TRUE, model = 2)
m2_df$model <- "model2"
m2_df <- m2_df[c(1:7), ]

m <- bind_rows(m1_df, m2_df)
#dotwhisker::dwplot(m1_df)
#dotwhisker::dwplot(m2_df)

m <- m |>
  mutate(term2 = c("WCGBT pass 1", "WCGBT pass 2", "AFSC slope", "AFSC slope early", "NWFSC slope", "Triennial", "Triennial early", "WCGBT pass 1", "WCGBT pass 2", "AFSC slope", "AFSC slope early", "NWFSC slope", "Triennial", "Triennial early"))

ggplot(m, aes(estimate,term2)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  facet_wrap(~model) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  xlab("Estimate") + ylab("US West Coast survey")

ggsave("figs/nwfsc-catchability-coeff-plot.png", width = 5, height = 3)

