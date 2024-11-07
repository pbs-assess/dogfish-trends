# Plot catchability coefs



# plot coef for coastwide catachability coef ------------------------------
#pulled these from 010-overall-trawl-model.R
fitq <- readRDS(paste0("output/fit-trawl-coast-lognormal-mix-poisson-link-q-", min_edge, "-", max_edge, ".rds"))
# fit1 <- readRDS(paste0("output/fit-trawl-coast-lognormal-mix-",min_edge,"-", max_edge,".rds"))
fit4 <- readRDS(paste0("output/fit-trawl-coast-lognormal-mix-poisson-link-", min_edge, "-", max_edge, ".rds")) # this is the one that was used for the index generation. delta lognormal mix with a poisson link

m1_df <- broom::tidy(fit4)
m1_df
m2_df <- broom::tidy(fitq)
m2_df
m2_df <- m2_df[c(1, 4:6), ]

m2_df <- m2_df |>
  mutate(term = c("BC", "GOA", "WCGBT pass 1", "WCGBT pass 2"))

dotwhisker::dwplot(m2_df,
  dot_args = list(size = 2, pch = 21, fill = "red"),
  vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
) +
  xlab("Coefficient") +
  theme_bw() + xlab("Coefficient") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(
    legend.position = "none"
  )

ggsave("figs/coastwide-catcability-coeff-plot.png", width = 5, height = 5)



# plot coeff for NWFSC catchability model ---------------------------------
#from 012-individual-trawl-models.R
# plot out different analyses for the longest time series possible

out <- readRDS(file = "output/fit-trawl-by-region-lognormal-poisson-link-w-catchabilities.rds")
out$fit

m1_df <- broom::tidy(out$fit)
m1_df$model <- "model1"
m1_df <- m1_df[c(1:7), ]
m2_df <- broom::tidy(out$fit, model = 2)
m2_df$model <- "model2"
m2_df <- m2_df[c(1:7), ]

m <- bind_rows(m1_df, m2_df)
dotwhisker::dwplot(m1_df)
dotwhisker::dwplot(m2_df)
#dotwhisker::dwplot(m2_df, facet_wrap(~model))

m <- m |>
  mutate(term2 = c("WCGBT pass 1", "WCGBT pass 2", "AFSC slope", "AFSC slope early", "NWFSC slope", "Triennial", "Triennial early", "WCGBT pass 1", "WCGBT pass 2", "AFSC slope", "AFSC slope early", "NWFSC slope", "Triennial", "Triennial early")) |>
  mutate(upr = estimate  + std.error, lwr = estimate  - std.error)

ggplot(m, aes(estimate,term2), colour = "red") +
  geom_point() +
  geom_linerange(aes(xmin = lwr, xmax = upr)) +
  facet_wrap(~model) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  xlab("Estimate") + ylab("US West Coast survey")

ggsave("figs/nwfsc-catcability-coeff-plot.png", width = 5, height = 5)

