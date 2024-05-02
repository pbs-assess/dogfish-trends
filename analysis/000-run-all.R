if (FALSE) {
  remotes::install_github("pbs-assess/sdmTMB")
  remotes::install_github("seananderson/ggsidekick")
  remotes::install_github("ropensci/rnaturalearthhires")
  remotes::install_github("eliocamp/tagger")
}

dir.create("output", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)

# main coast-wide index models:
source("analysis/010-overall-trawl-model.R")
source("analysis/010-overall-longline-model.R")
source("analysis/011-plot-overall-trends.R")
source("analysis/012-plot-coefs.R")

# biomass-weighted depth:
source("analysis/013-calculate-weighted-mean-depth.R")
source("analysis/014-plot-mean-depth.R")

# SVC trends:
source("analysis/020-svc-models.R")
source("analysis/021-plot-svc-models.R")
source("analysis/021-plot-svc-models2.R") # has intercept too

# maturity-stage based:
source("analysis/030-maturity-models.R")
source("analysis/031-plot-maturity-trends.R")
