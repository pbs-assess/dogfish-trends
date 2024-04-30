dir.create("data-generated", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)

# main coast-wide index models:
source("analysis/010-overall-trawl-model.R")
source("analysis/010-overall-longline-model.R")
source("analysis/011-fig1.R")
source("analysis/012-weighted-mean-depth.R")

# overall SVC:
source("analysis/020-svc-models.R")
source("analysis/021-plot-svc-models.R")

# maturity-stage based:
source("analysis/030-maturity-models.R")
source("analysis/031-plot-maturity-models.R")
source("analysis/031-plot-maturity-svc.R")
