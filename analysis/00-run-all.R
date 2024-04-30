dir.create("data-generated", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)

source("analysis/01-prep-overall-trawl.R")
source("analysis/02-overall-trawl-model.R")
# source("analysis/02-overall-longline-model.R")
source("analysis/03-fig1.R")
# source("analysis/03-svc-models.R")
source("analysis/04-maturity-models.R")
source("analysis/05-plot-maturity-models.R")
source("analysis/05-plot-maturity-svc.R")
source("analysis/06-fit-by-depth.R")

source("analysis/07-weighted-mean-depth.R")
