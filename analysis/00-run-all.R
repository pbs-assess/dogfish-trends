dir.create("data-generated", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)

source("analysis/01-prep-overall-trawl.R")
source("analysis/02-overall-trawl-model.R")
# source("analysis/02-overall-longline-model.R")
source("analysis/03-fig1.R")
# source("analysis/03-svc-models.R")
source("analysis/04-maturity-models.R")
