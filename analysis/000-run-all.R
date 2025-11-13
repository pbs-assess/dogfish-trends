dir.create("output", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)

# main coast-wide index models:
source("analysis/010-overall-trawl-model.R")
source("analysis/010-overall-longline-model.R")
source("analysis/012-individual-trawl-models.R")
source("analysis/013-plot-overall-trends.R")
source("analysis/014-plot-coefs.R")
source("analysis/014a-plot-catchability-coefs.R")

source("analysis/015-maturity-models.R")
source("analysis/016-calculate-weighted-mean-depth.R")
source("analysis/017-plot-mean-depth.R")

# SVC trends:
source("analysis/020-svc-models.R")
source("analysis/021-plot-svc-models2.R") # this is absolute declines
source("analysis/021-plot-svc-models.R") # this is proportional declines.

source("analysis/031-plot-maturity-trends.R")
source("analysis/040-model-table.R")
source("analysis/050-plot-maturity-hists-ogive.R")

# values for paper:
source("analysis/080-values.R")
