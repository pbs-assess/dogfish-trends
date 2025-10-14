dir.create("output", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)

if (FALSE) {
  remotes::install_github("pbs-assess/gfiphc")
  remotes::install_github("pfmc-assessments/nwfscSurvey")
}

source("data-prep/01-load-trawl-data.R")
source("data-prep/02-wrangle-IPHC-data.R")
source("data-prep/02a-wrangle-sets-trawl-data.R")
source("data-prep/02b-wrangle-sample-trawl-data.R")
source("data-prep/03-exploratory-plots-iphc-sets.R")
## source("data-prep/04-SOM-figures-raw-survey-data.R")
source("data-prep/05-split-index-by-region-maturity.R")
## source("data-prep/06-load-cpue-data.R")
## source("data-prep/07-wrange-cpue-data.R")
## source("data-prep/08-exploratory-plots-cpue.R")
source("data-prep/09-IPHC-grid-creation.R")
source("data-prep/09-trawl-grid-creation.R")
