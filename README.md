# Dogfish Trends: Transboundary Population Analyses

This repository contains code and data for:

Davidson, L.N.K., P.A. English, J.R. King, P. Grant, I.G. Taylor, L. Barnett, V. Gertseva, C.A. Tribuzio, and S.C. Anderson. 2025. Mystery of the disappearing dogfish: transboundary analyses reveal steep population declines across the Northeast Pacific with little evidence for regional redistribution. *Fish and Fisheries*. <https://doi.org/10.1111/faf.70028>

## Repository Structure

- `data-raw/`: Raw data files from trawl and longline surveys
- `data-prep/`: Scripts for data wrangling and preparation
- `analysis/`: Statistical modeling and analysis scripts
- `output/`: Model outputs and processed results
- `figs/`: Figures generated for the publication

## Requirements

The analysis requires R and the following packages (other packages on CRAN):

- [sdmTMB](https://github.com/pbs-assess/sdmTMB)
- [gfiphc](https://github.com/pbs-assess/gfiphc)
- [nwfscSurvey](https://github.com/pfmc-assessments/nwfscSurvey)
- [ggsidekick](https://github.com/seananderson/ggsidekick)
- [rnaturalearthhires](https://github.com/ropensci/rnaturalearthhires)
- [tagger](https://github.com/eliocamp/tagger)

Install required packages with:

```R
install.packages("pak")
install.packages("tidyverse")
install.packages("sf")
install.packages("data.table")
install.packages("patchwork")
install.packages("marmap")
install.packages("sdmTMB", dependencies = TRUE)
install.packages("wesanderson")
install.packages("inlabru")
install.packages("RColorBrewer")
install.packages("RhpcBLASctl")
install.packages("concaveman")
install.packages("spatioEco")
install.packages("cowplot")
pak::pak("pbs-assess/gfiphc")
pak::pak("pfmc-assessments/nwfscSurvey")
pak::pak("seananderson/ggsidekick")
pak::pak("ropensci/rnaturalearthhires")
pak::pak("eliocamp/tagger")
```

## Running the Analysis

The analysis can be reproduced by running scripts in sequence:

1. **Data preparation**: Run `data-prep/00-run-all.R` to prepare all survey data
2. **Statistical analysis**: Run `analysis/000-run-all.R` to fit models and generate figures

Alternatively, run individual scripts in numerical order within each directory.

## Contact

For questions about this code, please open an issue or contact the corresponding author.
