library(ggplot2)
library(dplyr)

predictions_svc <- readRDS("output/predictions-trawl-svc-by-maturity-poisson-link.rds")
fits_svc <- readRDS("output/fit-trawl-svc-by-maturity-poisson-link.rds")

g <- lapply(seq_along(predictions_svc), \(x) {
  predictions_svc[[x]] |>
    ggplot(aes(X, Y, fill = exp(svc))) +
    geom_raster() +
    scale_fill_gradient2(trans = "log10") +
    labs(fill = "Spatially\nvarying\ntrend\n\n(Proportion change\nby decade)") +
    coord_equal() +
    ggtitle(names(fits_svc)[x])

})

cowplot::plot_grid(plotlist = g)
