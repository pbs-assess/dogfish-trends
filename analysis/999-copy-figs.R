optipng <- function() {
  wd <- getwd()
  on.exit(setwd(wd))
  if (!gfplot:::is_windows()) {
    setwd("figs")
    files_per_core <- 2
    cores <- 10
    system(paste0(
      "find -X . -name '*.png' -print0 | xargs -0 -n ",
      files_per_core, " -P ", cores, " optipng -strip all"
    ))
  }
}
fcopy <- function(x) {
  x <- gsub("figs\\/", "", x)
  file.copy(paste0("figs/", x), paste0("../dogfish-trends-ms/figs/", x), overwrite = TRUE)
}

optipng()
f <- c(
  "figs/overall-survey-trends2.pdf",
  "figs/overall-survey-trends2.png",
  "figs/svc-trawl-stacked-start.png",
  "figs/qq-trawl-main.png",
  "figs/index-trawl-main-by-region-catchability-effects.pdf",
  "figs/qq-iphc-main.png",
  "figs/biomass-weighted-depth.pdf",
  "figs/biomass-weighted-depth.png",
  "figs/maturity-index-trends-combo.pdf",
  "figs/maturity-index-trends-combo.png",
  "figs/coefs.pdf",
  "figs/depth-effects.pdf",
  "figs/trawl-model-mesh.png",
  "figs/iphc-model-mesh.png"
)

sapply(f, fcopy)

file.copy("output/values.tex", "../dogfish-trends-ms/figs/", overwrite = TRUE)
