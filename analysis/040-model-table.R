fit_trawl_lognormal_mix_pl <- readRDS("output/fit-trawl-coast-lognormal-mix-poisson-link.rds")

f <- fit_trawl_lognormal_mix_pl

f$priors$matern_s[1]
f$priors$matern_s[2]

f$priors$matern_st[1]
f$priors$matern_st[2]

fit_iphc_nb2 <- readRDS("output/fit-iphc-nb2-coastwide.rds")

fit_svc_all <- readRDS("output/fit-trawl-svc-lognormal-mix.rds")

# WHICH!??
fits_svc_mat <- readRDS("output/fit-trawl-svc-maturity.rds")
# fits_svc_mat <- readRDS("output/fit-trawl-svc-by-maturity-poisson-link.rds")

fits_index_mat <- readRDS("output/fit-trawl-by-maturity-poisson-link.rds")

get_stuff <- function(x, tag, tag2 = "") {
  fam <- sdmTMB:::print_model_info(x)$overall_family
  fam <- gsub("Family: ", "", fam)
  fam <- gsub("[\r\n]", "", fam)

  out <- data.frame(
    spatiotemporal = paste(x$spatiotemporal, collapse = ", "),
    spatial = paste(x$spatial, collapse = ", "),
    spatial_range_prior = x$priors$matern_s[1],
    spatiotemporal_range_prior = x$priors$matern_st[1],
    spatial_sd_prior = x$priors$matern_s[2],
    spatiotemporal_sd_prior = x$priors$matern_st[2],
    family = fam,
    svc = if (is.null(x$spatial_varying)) "" else x$spatial_varying,
    mesh_vertices = x$spde$mesh$n,
    stringsAsFactors = FALSE
    )
  out$family <- ifelse(grepl("delta_lognormal\\(", out$family), "delta_lognormal", out$family)
  out$family <- ifelse(grepl("delta_gamma\\(", out$family), "delta_gamma", out$family)
  out$family <- ifelse(grepl("delta_lognormal_mix\\(", out$family), "delta_lognormal_mix", out$family)
  out$family <- ifelse(grepl("nbinom2\\(", out$family), "nbinom2", out$family)
  out$family <- ifelse(grepl("tweedie\\(", out$family), "tweedie", out$family)
  out$svc <- ifelse(grepl("year_scaled", out$svc), "year", out$svc)
  out$tag <- tag
  if (tag2 != "") out$tag <- paste0(out$tag, "- ", tag2)
  out
}


mm <- lapply(fits_index_mat, \(x) x$data$lengthgroup[1]) |> unlist()
# mm <- lapply(fits_svc_mat, \(x) x$data$lengthgroup[1]) |> unlist()
mn <- rev(c("Mature males", "Mature females", "Maturing males", "Maturing females", "Immature"))
mm
mn

x <- list()
x[[1]] <- get_stuff(fit_trawl_lognormal_mix_pl, "Coastwide trawl index")
x[[2]] <- get_stuff(fit_iphc_nb2, "Coastwide longline index")
x[[3]] <- purrr::map_dfr(fits_index_mat, get_stuff, tag = "Coastwide trawl index") |>
  mutate(tag = paste0(tag, " (", mn, ")"))
x[[4]] <- get_stuff(fit_svc_all, "Coastwide SVC")
x[[5]] <- purrr::map_dfr(fits_svc_mat, get_stuff, tag = "Coastwide SVC") |> mutate(tag = paste0(tag, " (", mn, ")"))


bind_rows(x) |>
  mutate(pc_spatial = paste(spatial_range_prior, spatial_sd_prior, sep = ", ")) |>
  mutate(pc_spatiotemporal = paste(spatial_range_prior, spatial_sd_prior, sep = ", ")) |>
  select(model = tag, family, spatial, spatiotemporal, svc, pc_spatial, pc_spatiotemporal, mesh_vertices) |>
  knitr::kable()
