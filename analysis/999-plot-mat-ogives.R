plot_mat_ogive <- function(object,
  col = c("M" = "grey50", "F" = "black"),
  xlab = if (object$type[[1]] == "age") "Age (years)" else "Length (cm)",
  title =
    if (object$type[[1]] == "age") "Age at maturity" else "Length at maturity",
  rug = TRUE, rug_n = 1500, x_max = 1.75,
  prediction_type = c("all", "male", "female", "none"),
  text_label_size = 3,
  show_quant_text = TRUE,
  french = FALSE) {
  if (object$sample_id_re) {
    b <- glmmTMB::fixef(object$model)[[1L]]
  } else {
    if (object$year_re) {
      b <- glmmTMB::fixef(object$model)[[1L]]
    } else {
      b <- stats::coef(object$model)
    }
  }

  en2fr <- function(x, ...) x # fake

  nd_re <- object$pred_data
  nd_fe <- object$pred_data |>
    dplyr::distinct(age_or_length, female, .keep_all = TRUE) # Remove duplicate predictions (predictions were done for each sample_id)
  # nd_fe <- filter(nd_re, year == nd_re$year[[1L]]) # fake; all same
  nd_fe$glmm_re <- NULL # also may not exist if no random effects

  prediction_type <- match.arg(prediction_type)
  if (prediction_type == "male") {
    nd_fe <- filter(nd_fe, female == 0L)
  }
  if (prediction_type == "female") {
    nd_fe <- filter(nd_fe, female == 1L)
  }

  # if (prediction_type == "none") {
  #   nd_fe <- filter(nd_fe, !female %in% c(0L, 1L)) # i.e. nothing
  # }

  binomial_perc <- gfplot:::binomial_perc
  m_perc <- data.frame(
    p0.5 = binomial_perc(a = b[[1]], b = b[[2]], perc = 0.5, linkinv = family(object$model)$linkinv)
  )
  m_perc$p0.95 <- binomial_perc(a = b[[1]], b = b[[2]], perc = 0.95, linkinv = family(object$model)$linkinv)
  m_perc$p0.05 <- binomial_perc(a = b[[1]], b = b[[2]], perc = 0.05, linkinv = family(object$model)$linkinv)

  f_perc <- data.frame(
    p0.5 = binomial_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.5, linkinv = family(object$model)$linkinv)
  )
  f_perc$p0.95 <- binomial_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.95, linkinv = family(object$model)$linkinv)
  f_perc$p0.05 <- binomial_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.05, linkinv = family(object$model)$linkinv)

  labs_f <- tibble(
    p = c("05", "50", "95"),
    value = c(f_perc$p0.05, f_perc$p0.5, f_perc$p0.95),
    x = 0.75 * max(nd_re$age_or_length), # re-calculated below
    y = seq(0.75, 0.6, length.out = 3L),
    sex = "F"
  )

  labs_m <- tibble(
    p = c("05", "50", "95"),
    value = c(m_perc$p0.05, m_perc$p0.5, m_perc$p0.95),
    x = 0.75 * max(nd_re$age_or_length), # re-calculated below
    y = seq(0.4, 0.25, length.out = 3),
    sex = "M"
  )
  labs <- bind_rows(labs_m, labs_f)

  if (prediction_type == "male") {
    labs <- filter(labs, sex == "M")
  }
  if (prediction_type == "female") {
    labs <- filter(labs, sex == "F")
  }

  nd_fe <- mutate(nd_fe, sex = ifelse(female == 1L, "F", "M"))
  nd_re <- mutate(nd_re, sex = ifelse(female == 1L, "F", "M"))
  object$data <- mutate(object$data, sex = ifelse(female == 1L, "F", "M"))

  if (object$type[[1]] == "age") {
    labs <- mutate(labs,
      label =
        paste0(
          sex, " ", p, " = ",
          sprintf("%.1f", round(value, 1L)), en2fr("y", translate = french)
        )
    )
  } else {
    labs <- mutate(labs,
      label =
        paste0(sex, " ", p, " = ", sprintf("%.1f", round(value, 1L)), "cm")
    )
  }
  max_x <- min(c(max(labs$value) * x_max, max(nd_fe$age_or_length)))

  if (object$type[[1]] == "age") {
    labs <- mutate(labs, x = max_x * 0.7) # actual x position calculation
  } else {
    labs <- mutate(labs, x = max_x * 0.05) # actual x position calculation
  }

  nd_fe$sex <- factor(nd_fe$sex, levels = c("F", "M"))
  nd_re$sex <- factor(nd_re$sex, levels = c("F", "M"))
  labs$sex <- factor(labs$sex, levels = c("F", "M"))
  labs$sex <- factor(labs$sex, levels = c("F", "M"))

  if (isTRUE(french)) {
    labs$label <- gsub("\\.", ",", labs$label)
  }
  g <- ggplot(nd_fe, aes_string("age_or_length", "glmm_fe", colour = "sex"))

  if ("glmm_re" %in% names(nd_re)) {
    if (object$sample_id_re) {
      # n_re <- length(unique(nd_re$sample_id))/5
      n_re <- length(unique(nd_re$sample_id))
      # n_re2 <- ifelse(n_re < 15, 15, n_re)
      g <- g + geom_line(
        data = nd_re,
        aes_string("age_or_length", "glmm_re",
          group = "paste(sample_id, sex)",
          colour = "sex"
        ), inherit.aes = FALSE, alpha = 0.05,
        show.legend = FALSE
      )
    } else {
      if (object$year_re) {
        n_re <- length(unique(nd_re$year))
        g <- g + geom_line(
          data = nd_re,
          aes_string("age_or_length", "glmm_re",
            group = "paste(year, sex)",
            colour = "sex", lty = "sex"
          ), inherit.aes = FALSE, alpha = 2/n_re,
          show.legend = FALSE
        )
      }
    }
  }

  if (prediction_type != "none") {
    g <- g + geom_line(linewidth = 1.0) #lwd = 1, alpha = 0.8
    g <- g + geom_vline(
      data = filter(labs, p %in% c("05", "95")),
      aes_string(xintercept = "value", colour = "sex"),
      # lwd = 0.8,
      alpha = 0.95,
      show.legend = FALSE
    )
    if(show_quant_text){
      g <- g + geom_text(
        data = labs, aes_string(
          x = "x", y = "y", label = "label"
        ),
        hjust = 0, show.legend = FALSE, size = text_label_size
      )
    }
  }

  g <- g + scale_colour_manual(values = col,
    breaks = c("F", "M"), drop = FALSE) +
    # ggplot2::scale_linetype_discrete(breaks = c('F', 'M'), drop = FALSE) +
    xlab(en2fr(xlab, french)) + ylab(en2fr("Probability mature", french)) +
    # theme_pbs() +
    coord_cartesian(
      expand = FALSE, ylim = c(-0.005, 1.005),
      xlim = c(0, max_x)
    ) +
    labs(colour = en2fr("Sex", french), lty = en2fr("Sex", french))
    # ggplot2::ggtitle(en2fr(title, french))

  if (rug) {
    if (nrow(object$data) > rug_n) {
      temp <- object$data[sample(seq_len(nrow(object$data)), rug_n), , drop = FALSE]
    } else {
      temp <- object$data
    }
    position <- if (object$type == "age") "jitter" else "identity"
    g <- g + ggplot2::geom_rug(
      data = filter(temp, mature == 0L),
      sides = "b", position = position, alpha = 0.5, lty = 1,
      aes_string(x = "age_or_length", y = "as.numeric(mature)", colour = "sex", lty = "sex"), show.legend = FALSE
    )
    g <- g + ggplot2::geom_rug(
      data = filter(temp, mature == 1L),
      sides = "t", position = position, alpha = 0.5, lty = 1,
      aes_string(x = "age_or_length", y = "as.numeric(mature)", colour = "sex", lty = "sex"), show.legend = FALSE
    )
  }

  g
}
