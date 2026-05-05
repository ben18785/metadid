# S3 class for meta_did() results.

# ---------------------------------------------------------------------------
# Constructor
# ---------------------------------------------------------------------------

new_meta_did_fit <- function(
    fit,
    summary_data,
    individual_data,
    model_flags,
    priors,
    normalisation_factors,
    method = "sample"
) {
  structure(
    list(
      fit                   = fit,
      summary_data          = summary_data,
      individual_data       = individual_data,
      model_flags           = model_flags,
      priors                = priors,
      normalisation_factors = normalisation_factors,
      method                = method
    ),
    class = "meta_did_fit"
  )
}

# ---------------------------------------------------------------------------
# print()
# ---------------------------------------------------------------------------

#' Print a meta_did_fit object
#'
#' Displays a brief summary of the population treatment effect posterior.
#'
#' @param x A `meta_did_fit` object.
#' @param prob Width of the credible interval. Default `0.9`.
#' @param ... Ignored.
#' @export
print.meta_did_fit <- function(x, prob = 0.9, ...) {
  n_did    <- sum(x$summary_data$design == "did",        na.rm = TRUE) +
              sum(x$individual_data$design == "did",      na.rm = TRUE)
  n_rct    <- sum(x$summary_data$design == "rct",        na.rm = TRUE) +
              sum(x$individual_data$design == "rct",      na.rm = TRUE)
  n_pp     <- sum(x$summary_data$design == "pp",         na.rm = TRUE) +
              sum(x$individual_data$design == "pp",       na.rm = TRUE)
  n_change <- sum(x$summary_data$design == "did_change", na.rm = TRUE)

  cat("Bayesian meta-analysis (metadid)\n")
  cat("Studies: DiD =", n_did, "| RCT =", n_rct,
      "| Pre-Post =", n_pp, "| DiD (change only) =", n_change, "\n")

  if (x$method == "sample") {
    draws <- x$fit$draws("treatment_effect_mean", format = "matrix")
    m     <- mean(draws)
    lo    <- stats::quantile(draws, (1 - prob) / 2)
    hi    <- stats::quantile(draws, 1 - (1 - prob) / 2)
    cat(sprintf(
      "Population treatment effect: %.3f  %g%% CI [%.3f, %.3f]\n",
      m, prob * 100, lo, hi
    ))
    if (x$model_flags$is_student_t_heterogeneity) {
      nu <- mean(x$fit$draws("nu_treatment_vec", format = "matrix"))
      cat(sprintf("  (Student-t heterogeneity; estimated df = %.1f)\n", nu))
    }
    if (x$model_flags$is_design_effect) {
      delta_rct <- mean(x$fit$draws("treatment_effect_mean_rct", format = "matrix")) - m
      delta_pp  <- mean(x$fit$draws("treatment_effect_mean_pp",  format = "matrix")) - m
      cat(sprintf("  Design offsets: RCT = %.3f, Pre-Post = %.3f\n",
                  delta_rct, delta_pp))
    }
  } else {
    mle <- x$fit$mle()
    m   <- mle[["treatment_effect_mean"]]
    cat(sprintf(
      "Population treatment effect: %.3f  (MAP estimate, no uncertainty)\n", m
    ))
    if (x$model_flags$is_design_effect) {
      delta_rct <- mle[["treatment_effect_mean_rct"]] - m
      delta_pp  <- mle[["treatment_effect_mean_pp"]]  - m
      cat(sprintf("  Design offsets: RCT = %.3f, Pre-Post = %.3f\n",
                  delta_rct, delta_pp))
    }
  }

  invisible(x)
}

# ---------------------------------------------------------------------------
# summary()
# ---------------------------------------------------------------------------

#' Summarise a meta_did_fit object
#'
#' Returns a data frame of posterior summaries for the population-level
#' treatment effect parameters and each study-level treatment effect.
#'
#' @param object A `meta_did_fit` object.
#' @param prob Width of the credible interval. Default `0.9`.
#' @param ... Ignored.
#'
#' @return A data frame with columns `parameter`, `mean`, `sd`, `lo`, `hi`.
#' @export
summary.meta_did_fit <- function(object, prob = 0.9, ...) {
  lo_q <- (1 - prob) / 2
  hi_q <- 1 - lo_q

  if (object$method == "sample") {
    summarise_draws <- function(name) {
      d <- object$fit$draws(name, format = "matrix")
      data.frame(
        parameter = name,
        mean      = apply(d, 2, mean),
        sd        = apply(d, 2, stats::sd),
        lo        = apply(d, 2, stats::quantile, probs = lo_q),
        hi        = apply(d, 2, stats::quantile, probs = hi_q),
        row.names = NULL
      )
    }
  } else {
    # MAP: mle() returns a named vector; match scalar and vector parameters
    mle <- object$fit$mle()
    summarise_draws <- function(name) {
      # Match both scalar (exact) and vector (name[i]) parameters
      matched <- mle[grepl(paste0("^", name, "(\\[|$)"), names(mle))]
      if (length(matched) == 0L) return(NULL)
      data.frame(
        parameter = name,
        mean      = unname(matched),
        sd        = NA_real_,
        lo        = NA_real_,
        hi        = NA_real_,
        row.names = NULL
      )
    }
  }

  # Population-level parameters
  pop <- rbind(
    summarise_draws("treatment_effect_mean"),
    summarise_draws("treatment_effect_sd")
  )
  if (object$model_flags$is_design_effect) {
    pop <- rbind(pop,
      summarise_draws("treatment_effect_mean_rct"),
      summarise_draws("treatment_effect_mean_pp")
    )
  }

  # Study-level treatment effects (all design types combined)
  n_design <- function(data, design) {
    if (is.null(data)) return(0L)
    sum(data$design == design, na.rm = TRUE)
  }
  study_params <- c(
    if (n_design(object$summary_data, "did")        > 0) "treatment_effect_did_summary"     else NULL,
    if (n_design(object$summary_data, "rct")        > 0) "treatment_effect_rct_summary_derived" else NULL,
    if (n_design(object$summary_data, "pp")         > 0) "treatment_effect_pp_summary"      else NULL,
    if (n_design(object$summary_data, "did_change") > 0) "treatment_effect_did_change_only" else NULL,
    if (n_design(object$individual_data, "did")     > 0) "treatment_effect_did"             else NULL,
    if (n_design(object$individual_data, "rct")     > 0) "treatment_effect_rct_derived"     else NULL,
    if (n_design(object$individual_data, "pp")      > 0) "treatment_effect_pp"              else NULL
  )
  study <- do.call(rbind, lapply(study_params, summarise_draws))

  rbind(pop, study)
}

# ---------------------------------------------------------------------------
# tidy() stub
# ---------------------------------------------------------------------------

#' Tidy a meta_did_fit object
#'
#' @param x A `meta_did_fit` object.
#' @param ... Ignored.
#' @return `NULL` (not yet implemented).
#' @export
tidy.meta_did_fit <- function(x, ...) {
  message("tidy.meta_did_fit is not yet implemented.")
  invisible(NULL)
}
