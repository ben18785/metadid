#' Fit a Bayesian meta-analysis model across study designs
#'
#' Fits a hierarchical Bayesian model synthesising treatment effects from
#' studies with different designs: difference-in-differences (DiD), randomised
#' controlled trials (RCT), and pre-post studies. All designs contribute to a
#' shared population treatment effect.
#'
#' @param summary_data A data frame with one row per study containing
#'   summary statistics. Must include columns `study_id` and `design`. See
#'   [validate_summary_data()] for the full column specification per design.
#'   Valid designs: `"did"`, `"did_change"`, `"rct"`, `"pp"`.
#' @param individual_data A data frame in long format with one row per
#'   observation. Must include columns `study_id`, `design`, `group`,
#'   `time`, and `value`. Valid designs: `"did"`, `"rct"`, `"pp"`.
#'   No `study_id` may appear in both `summary_data` and `individual_data`.
#' @param normalise_by_baseline Logical. If `TRUE` (default), all means and
#'   SDs are divided by each study's pre-treatment control mean (or the
#'   grand mean for change-only studies), placing outcomes on a common
#'   fractional scale.
#' @param robust_heterogeneity Logical. If `TRUE`, study-level treatment
#'   effects are drawn from a Student-t distribution rather than a normal,
#'   providing robustness to outlier studies. The degrees-of-freedom
#'   parameter is estimated with the prior specified in `priors$nu`.
#' @param design_effects Logical. If `TRUE`, additive offsets on the
#'   population treatment effect mean are estimated for RCT and Pre-Post
#'   studies relative to DiD (the reference). Useful for testing whether
#'   designs yield systematically different effect estimates.
#' @param hierarchical_rho Logical. If `TRUE` (default), the pre-post
#'   correlation is modelled hierarchically across studies. Studies with a
#'   reported correlation inform the population distribution; studies without
#'   one have their correlation imputed.
#' @param priors A `did_priors` object from [set_priors()]. Controls the
#'   prior distributions on all population-level parameters.
#' @param chains Number of MCMC chains. Default `4`.
#' @param iter_warmup Number of warmup iterations per chain. Default `1000`.
#' @param iter_sampling Number of sampling iterations per chain. Default `1000`.
#' @param seed Integer random seed for reproducibility. Default `NULL`.
#' @param ... Additional arguments passed to the `$sample()` method of the
#'   CmdStanModel object (e.g., `parallel_chains`, `adapt_delta`).
#'
#' @return A `meta_did_fit` object. See [print.meta_did_fit()] and
#'   [summary.meta_did_fit()] for extracting results.
#'
#' @export
#'
#' @examples
#' if (instantiate::stan_cmdstan_exists()) {
#'   studies <- data.frame(
#'     study_id            = c("Smith 2020", "Jones 2021"),
#'     design              = c("did", "rct"),
#'     n_control           = c(50, 60),
#'     mean_pre_control    = c(0.45, NA),
#'     mean_post_control   = c(0.42, 0.48),
#'     sd_pre_control      = c(0.12, NA),
#'     sd_post_control     = c(0.11, 0.12),
#'     n_treatment         = c(55, 65),
#'     mean_pre_treatment  = c(0.46, NA),
#'     mean_post_treatment = c(0.30, 0.35),
#'     sd_pre_treatment    = c(0.13, NA),
#'     sd_post_treatment   = c(0.10, 0.11),
#'     rho                 = c(0.75, NA)
#'   )
#'   fit <- meta_did(summary_data = studies)
#' }
meta_did <- function(
    summary_data          = NULL,
    individual_data       = NULL,
    normalise_by_baseline = TRUE,
    robust_heterogeneity  = FALSE,
    design_effects        = FALSE,
    hierarchical_rho      = TRUE,
    priors                = set_priors(),
    chains                = 4L,
    iter_warmup           = 1000L,
    iter_sampling         = 1000L,
    seed                  = NULL,
    ...
) {
  # --- Input checks ---
  if (is.null(summary_data) && is.null(individual_data)) {
    stop("At least one of summary_data or individual_data must be provided.",
         call. = FALSE)
  }
  validate_summary_data(summary_data)
  validate_individual_data(individual_data)

  # Check no study_id overlap between the two inputs
  if (!is.null(summary_data) && !is.null(individual_data)) {
    overlap <- intersect(summary_data$study_id, individual_data$study_id)
    if (length(overlap) > 0) {
      stop(
        "The same study_id appears in both summary_data and individual_data: ",
        paste(overlap, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  # --- Normalisation ---
  normalisation_factors <- NULL
  if (normalise_by_baseline) {
    norm_result           <- normalise_summary(summary_data, individual_data)
    summary_data          <- norm_result$summary_data
    individual_data       <- norm_result$individual_data
    normalisation_factors <- norm_result$factors
  }

  # --- Model flags ---
  model_flags <- list(
    is_baseline_normalised                  = as.integer(normalise_by_baseline),
    is_correlation_coefficient_hierarchical = as.integer(hierarchical_rho),
    is_student_t_heterogeneity              = as.integer(robust_heterogeneity),
    is_design_effect                        = as.integer(design_effects)
  )

  # --- Stan data ---
  stan_data <- prepare_stan_data(summary_data, individual_data, model_flags, priors)

  # --- Fit ---
  model <- instantiate::stan_package_model(
    name    = "meta_analysis_master",
    package = "metadid"
  )

  fit <- model$sample(
    data          = stan_data,
    chains        = chains,
    iter_warmup   = iter_warmup,
    iter_sampling = iter_sampling,
    seed          = seed,
    ...
  )

  # --- Return ---
  new_meta_did_fit(
    fit                   = fit,
    summary_data          = summary_data,
    individual_data       = individual_data,
    model_flags           = model_flags,
    priors                = priors,
    normalisation_factors = normalisation_factors
  )
}

# ---------------------------------------------------------------------------
# Normalisation helper
# ---------------------------------------------------------------------------

normalise_summary <- function(summary_data, individual_data) {
  factors <- list()

  if (!is.null(summary_data) && nrow(summary_data) > 0) {
    # DiD: normalise by each study's own mean_pre_control
    did_rows <- summary_data$design == "did"
    if (any(did_rows)) {
      norm <- summary_data$mean_pre_control[did_rows]
      mean_cols <- grep("^mean_", names(summary_data), value = TRUE)
      sd_cols   <- grep("^sd_",   names(summary_data), value = TRUE)
      summary_data[did_rows, mean_cols] <-
        sweep(summary_data[did_rows, mean_cols, drop = FALSE], 1, norm, "/")
      summary_data[did_rows, sd_cols] <-
        sweep(summary_data[did_rows, sd_cols,   drop = FALSE], 1, norm, "/")
      factors$did <- stats::setNames(norm, summary_data$study_id[did_rows])
    }

    # RCT: normalise by each study's mean_post_control (best available baseline)
    rct_rows <- summary_data$design == "rct"
    if (any(rct_rows)) {
      norm <- summary_data$mean_post_control[rct_rows]
      mean_cols <- grep("^mean_post", names(summary_data), value = TRUE)
      sd_cols   <- grep("^sd_post",  names(summary_data), value = TRUE)
      summary_data[rct_rows, mean_cols] <-
        sweep(summary_data[rct_rows, mean_cols, drop = FALSE], 1, norm, "/")
      summary_data[rct_rows, sd_cols] <-
        sweep(summary_data[rct_rows, sd_cols,   drop = FALSE], 1, norm, "/")
      factors$rct <- stats::setNames(norm, summary_data$study_id[rct_rows])
    }

    # PP: normalise by each study's mean_pre_treatment
    pp_rows <- summary_data$design == "pp"
    if (any(pp_rows)) {
      norm <- summary_data$mean_pre_treatment[pp_rows]
      mean_cols <- grep("^mean_", names(summary_data), value = TRUE)
      sd_cols   <- grep("^sd_",   names(summary_data), value = TRUE)
      summary_data[pp_rows, mean_cols] <-
        sweep(summary_data[pp_rows, mean_cols, drop = FALSE], 1, norm, "/")
      summary_data[pp_rows, sd_cols] <-
        sweep(summary_data[pp_rows, sd_cols,   drop = FALSE], 1, norm, "/")
      factors$pp <- stats::setNames(norm, summary_data$study_id[pp_rows])
    }

    # did_change: normalise by grand mean of mean_pre_control across did studies
    did_change_rows <- summary_data$design == "did_change"
    if (any(did_change_rows)) {
      grand_mean <- mean(factors$did, na.rm = TRUE)
      if (is.nan(grand_mean)) grand_mean <- 1
      mean_cols <- grep("^mean_change", names(summary_data), value = TRUE)
      sd_cols   <- grep("^sd_change",  names(summary_data), value = TRUE)
      summary_data[did_change_rows, mean_cols] <-
        summary_data[did_change_rows, mean_cols, drop = FALSE] / grand_mean
      summary_data[did_change_rows, sd_cols] <-
        summary_data[did_change_rows, sd_cols, drop = FALSE] / grand_mean
      factors$did_change <- grand_mean
    }
  }

  if (!is.null(individual_data) && nrow(individual_data) > 0) {
    individual_data <- individual_data |>
      dplyr::group_by(.data$study_id) |>
      dplyr::mutate(
        value = .data$value / mean(.data$value[.data$group == "control" &
                                                 .data$time == "pre"],
                                   na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  }

  list(
    summary_data    = summary_data,
    individual_data = individual_data,
    factors         = factors
  )
}
