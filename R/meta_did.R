#' Fit a Bayesian meta-analysis model across study designs
#'
#' Fits a hierarchical Bayesian model synthesising treatment effects from
#' studies with different designs: difference-in-differences (DiD), randomised
#' controlled trials (RCT), and pre-post studies. All designs contribute to a
#' shared population treatment effect.
#'
#' DiD studies are required for identification of the treatment effect. Without
#' them, the treatment effect is confounded with time trends (pre-post) or
#' baseline group differences (RCT). See `vignette("model-details")` for a
#' full discussion of the model, normalisation, and identification.
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
#' @param covariates An optional one-sided formula specifying study-level
#'   covariates for meta-regression on the treatment effect (e.g.,
#'   `~ dose + year`). The named columns must be numeric and present in
#'   both `summary_data` and `individual_data` (whichever are provided).
#'   For individual-level data, covariate values must be constant within
#'   each study. Default `NULL` (no meta-regression).
#' @param center_covariates Logical. If `TRUE` (default), covariates are
#'   mean-centered across all studies before fitting. This ensures that
#'   `treatment_effect_mean` is the population treatment effect at the
#'   average covariate values. Set to `FALSE` to use raw covariate values,
#'   in which case `treatment_effect_mean` is the effect when all covariates
#'   equal zero. The covariate coefficients (`beta_cov`) have the same
#'   interpretation regardless of centering: the change in expected
#'   treatment effect per unit increase in the covariate.
#' @param method Inference method. `"sample"` (default) runs full MCMC via
#'   Stan's HMC-NUTS sampler and returns a posterior distribution. `"optimize"`
#'   finds the maximum a posteriori (MAP) estimate via L-BFGS and is
#'   substantially faster, but returns only a point estimate with no
#'   uncertainty quantification.
#' @param chains Number of MCMC chains. Ignored when `method = "optimize"`.
#'   Default `4`.
#' @param iter_warmup Number of warmup iterations per chain. Ignored when
#'   `method = "optimize"`. Default `1000`.
#' @param iter_sampling Number of sampling iterations per chain. Ignored when
#'   `method = "optimize"`. Default `1000`.
#' @param seed Integer random seed for reproducibility. Default `NULL`.
#' @param allow_no_did Logical. If `FALSE` (default), `meta_did()` will
#'   stop with an error when no DiD studies are present, because the
#'   treatment effect is not identified from the data without the
#'   double-difference structure. Set to `TRUE` to override this check
#'   if you understand the limitation (the posterior will be prior-driven).
#' @param ... Additional arguments passed to the underlying CmdStanModel
#'   method: `$sample()` when `method = "sample"` (e.g., `parallel_chains`,
#'   `adapt_delta`) or `$optimize()` when `method = "optimize"` (e.g.,
#'   `algorithm`, `iter`).
#'
#' @return A `meta_did_fit` object. See [print.meta_did_fit()] and
#'   [summary.meta_did_fit()] for extracting results. When
#'   `method = "optimize"`, the summary contains MAP point estimates only;
#'   `sd`, `lo`, and `hi` columns will be `NA`.
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
    covariates            = NULL,
    center_covariates     = TRUE,
    priors                = set_priors(),
    method                = c("sample", "optimize"),
    chains                = 4L,
    iter_warmup           = 1000L,
    iter_sampling         = 1000L,
    seed                  = NULL,
    allow_no_did          = FALSE,
    ...
) {
  .meta_did_core(
    summary_data          = summary_data,
    individual_data       = individual_data,
    normalise_by_baseline = normalise_by_baseline,
    robust_heterogeneity  = robust_heterogeneity,
    design_effects        = design_effects,
    hierarchical_rho      = hierarchical_rho,
    covariates            = covariates,
    center_covariates     = center_covariates,
    priors                = priors,
    method                = method,
    chains                = chains,
    iter_warmup           = iter_warmup,
    iter_sampling         = iter_sampling,
    seed                  = seed,
    allow_no_did          = allow_no_did,
    ...
  )
}


#' Fit a meta-analysis model with explicit control over design assumptions
#'
#' A flexible interface for controlling how nuisance parameters are handled
#' for non-DiD study designs. By default, behaves identically to
#' [meta_did()]. Users can independently control whether time trends and
#' baseline imbalances are estimated or fixed to zero for RCT and pre-post
#' studies.
#'
#' DiD studies always estimate both time trends and baseline differences
#' regardless of these settings, since they provide the identifying
#' information for these parameters.
#'
#' @inheritParams meta_did
#' @param time_trend How to handle the time trend \eqn{\beta_i} for non-DiD
#'   studies. One of:
#'   \describe{
#'     \item{`"pooled"`}{(Default) Estimate study-level time trends with a
#'       hierarchical prior shared across designs. Information from DiD studies
#'       informs the RCT and pre-post time trends. This is the same behaviour
#'       as [meta_did()].}
#'     \item{`"fixed_zero"`}{Fix \eqn{\beta_i = 0} for RCT and pre-post
#'       studies. For pre-post studies, this means the pre-post change is
#'       attributed entirely to treatment. For RCT studies, the
#'       reparameterised time trend correction is bypassed.}
#'   }
#' @param baseline_imbalance How to handle the baseline difference
#'   \eqn{\gamma_i} between treatment and control groups for RCT studies.
#'   One of:
#'   \describe{
#'     \item{`"estimated"`}{(Default) Estimate \eqn{\gamma_i}, borrowing
#'       information from DiD studies when baseline-normalised. This is the
#'       same behaviour as [meta_did()].}
#'     \item{`"fixed_zero"`}{Fix \eqn{\gamma_i = 0}, assuming randomisation
#'       eliminates baseline imbalances. This is the standard RCT assumption.}
#'   }
#'
#' @return A `meta_did_fit` object, identical in structure to the return
#'   value of [meta_did()].
#'
#' @seealso [meta_did()] for the standard model, which is equivalent to
#'   `meta_did_general()` with default settings.
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
#'
#'   # Borrow time trends from DiD, but assume equal baselines for RCT
#'   fit <- meta_did_general(
#'     summary_data       = studies,
#'     baseline_imbalance = "fixed_zero"
#'   )
#' }
meta_did_general <- function(
    summary_data          = NULL,
    individual_data       = NULL,
    normalise_by_baseline = TRUE,
    robust_heterogeneity  = FALSE,
    design_effects        = FALSE,
    hierarchical_rho      = TRUE,
    covariates            = NULL,
    center_covariates     = TRUE,
    priors                = set_priors(),
    time_trend            = c("pooled", "fixed_zero"),
    baseline_imbalance    = c("estimated", "fixed_zero"),
    method                = c("sample", "optimize"),
    chains                = 4L,
    iter_warmup           = 1000L,
    iter_sampling         = 1000L,
    seed                  = NULL,
    allow_no_did          = FALSE,
    ...
) {
  time_trend         <- match.arg(time_trend)
  baseline_imbalance <- match.arg(baseline_imbalance)

  overrides <- list()

  if (time_trend == "fixed_zero") {
    overrides$is_time_trend_pp_zero             <- 1L
    overrides$is_time_trend_pp_summary_zero     <- 1L
    overrides$is_time_trend_rct_zero            <- 1L
    overrides$is_time_trend_rct_summary_zero    <- 1L
  }

  if (baseline_imbalance == "fixed_zero") {
    overrides$is_baseline_control_equal_treatment_rct         <- 1L
    overrides$is_baseline_control_equal_treatment_rct_summary <- 1L
  }

  .meta_did_core(
    summary_data          = summary_data,
    individual_data       = individual_data,
    normalise_by_baseline = normalise_by_baseline,
    robust_heterogeneity  = robust_heterogeneity,
    design_effects        = design_effects,
    hierarchical_rho      = hierarchical_rho,
    covariates            = covariates,
    center_covariates     = center_covariates,
    priors                = priors,
    method                = method,
    chains                = chains,
    iter_warmup           = iter_warmup,
    iter_sampling         = iter_sampling,
    seed                  = seed,
    allow_no_did          = allow_no_did,
    stan_data_overrides   = if (length(overrides) > 0) overrides else NULL,
    ...
  )
}


#' Fit a naive meta-analysis model (no borrowing across designs)
#'
#' @description
#' **Deprecated.** `meta_did_naive()` is deprecated in favour of [meta_did_general()] with
#' `time_trend = "fixed_zero"` and `baseline_imbalance = "fixed_zero"`,
#' which provides the same behaviour with more explicit control.
#'
#' This function imposes the "naive" assumptions typically made when
#' analysing each design in isolation:
#' \itemize{
#'   \item **RCT**: Baseline means are assumed equal across treatment and
#'     control groups (randomisation assumption), so the baseline difference
#'     \eqn{\gamma} is fixed to zero.
#'   \item **Pre-post**: The time trend \eqn{\beta} is fixed to zero, so the
#'     pre-post change is attributed entirely to the treatment effect.
#' }
#'
#' @inheritParams meta_did
#' @return A `meta_did_fit` object, identical in structure to the return
#'   value of [meta_did()].
#'
#' @seealso [meta_did_general()] for independent control over assumptions.
#' @export
#' @keywords internal
meta_did_naive <- function(
    summary_data          = NULL,
    individual_data       = NULL,
    normalise_by_baseline = TRUE,
    robust_heterogeneity  = FALSE,
    design_effects        = FALSE,
    hierarchical_rho      = TRUE,
    covariates            = NULL,
    center_covariates     = TRUE,
    priors                = set_priors(),
    method                = c("sample", "optimize"),
    chains                = 4L,
    iter_warmup           = 1000L,
    iter_sampling         = 1000L,
    seed                  = NULL,
    allow_no_did          = FALSE,
    ...
) {
  .Deprecated("meta_did_general",
              msg = paste(
                "'meta_did_naive()' is deprecated.",
                "Use meta_did_general(time_trend = \"fixed_zero\",",
                "baseline_imbalance = \"fixed_zero\") instead."
              ))
  meta_did_general(
    summary_data          = summary_data,
    individual_data       = individual_data,
    normalise_by_baseline = normalise_by_baseline,
    robust_heterogeneity  = robust_heterogeneity,
    design_effects        = design_effects,
    hierarchical_rho      = hierarchical_rho,
    covariates            = covariates,
    center_covariates     = center_covariates,
    priors                = priors,
    time_trend            = "fixed_zero",
    baseline_imbalance    = "fixed_zero",
    method                = method,
    chains                = chains,
    iter_warmup           = iter_warmup,
    iter_sampling         = iter_sampling,
    seed                  = seed,
    allow_no_did          = allow_no_did,
    ...
  )
}


# ---------------------------------------------------------------------------
# Internal core implementation shared by meta_did() and meta_did_naive()
# ---------------------------------------------------------------------------

.meta_did_core <- function(
    summary_data          = NULL,
    individual_data       = NULL,
    normalise_by_baseline = TRUE,
    robust_heterogeneity  = FALSE,
    design_effects        = FALSE,
    hierarchical_rho      = TRUE,
    covariates            = NULL,
    center_covariates     = TRUE,
    priors                = set_priors(),
    method                = c("sample", "optimize"),
    chains                = 4L,
    iter_warmup           = 1000L,
    iter_sampling         = 1000L,
    seed                  = NULL,
    allow_no_did          = FALSE,
    stan_data_overrides   = NULL,
    ...
) {
  method <- match.arg(method)

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

  # --- DiD identification check ---
  n_did <- sum(c(
    if (!is.null(summary_data))    sum(summary_data$design %in% c("did", "did_change")),
    if (!is.null(individual_data)) sum(individual_data$design == "did")
  ))
  if (n_did == 0 && !allow_no_did) {
    stop(
      "No DiD studies found. Without DiD studies, the treatment effect is not ",
      "identified from the data \u2014 it is confounded with time trends (pre-post) ",
      "or baseline group differences (RCT), and the posterior will be driven by ",
      "the priors rather than the data.\n",
      "If you understand this limitation and wish to proceed, set allow_no_did = TRUE.",
      call. = FALSE
    )
  }

  # --- Parse covariates ---
  covariate_names <- NULL
  if (!is.null(covariates)) {
    if (!inherits(covariates, "formula")) {
      stop("'covariates' must be a one-sided formula (e.g., ~ dose + year).",
           call. = FALSE)
    }
    # Extract variable names from the formula (drop intercept)
    covariate_names <- all.vars(covariates)
    if (length(covariate_names) == 0) {
      stop("'covariates' formula contains no variables.", call. = FALSE)
    }
    validate_covariates(covariate_names, summary_data, individual_data)
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
  stan_data <- prepare_stan_data(summary_data, individual_data, model_flags, priors,
                                  covariate_names = covariate_names,
                                  center_covariates = center_covariates)
  cov_centers <- attr(stan_data, "cov_centers")

  # Apply any overrides (e.g. naive-mode flags)
  if (!is.null(stan_data_overrides)) {
    stan_data[names(stan_data_overrides)] <- stan_data_overrides
  }

  # --- Fit ---
  stan_dir <- system.file("bin/stan", package = "metadid")
  if (nzchar(stan_dir)) {
    # Installed package: load pre-compiled binary directly via cmdstanr.
    # We bypass instantiate::stan_package_model() because it does not
    # propagate include_paths to the CmdStanModel object, causing stanc
    # to fail when the model uses #include directives.
    model <- cmdstanr::cmdstan_model(
      stan_file     = file.path(stan_dir, "meta_analysis_master.stan"),
      exe_file      = file.path(stan_dir, "meta_analysis_master"),
      include_paths = stan_dir
    )
  } else {
    # Development (devtools::load_all()): compile directly from src/stan/
    pkg_root <- dirname(system.file(package = "metadid"))
    stan_dir <- file.path(pkg_root, "src", "stan")
    model <- cmdstanr::cmdstan_model(
      stan_file     = file.path(stan_dir, "meta_analysis_master.stan"),
      include_paths = stan_dir
    )
  }

  fit <- if (method == "sample") {
    model$sample(
      data          = stan_data,
      chains        = chains,
      iter_warmup   = iter_warmup,
      iter_sampling = iter_sampling,
      seed          = seed,
      ...
    )
  } else {
    model$optimize(
      data   = stan_data,
      seed   = seed,
      ...
    )
  }

  # --- Return ---
  new_meta_did_fit(
    fit                   = fit,
    summary_data          = summary_data,
    individual_data       = individual_data,
    model_flags           = model_flags,
    priors                = priors,
    normalisation_factors = normalisation_factors,
    method                = method,
    covariate_names       = covariate_names,
    cov_centers           = cov_centers,
    center_covariates     = center_covariates
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
      dplyr::group_by(.data$study_id, .data$design) |>
      dplyr::mutate(
        value = .data$value / dplyr::case_when(
          # DiD: normalise by pre-treatment control mean
          .data$design == "did" ~ mean(
            .data$value[.data$group == "control" & .data$time == "pre"],
            na.rm = TRUE
          ),
          # RCT: normalise by post-treatment control mean (no pre data)
          .data$design == "rct" ~ mean(
            .data$value[.data$group == "control" & .data$time == "post"],
            na.rm = TRUE
          ),
          # Pre-post: normalise by pre-treatment treatment mean (no control)
          .data$design == "pp" ~ mean(
            .data$value[.data$group == "treatment" & .data$time == "pre"],
            na.rm = TRUE
          )
        )
      ) |>
      dplyr::ungroup()
  }

  list(
    summary_data    = summary_data,
    individual_data = individual_data,
    factors         = factors
  )
}
