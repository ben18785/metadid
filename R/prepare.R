# Internal data preparation functions.
# Adapts user-facing data formats to the flat lists expected by the Stan model.
# None of these functions are exported.

# ---------------------------------------------------------------------------
# filter_design()
# ---------------------------------------------------------------------------

filter_design <- function(data, design) {
  if (is.null(data) || nrow(data) == 0) return(data.frame())
  data[data$design == design, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Null Stan data (empty structures for absent design types)
# ---------------------------------------------------------------------------

null_stan_data_did <- function() {
  list(
    n_studies_did             = 0L,
    sample_size_control_did   = integer(0),
    sample_size_treatment_did = integer(0),
    study_start_control_did   = integer(0),
    study_end_control_did     = integer(0),
    study_start_treatment_did = integer(0),
    study_end_treatment_did   = integer(0),
    x_control_before_did      = numeric(0),
    x_control_after_did       = numeric(0),
    x_treatment_before_did    = numeric(0),
    x_treatment_after_did     = numeric(0)
  )
}

null_stan_data_rct <- function() {
  list(
    n_studies_rct               = 0L,
    sample_size_control_rct     = integer(0),
    sample_size_treatment_rct   = integer(0),
    study_start_control_rct     = integer(0),
    study_end_control_rct       = integer(0),
    study_start_treatment_rct   = integer(0),
    study_end_treatment_rct     = integer(0),
    x_control_after_rct         = numeric(0),
    x_treatment_after_rct       = numeric(0),
    is_time_trend_rct_zero = 0L
  )
}

null_stan_data_pp <- function() {
  list(
    n_studies_pp               = 0L,
    sample_size_treatment_pp   = integer(0),
    study_start_treatment_pp   = integer(0),
    study_end_treatment_pp     = integer(0),
    x_treatment_before_pp      = numeric(0),
    x_treatment_after_pp       = numeric(0),
    is_time_trend_pp_zero       = 0L,
    is_differenced_likelihood_pp = 1L
  )
}

null_stan_data_did_summary <- function() {
  list(
    n_studies_did_summary                  = 0L,
    x_bar_control_before_did_summary       = numeric(0),
    x_bar_control_after_did_summary        = numeric(0),
    x_bar_treatment_before_did_summary     = numeric(0),
    x_bar_treatment_after_did_summary      = numeric(0),
    sample_size_control_did_summary        = integer(0),
    sample_size_treatment_did_summary      = integer(0),
    sd_control_before_did_summary          = numeric(0),
    sd_control_after_did_summary           = numeric(0),
    sd_treatment_before_did_summary        = numeric(0),
    sd_treatment_after_did_summary         = numeric(0),
    is_differenced_likelihood_did_summary  = 0L,
    n_rho_known_did_summary                = 0L,
    n_rho_missing_did_summary              = 0L,
    idx_rho_known_did_summary              = integer(0),
    idx_rho_missing_did_summary            = integer(0),
    rho_known_did_summary                  = numeric(0)
  )
}

null_stan_data_did_change_only <- function() {
  list(
    n_studies_did_change_only              = 0L,
    x_bar_change_control_did_change_only   = numeric(0),
    x_bar_change_treatment_did_change_only = numeric(0),
    sample_size_control_did_change_only    = integer(0),
    sample_size_treatment_did_change_only  = integer(0),
    sd_change_control_did_change_only      = numeric(0),
    sd_change_treatment_did_change_only    = numeric(0)
  )
}

null_stan_data_rct_summary <- function() {
  list(
    n_studies_rct_summary                           = 0L,
    x_bar_control_after_rct_summary                 = numeric(0),
    x_bar_treatment_after_rct_summary               = numeric(0),
    sample_size_control_rct_summary                 = integer(0),
    sample_size_treatment_rct_summary               = integer(0),
    sd_control_after_rct_summary                    = numeric(0),
    sd_treatment_after_rct_summary                  = numeric(0),
    is_time_trend_rct_summary_zero = 0L
  )
}

null_stan_data_pp_summary <- function() {
  list(
    n_studies_pp_summary                 = 0L,
    x_bar_treatment_before_pp_summary    = numeric(0),
    x_bar_treatment_after_pp_summary     = numeric(0),
    sample_size_treatment_pp_summary     = integer(0),
    sd_treatment_before_pp_summary       = numeric(0),
    sd_treatment_after_pp_summary        = numeric(0),
    is_time_trend_pp_summary_zero        = 0L,
    is_differenced_likelihood_pp_summary = 1L,
    n_rho_known_pp_summary               = 0L,
    n_rho_missing_pp_summary             = 0L,
    idx_rho_known_pp_summary             = integer(0),
    idx_rho_missing_pp_summary           = integer(0),
    rho_known_pp_summary                 = numeric(0)
  )
}

# ---------------------------------------------------------------------------
# Rho split helper (known vs missing)
# ---------------------------------------------------------------------------

split_rho <- function(rho_vec) {
  idx_known   <- which(!is.na(rho_vec))
  idx_missing <- which( is.na(rho_vec))
  list(
    n_known     = length(idx_known),
    n_missing   = length(idx_missing),
    idx_known   = as.integer(idx_known),
    idx_missing = as.integer(idx_missing),
    rho_known   = rho_vec[idx_known]
  )
}

# ---------------------------------------------------------------------------
# adapt_individual()  -- long (group/time/value) → wide (type/before/after)
# ---------------------------------------------------------------------------

adapt_individual <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data.frame())

  design <- unique(data$design)

  if (any(design %in% c("did", "pp"))) {
    # Repeated-measures designs: pivot on subject_id for correct pairing
    data |>
      tidyr::pivot_wider(
        id_cols     = c("study_id", "group", "subject_id"),
        names_from  = "time",
        values_from = "value"
      ) |>
      dplyr::select(-"subject_id") |>
      dplyr::rename(type = "group", before = "pre", after = "post")
  } else {
    # RCT: post-only, no pairing needed
    data |>
      dplyr::rename(type = "group", after = "value") |>
      dplyr::select("study_id", "type", "after")
  }
}

# ---------------------------------------------------------------------------
# Summary-level adapters  -- user columns → internal names
# ---------------------------------------------------------------------------

adapt_summary_did <- function(data) {
  if (nrow(data) == 0) return(null_stan_data_did_summary())
  rho <- if ("rho" %in% names(data)) data$rho else rep(NA_real_, nrow(data))
  rho_split <- split_rho(rho)
  list(
    n_studies_did_summary                  = nrow(data),
    x_bar_control_before_did_summary       = data$mean_pre_control,
    x_bar_control_after_did_summary        = data$mean_post_control,
    x_bar_treatment_before_did_summary     = data$mean_pre_treatment,
    x_bar_treatment_after_did_summary      = data$mean_post_treatment,
    sample_size_control_did_summary        = as.integer(data$n_control),
    sample_size_treatment_did_summary      = as.integer(data$n_treatment),
    sd_control_before_did_summary          = data$sd_pre_control,
    sd_control_after_did_summary           = data$sd_post_control,
    sd_treatment_before_did_summary        = data$sd_pre_treatment,
    sd_treatment_after_did_summary         = data$sd_post_treatment,
    is_differenced_likelihood_did_summary  = 0L,
    n_rho_known_did_summary                = rho_split$n_known,
    n_rho_missing_did_summary              = rho_split$n_missing,
    idx_rho_known_did_summary              = rho_split$idx_known,
    idx_rho_missing_did_summary            = rho_split$idx_missing,
    rho_known_did_summary                  = rho_split$rho_known
  )
}

adapt_summary_did_change <- function(data) {
  if (nrow(data) == 0) return(null_stan_data_did_change_only())
  list(
    n_studies_did_change_only              = nrow(data),
    x_bar_change_control_did_change_only   = data$mean_change_control,
    x_bar_change_treatment_did_change_only = data$mean_change_treatment,
    sample_size_control_did_change_only    = as.integer(data$n_control),
    sample_size_treatment_did_change_only  = as.integer(data$n_treatment),
    sd_change_control_did_change_only      = data$sd_change_control,
    sd_change_treatment_did_change_only    = data$sd_change_treatment
  )
}

adapt_summary_rct <- function(data) {
  if (nrow(data) == 0) return(null_stan_data_rct_summary())
  list(
    n_studies_rct_summary                           = nrow(data),
    x_bar_control_after_rct_summary                 = data$mean_post_control,
    x_bar_treatment_after_rct_summary               = data$mean_post_treatment,
    sample_size_control_rct_summary                 = as.integer(data$n_control),
    sample_size_treatment_rct_summary               = as.integer(data$n_treatment),
    sd_control_after_rct_summary                    = data$sd_post_control,
    sd_treatment_after_rct_summary                  = data$sd_post_treatment,
    is_time_trend_rct_summary_zero = 0L
  )
}

adapt_summary_pp <- function(data) {
  if (nrow(data) == 0) return(null_stan_data_pp_summary())
  rho <- if ("rho" %in% names(data)) data$rho else rep(NA_real_, nrow(data))
  rho_split <- split_rho(rho)
  list(
    n_studies_pp_summary                 = nrow(data),
    x_bar_treatment_before_pp_summary    = data$mean_pre_treatment,
    x_bar_treatment_after_pp_summary     = data$mean_post_treatment,
    sample_size_treatment_pp_summary     = as.integer(data$n_treatment),
    sd_treatment_before_pp_summary       = data$sd_pre_treatment,
    sd_treatment_after_pp_summary        = data$sd_post_treatment,
    is_time_trend_pp_summary_zero        = 0L,
    is_differenced_likelihood_pp_summary = 1L,
    n_rho_known_pp_summary               = rho_split$n_known,
    n_rho_missing_pp_summary             = rho_split$n_missing,
    idx_rho_known_pp_summary             = rho_split$idx_known,
    idx_rho_missing_pp_summary           = rho_split$idx_missing,
    rho_known_pp_summary                 = rho_split$rho_known
  )
}

# ---------------------------------------------------------------------------
# Individual-level prep functions (migrated from original project)
# ---------------------------------------------------------------------------

prepare_individual_did <- function(df) {
  if (nrow(df) == 0) return(null_stan_data_did())
  df <- dplyr::arrange(df, .data$study_id)
  n_studies <- dplyr::n_distinct(df$study_id)
  ctrl <- dplyr::filter(df, .data$type == "control")
  trt  <- dplyr::filter(df, .data$type == "treatment")
  n_c  <- dplyr::count(ctrl, .data$study_id)$n
  n_t  <- dplyr::count(trt,  .data$study_id)$n
  list(
    n_studies_did             = n_studies,
    sample_size_control_did   = as.integer(n_c),
    sample_size_treatment_did = as.integer(n_t),
    study_start_control_did   = as.integer(c(1L, cumsum(n_c[-length(n_c)]) + 1L)),
    study_end_control_did     = as.integer(cumsum(n_c)),
    study_start_treatment_did = as.integer(c(1L, cumsum(n_t[-length(n_t)]) + 1L)),
    study_end_treatment_did   = as.integer(cumsum(n_t)),
    x_control_before_did      = ctrl$before,
    x_control_after_did       = ctrl$after,
    x_treatment_before_did    = trt$before,
    x_treatment_after_did     = trt$after
  )
}

prepare_individual_rct <- function(df) {
  if (nrow(df) == 0) return(null_stan_data_rct())
  df <- dplyr::arrange(df, .data$study_id)
  n_studies <- dplyr::n_distinct(df$study_id)
  ctrl <- dplyr::filter(df, .data$type == "control")
  trt  <- dplyr::filter(df, .data$type == "treatment")
  n_c  <- dplyr::count(ctrl, .data$study_id)$n
  n_t  <- dplyr::count(trt,  .data$study_id)$n
  list(
    n_studies_rct               = n_studies,
    sample_size_control_rct     = as.integer(n_c),
    sample_size_treatment_rct   = as.integer(n_t),
    study_start_control_rct     = as.integer(c(1L, cumsum(n_c[-length(n_c)]) + 1L)),
    study_end_control_rct       = as.integer(cumsum(n_c)),
    study_start_treatment_rct   = as.integer(c(1L, cumsum(n_t[-length(n_t)]) + 1L)),
    study_end_treatment_rct     = as.integer(cumsum(n_t)),
    x_control_after_rct         = ctrl$after,
    x_treatment_after_rct       = trt$after,
    is_time_trend_rct_zero = 0L
  )
}

prepare_individual_pp <- function(df) {
  if (nrow(df) == 0) return(null_stan_data_pp())
  df <- dplyr::arrange(df, .data$study_id)
  n_studies <- dplyr::n_distinct(df$study_id)
  trt <- dplyr::filter(df, .data$type == "treatment")
  n_t <- dplyr::count(trt, .data$study_id)$n
  list(
    n_studies_pp               = n_studies,
    sample_size_treatment_pp   = as.integer(n_t),
    study_start_treatment_pp   = as.integer(c(1L, cumsum(n_t[-length(n_t)]) + 1L)),
    study_end_treatment_pp     = as.integer(cumsum(n_t)),
    x_treatment_before_pp      = trt$before,
    x_treatment_after_pp       = trt$after,
    is_time_trend_pp_zero       = 0L,
    is_differenced_likelihood_pp = 1L
  )
}

# ---------------------------------------------------------------------------
# Covariate matrix extraction helpers
# ---------------------------------------------------------------------------

# Extract a covariate matrix from a summary-level data subset.
# Returns a matrix with nrow(data) rows and length(covariate_names) columns.
.extract_cov_matrix_summary <- function(data, covariate_names) {
  if (nrow(data) == 0 || length(covariate_names) == 0) {
    return(matrix(0, nrow = nrow(data), ncol = length(covariate_names)))
  }
  as.matrix(data[, covariate_names, drop = FALSE])
}

# Extract a covariate matrix from individual-level data (one row per study).
# Covariates are constant within study, so take the first value per study.
.extract_cov_matrix_individual <- function(data, covariate_names) {
  if (nrow(data) == 0 || length(covariate_names) == 0) {
    n_studies <- if ("study_id" %in% names(data)) dplyr::n_distinct(data$study_id) else 0L
    return(matrix(0, nrow = n_studies, ncol = length(covariate_names)))
  }
  study_covs <- data |>
    dplyr::group_by(.data$study_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$study_id)
  as.matrix(study_covs[, covariate_names, drop = FALSE])
}

# ---------------------------------------------------------------------------
# prepare_stan_data()  -- main dispatcher
# ---------------------------------------------------------------------------

# Compute the upper bound for the wide uniform prior on the per-study
# latent baseline parameter in modelled modes.
#
# If the user has explicitly set a baseline_per_study uniform prior via
# set_priors(), that bound is used directly. Otherwise the bound is
# computed from the observed data as 100x the largest observed pre-period
# or contemporaneous-control mean across studies. Returns 1.0 as a benign
# fallback when no relevant data are present (e.g. when fitting in
# baseline_latent = "none" mode where the bound is unused but Stan still
# expects the variable to be declared).
compute_baseline_prior_upper <- function(summary_data, individual_data, user_prior = NULL) {

  # User override takes precedence
  if (!is.null(user_prior)) {
    if (!identical(user_prior$dist, "uniform")) {
      stop("baseline_per_study prior must be a uniform(...) distribution.",
           call. = FALSE)
    }
    return(as.numeric(user_prior$upper))
  }

  candidate_means <- c()

  if (!is.null(summary_data) && nrow(summary_data) > 0) {
    for (col in c("mean_pre_control", "mean_pre_treatment",
                  "mean_post_control", "mean_post_treatment")) {
      if (col %in% names(summary_data)) {
        candidate_means <- c(candidate_means, summary_data[[col]])
      }
    }
  }

  if (!is.null(individual_data) && nrow(individual_data) > 0) {
    # Use the maximum absolute group-time mean across studies as the scale.
    by_keys <- c("study_id", "design", "group", "time")
    has_cols <- all(by_keys %in% names(individual_data))
    if (has_cols) {
      ind_means <- individual_data |>
        dplyr::group_by(.data$study_id, .data$design, .data$group, .data$time) |>
        dplyr::summarise(m = mean(.data$value, na.rm = TRUE), .groups = "drop")
      candidate_means <- c(candidate_means, ind_means$m)
    }
  }

  candidate_means <- candidate_means[is.finite(candidate_means)]

  if (length(candidate_means) == 0) {
    return(1.0)
  }

  data_scale <- max(abs(candidate_means), na.rm = TRUE)
  if (!is.finite(data_scale) || data_scale <= 0) {
    return(1.0)
  }

  100 * data_scale
}


prepare_stan_data <- function(summary_data, individual_data, model_flags, priors,
                              covariate_names = NULL,
                              multiplicative_covariate = NULL,
                              center_covariates = TRUE) {

  K_cov <- length(covariate_names)
  has_mult <- as.integer(!is.null(multiplicative_covariate))

  # --- Compute centering values across all studies ---
  # Only additive covariates are centered. The multiplicative covariate is
  # never centered (a 0/1 indicator shifted to mean -0.4 has no clean
  # multiplicative interpretation: gamma^{-0.4} is not what the user wants).
  cov_centers <- NULL
  if (K_cov > 0 && center_covariates) {
    # Collect one row per study from both data sources
    cov_vals <- list()
    if (!is.null(summary_data) && nrow(summary_data) > 0) {
      cov_vals[[1]] <- summary_data[, covariate_names, drop = FALSE]
    }
    if (!is.null(individual_data) && nrow(individual_data) > 0) {
      ind_collapsed <- individual_data |>
        dplyr::group_by(.data$study_id) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
      cov_vals[[2]] <- ind_collapsed[, covariate_names, drop = FALSE]
    }
    all_covs <- do.call(rbind, cov_vals)
    cov_centers <- colMeans(all_covs)

    # Center the data in place
    if (!is.null(summary_data) && nrow(summary_data) > 0) {
      for (col in covariate_names) {
        summary_data[[col]] <- summary_data[[col]] - cov_centers[[col]]
      }
    }
    if (!is.null(individual_data) && nrow(individual_data) > 0) {
      for (col in covariate_names) {
        individual_data[[col]] <- individual_data[[col]] - cov_centers[[col]]
      }
    }
  }

  # --- Summary-level path ---
  sum_did        <- filter_design(summary_data, "did")
  sum_did_change <- filter_design(summary_data, "did_change")
  sum_rct        <- filter_design(summary_data, "rct")
  sum_pp         <- filter_design(summary_data, "pp")

  stan_did_summary        <- adapt_summary_did(sum_did)
  stan_did_change_only    <- adapt_summary_did_change(sum_did_change)
  stan_rct_summary        <- adapt_summary_rct(sum_rct)
  stan_pp_summary         <- adapt_summary_pp(sum_pp)

  # --- Individual-level path ---
  ind_did_raw <- filter_design(individual_data, "did")
  ind_rct_raw <- filter_design(individual_data, "rct")
  ind_pp_raw  <- filter_design(individual_data, "pp")

  ind_did <- adapt_individual(ind_did_raw)
  ind_rct <- adapt_individual(ind_rct_raw)
  ind_pp  <- adapt_individual(ind_pp_raw)

  stan_did <- prepare_individual_did(ind_did)
  stan_rct <- prepare_individual_rct(ind_rct)
  stan_pp  <- prepare_individual_pp(ind_pp)

  # --- Covariate matrices ---
  cov_names <- if (K_cov > 0) covariate_names else character(0)

  # Summary-level covariate matrices
  stan_did_summary$X_cov_did_summary       <- .extract_cov_matrix_summary(sum_did, cov_names)
  stan_did_change_only$X_cov_did_change_only <- .extract_cov_matrix_summary(sum_did_change, cov_names)
  stan_rct_summary$X_cov_rct_summary       <- .extract_cov_matrix_summary(sum_rct, cov_names)
  stan_pp_summary$X_cov_pp_summary         <- .extract_cov_matrix_summary(sum_pp, cov_names)

  # Individual-level covariate matrices (one row per study)
  stan_did$X_cov_did <- .extract_cov_matrix_individual(ind_did_raw, cov_names)
  stan_rct$X_cov_rct <- .extract_cov_matrix_individual(ind_rct_raw, cov_names)
  stan_pp$X_cov_pp   <- .extract_cov_matrix_individual(ind_pp_raw, cov_names)

  # --- Multiplicative covariate per-design vectors ---
  # Each design carries an integer code vector (one entry per study) giving the
  # 0-based index of each study's level in the joint level set; the reference
  # level is 0. Zero-filled when the feature is off (n_effect_multipliers = 0),
  # which the Stan model ignores via mult_factor(). Level coding is computed
  # jointly across every design frame so a code means the same level everywhere
  # (see .compute_mult_levels()).
  mult_levels <- .compute_mult_levels(
    multiplicative_covariate,
    summary_frames    = list(sum_did, sum_did_change, sum_rct, sum_pp),
    individual_frames = list(ind_did_raw, ind_rct_raw, ind_pp_raw)
  )
  n_mult <- if (length(mult_levels) >= 2L) length(mult_levels) - 1L else 0L

  stan_did_summary$x_mult_did_summary             <- .extract_mult_vec_summary(sum_did,        multiplicative_covariate, mult_levels)
  stan_did_change_only$x_mult_did_change_only     <- .extract_mult_vec_summary(sum_did_change, multiplicative_covariate, mult_levels)
  stan_rct_summary$x_mult_rct_summary             <- .extract_mult_vec_summary(sum_rct,        multiplicative_covariate, mult_levels)
  stan_pp_summary$x_mult_pp_summary               <- .extract_mult_vec_summary(sum_pp,         multiplicative_covariate, mult_levels)

  stan_did$x_mult_did <- .extract_mult_vec_individual(ind_did_raw, multiplicative_covariate, mult_levels)
  stan_rct$x_mult_rct <- .extract_mult_vec_individual(ind_rct_raw, multiplicative_covariate, mult_levels)
  stan_pp$x_mult_pp   <- .extract_mult_vec_individual(ind_pp_raw,  multiplicative_covariate, mult_levels)

  # --- Baseline-latent prior upper bound ---
  # In modelled modes the per-study baseline has a wide uniform prior. The
  # upper bound is set to a large multiple of the observed baseline scale
  # (control-pre / treatment-pre / control-post means across all studies)
  # so the prior is data-vague but proper. Users can override the bound
  # via set_priors(baseline_per_study = uniform(0, X)).
  baseline_prior_upper <- compute_baseline_prior_upper(
    summary_data    = summary_data,
    individual_data = individual_data,
    user_prior      = priors$baseline_per_study
  )

  # --- Shared data: flags + prior hyperparameters + covariate info ---
  shared <- c(
    model_flags,
    as_stan_data(priors),
    list(
      K_cov                        = K_cov,
      n_effect_multipliers         = n_mult,
      baseline_prior_upper         = baseline_prior_upper
    )
  )

  # --- Combine ---
  result <- c(
    shared,
    stan_did,
    stan_rct,
    stan_pp,
    stan_did_summary,
    stan_rct_summary,
    stan_pp_summary,
    stan_did_change_only
  )

  attr(result, "cov_centers") <- cov_centers
  attr(result, "multiplier_levels") <- if (has_mult == 1L) mult_levels else NULL
  result
}

# ---------------------------------------------------------------------------
# Multiplicative-covariate level coding helpers
#
# The covariate is categorical. Levels are determined jointly across every
# design data frame (see .compute_mult_levels()), so an integer code means the
# same level everywhere; each per-design frame is then coded against that shared
# level set (.extract_mult_vec_*()). The first level is the reference (Stan
# code 0, factor fixed at 1); a study at level k > 0 selects
# effect_multiplier[k].
# ---------------------------------------------------------------------------

#' First value of a column per study
#'
#' Collapses an individual-level data frame to one row per `study_id` and
#' returns the requested column. The multiplicative covariate is constant within
#' study (enforced by [validate_multiplicative_covariate()]), so the first value
#' represents the whole study.
#'
#' @param data Individual-level data frame, or `NULL`.
#' @param column Name of the column to extract.
#' @return The column vector with one entry per distinct `study_id`, in
#'   first-appearance order. `character(0)` for `NULL` or empty input.
#' @keywords internal
#' @noRd
.study_first_value <- function(data, column) {
  if (is.null(data) || nrow(data) == 0) return(character(0))
  data[!duplicated(data$study_id), , drop = FALSE][[column]]
}

#' Derive globally consistent levels for the multiplicative covariate
#'
#' Levels are determined jointly across every data frame that carries the
#' covariate, so the same integer code means the same level everywhere.
#'
#' @param values_list List of covariate-column vectors, one per design/data
#'   source. Zero-length entries (designs with no studies) are ignored.
#' @details Ordering depends on the input type:
#'   \itemize{
#'     \item factor: the declared level order is respected (declare identical
#'       levels in every frame; unused declared levels are dropped, conflicting
#'       level sets/orders error, and observed values outside the declared
#'       levels error);
#'     \item numeric: levels sort in ascending numeric order, so a `{0, 1}`
#'       column codes 0 to the reference and 1 to the single non-reference level;
#'     \item character / logical: levels sort alphabetically (`FALSE` before
#'       `TRUE`).
#'   }
#' @return Ordered character vector of distinct levels. The first element is the
#'   reference (Stan code 0, factor fixed at 1); a study at level k > 0 selects
#'   `effect_multiplier[k]`. `character(0)` when no frame carries values.
#' @keywords internal
#' @noRd
.mult_levels <- function(values_list) {
  vals <- values_list[vapply(values_list, length, integer(1)) > 0]
  if (length(vals) == 0) return(character(0))
  observed <- unique(as.character(unlist(lapply(vals, as.character))))
  is_fac <- vapply(vals, is.factor, logical(1))
  if (any(is_fac)) {
    lv_sets <- unique(lapply(vals[is_fac], levels))
    if (length(lv_sets) > 1) {
      stop("Multiplicative covariate factors declare different level ",
           "sets/orders across data frames; declare identical factor ",
           "levels everywhere.", call. = FALSE)
    }
    declared <- lv_sets[[1]]
    extra <- setdiff(observed, declared)
    if (length(extra) > 0) {
      stop("Multiplicative covariate contains values not among the declared ",
           "factor levels: ", paste(extra, collapse = ", "), ".",
           call. = FALSE)
    }
    declared[declared %in% observed]
  } else if (all(vapply(vals, is.numeric, logical(1)))) {
    as.character(sort(unique(as.numeric(unlist(vals)))))
  } else {
    sort(observed)
  }
}

#' Compute the joint level set for the multiplicative covariate
#'
#' Collects the covariate column from every per-design data frame (summary
#' frames are already one row per study; individual frames are collapsed via
#' `.study_first_value()`) and delegates to `.mult_levels()` to derive a single
#' globally consistent, ordered set of levels.
#'
#' @param multiplicative_covariate Column name (length-1 character), or `NULL`
#'   when the feature is off.
#' @param summary_frames List of summary-level design frames (one row per study).
#' @param individual_frames List of individual-level design frames (multiple rows
#'   per study).
#' @return Ordered character vector of distinct levels, reference first (Stan
#'   code 0). `character(0)` when `multiplicative_covariate` is `NULL`. Errors if
#'   the covariate takes fewer than two distinct values across all studies (the
#'   multiplier and the population mean are then jointly unidentified).
#' @keywords internal
#' @noRd
.compute_mult_levels <- function(multiplicative_covariate,
                                 summary_frames, individual_frames) {
  if (is.null(multiplicative_covariate)) return(character(0))
  summary_vals    <- lapply(summary_frames,
                            function(d) d[[multiplicative_covariate]])
  individual_vals <- lapply(individual_frames, .study_first_value,
                            column = multiplicative_covariate)
  mult_levels <- .mult_levels(c(summary_vals, individual_vals))
  if (length(mult_levels) < 2L) {
    stop("Multiplicative covariate '", multiplicative_covariate,
         "' must take at least two distinct values across studies.",
         call. = FALSE)
  }
  mult_levels
}

#' Integer level codes for a summary-level design frame
#'
#' @param data Summary-level data frame (one row per study).
#' @param multiplicative_covariate Column name, or `NULL` when the feature is off.
#' @param mult_levels Joint level vector from `.mult_levels()`.
#' @return Integer array (length `nrow(data)`); each entry is the 0-based index
#'   of that study's level in `mult_levels` (reference = 0). An all-zero array
#'   when the feature is off or the frame is empty.
#' @keywords internal
#' @noRd
.extract_mult_vec_summary <- function(data, multiplicative_covariate,
                                      mult_levels = character(0)) {
  n <- nrow(data)
  if (is.null(multiplicative_covariate) || n == 0) {
    return(as.array(rep(0L, n)))
  }
  as.array(match(as.character(data[[multiplicative_covariate]]), mult_levels) - 1L)
}

#' Integer level codes for an individual-level design frame
#'
#' @param data Individual-level data frame (multiple rows per study).
#' @param multiplicative_covariate Column name, or `NULL` when the feature is off.
#' @param mult_levels Joint level vector from `.mult_levels()`.
#' @return Integer array with one entry per distinct `study_id`, the 0-based
#'   level index into `mult_levels` (reference = 0). `integer(0)` array when the
#'   frame is empty; an all-zero array when the feature is off.
#' @keywords internal
#' @noRd
.extract_mult_vec_individual <- function(data, multiplicative_covariate,
                                         mult_levels = character(0)) {
  if (is.null(data) || nrow(data) == 0) return(as.array(integer(0)))
  n_studies <- length(unique(data$study_id))
  if (is.null(multiplicative_covariate)) {
    return(as.array(rep(0L, n_studies)))
  }
  study_vals <- .study_first_value(data, multiplicative_covariate)
  as.array(match(as.character(study_vals), mult_levels) - 1L)
}
