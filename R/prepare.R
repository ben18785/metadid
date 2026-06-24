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

prepare_stan_data <- function(summary_data, individual_data, model_flags, priors,
                              covariate_names = NULL, center_covariates = TRUE) {

  K_cov <- length(covariate_names)

  # --- Compute centering values across all studies ---
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

  # --- Shared data: flags + prior hyperparameters + covariate info ---
  shared <- c(model_flags, as_stan_data(priors), list(K_cov = K_cov))

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
  result
}
