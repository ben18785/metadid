# Functions for simulating meta-analysis datasets from known parameters.
#
# The design reflects the underlying scientific assumption: all study designs
# are views of the same individual-level difference-in-differences process.
# Some studies provide full access to individual observations (the gold
# standard); others provide only summary statistics or a subset of the arms
# and time points.
#
# Workflow:
#   1. simulate_meta_did()      -- simulate individual-level DiD data
#   2a. as_individual_did()     -- full individual data     (individual_data)
#       as_individual_rct()     -- post-only individual     (individual_data)
#       as_individual_pp()      -- treatment-arm individual  (individual_data)
#   2b. as_summary_did()        -- 4-cell summary + rho     (summary_data)
#       as_summary_rct()        -- post-only summary         (summary_data)
#       as_summary_pp()         -- treatment-arm summary     (summary_data)
#       as_summary_did_change() -- change-score summary      (summary_data)

# ---------------------------------------------------------------------------
# simulate_meta_did()
# ---------------------------------------------------------------------------

#' Simulate individual-level DiD data for a collection of studies
#'
#' Generates individual-level pre/post observations for both arms of a set of
#' difference-in-differences studies under a hierarchical model. This is the
#' gold-standard view of the data; the `as_*` family of functions then produce
#' the data as it would appear under different levels of access.
#'
#' @section Data-generating model:
#' Study-level parameters are drawn hierarchically:
#' \deqn{\theta_i \sim \text{Normal}(\texttt{true\_effect},\, \texttt{sigma\_effect}^2)}
#' \deqn{\gamma_i \sim \text{Normal}(\texttt{true\_trend},\, \texttt{sigma\_trend}^2)}
#' \deqn{b_i      \sim \text{Normal}(\texttt{baseline\_mean},\, \texttt{baseline\_sd}^2)}
#'
#' Within each study, individual (pre, post) pairs are drawn from a bivariate
#' normal distribution with covariance matrix
#' \deqn{\Sigma = \sigma^2 \begin{pmatrix} 1 & \rho \\ \rho & 1 \end{pmatrix}}
#' making the role of \eqn{\rho} as the pre-post correlation explicit. The mean
#' vector for the control group is \eqn{(b_i,\, b_i + \gamma_i)} and for the
#' treatment group \eqn{(b_i,\, b_i + \gamma_i + \theta_i)}.
#'
#' @param n_studies Number of studies. Default `20`.
#' @param n_control Number of individuals per study in the control arm.
#'   Default `100L`.
#' @param n_treatment Number of individuals per study in the treatment arm.
#'   Default `100L`.
#' @param true_effect Population mean treatment effect. Default `-0.15`.
#' @param sigma_effect Between-study SD of treatment effects. Default `0.03`.
#' @param true_trend Population mean time trend. Default `-0.02`.
#' @param sigma_trend Between-study SD of time trends. Default `0`.
#' @param baseline_mean Population mean of study baselines. Default `0.45`.
#' @param baseline_sd Between-study SD of baselines. Default `0`.
#' @param within_sd Individual-level SD (the same for pre and post and for
#'   both groups). Default `0.12`.
#' @param rho Pre-post correlation within individuals. Directly parameterises
#'   the off-diagonal of the bivariate normal covariance matrix. Default `0.5`.
#' @param seed Integer random seed for reproducibility. Default `NULL`.
#'
#' @return A data frame with columns `study_id`, `subject_id`, `group`
#'   (`"control"` or `"treatment"`), `time` (`"pre"` or `"post"`), `value`.
#'   The true study-level parameters are attached as attribute `"true_params"`.
#'
#' @seealso [as_individual_did()], [as_individual_rct()], [as_individual_pp()],
#'   [as_summary_did()], [as_summary_rct()], [as_summary_pp()],
#'   [as_summary_did_change()]
#' @export
#'
#' @examples
#' dat <- simulate_meta_did(n_studies = 10, true_effect = -0.15, seed = 42)
#' head(dat)
#' attr(dat, "true_params")
simulate_meta_did <- function(
  n_studies     = 20L,
  n_control     = 100L,
  n_treatment   = 100L,
  true_effect   = -0.15,
  sigma_effect  = 0.03,
  true_trend    = -0.02,
  sigma_trend   = 0,
  baseline_mean = 0.45,
  baseline_sd   = 0,
  within_sd     = 0.12,
  rho           = 0.5,
  seed          = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  # Covariance matrix: same for both groups, pre-post corr = rho
  Sigma <- within_sd^2 * matrix(c(1, rho, rho, 1), 2, 2)

  # Study-level parameters
  params <- tibble::tibble(
    study_id = paste0("study_", seq_len(n_studies)),
    theta    = stats::rnorm(n_studies, true_effect,   sigma_effect),
    gamma    = stats::rnorm(n_studies, true_trend,    sigma_trend),
    baseline = stats::rnorm(n_studies, baseline_mean, baseline_sd)
  )

  obs <- purrr::pmap_dfr(params, function(study_id, theta, gamma, baseline) {
    mu_ctrl <- c(baseline, baseline + gamma)
    mu_trt  <- c(baseline, baseline + gamma + theta)

    ctrl <- .rbvnorm(n_control,   mu_ctrl, Sigma)
    trt  <- .rbvnorm(n_treatment, mu_trt,  Sigma)

    dplyr::bind_rows(
      tibble::tibble(study_id = study_id,
                     subject_id = seq_len(n_control),
                     group = "control",
                     pre = ctrl[, 1], post = ctrl[, 2]),
      tibble::tibble(study_id = study_id,
                     subject_id = seq_len(n_treatment),
                     group = "treatment",
                     pre = trt[, 1], post = trt[, 2])
    ) |>
      tidyr::pivot_longer(c("pre", "post"), names_to = "time", values_to = "value")
  })

  attr(obs, "true_params") <- params
  obs
}

# ---------------------------------------------------------------------------
# Individual-level extractors
# ---------------------------------------------------------------------------

#' Extract full individual-level DiD data for use with meta_did()
#'
#' Returns the output of [simulate_meta_did()] reformatted as the `individual_data`
#' argument of [meta_did()]: one row per observation with columns
#' `study_id`, `design`, `group`, `time`, `value`.
#'
#' @param sim A data frame produced by [simulate_meta_did()].
#' @return A data frame in the format required by [validate_individual_data()]
#'   for design `"did"`.
#' @export
#' @examples
#' sim <- sim_meta(n_studies = 3, seed = 1)
#' head(as_individual_did(sim))
as_individual_did <- function(sim) {
  dplyr::mutate(sim, design = "did") |>
    dplyr::select("study_id", "design", "group", "time", "value")
}

#' Extract post-only individual-level RCT data for use with meta_did()
#'
#' Discards all pre-treatment observations and returns only post-treatment rows
#' for both arms. Mimics a study in which only post-treatment measurements were
#' taken.
#'
#' @param sim A data frame produced by [simulate_meta_did()].
#' @return A data frame in the format required by [validate_individual_data()]
#'   for design `"rct"`.
#' @export
#' @examples
#' sim <- sim_meta(n_studies = 3, seed = 1)
#' head(as_individual_rct(sim))
as_individual_rct <- function(sim) {
  sim |>
    dplyr::filter(.data$time == "post") |>
    dplyr::mutate(design = "rct") |>
    dplyr::select("study_id", "design", "group", "time", "value")
}

#' Extract treatment-arm individual-level pre-post data for use with meta_did()
#'
#' Discards all control-arm observations and returns treatment-arm rows (both
#' pre and post). Mimics a study with no control group.
#'
#' @param sim A data frame produced by [simulate_meta_did()].
#' @return A data frame in the format required by [validate_individual_data()]
#'   for design `"pp"`.
#' @export
#' @examples
#' sim <- sim_meta(n_studies = 3, seed = 1)
#' head(as_individual_pp(sim))
as_individual_pp <- function(sim) {
  sim |>
    dplyr::filter(.data$group == "treatment") |>
    dplyr::mutate(design = "pp") |>
    dplyr::select("study_id", "design", "group", "time", "value")
}

# ---------------------------------------------------------------------------
# Summary-level extractors
# ---------------------------------------------------------------------------

#' Summarise simulated DiD data to study-level statistics
#'
#' Aggregates individual-level data from [simulate_meta_did()] to the four-cell summary
#' (pre/post × control/treatment) required by [meta_did()]. The pre-post
#' correlation \eqn{\rho} is estimated empirically from matched pairs within
#' each arm and then averaged.
#'
#' @param sim A data frame produced by [simulate_meta_did()].
#' @return A data frame with one row per study in the format required by
#'   [validate_summary_data()] for design `"did"`.
#' @export
#' @examples
#' sim <- sim_meta(n_studies = 3, seed = 1)
#' as_summary_did(sim)
as_summary_did <- function(sim) {
  cell_stats <- sim |>
    dplyr::group_by(.data$study_id, .data$group, .data$time) |>
    dplyr::summarise(
      n    = dplyr::n(),
      mean = mean(.data$value),
      sd   = stats::sd(.data$value),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from  = c("group", "time"),
      values_from = c("n", "mean", "sd")
    )

  rho_est <- sim |>
    dplyr::group_by(.data$study_id, .data$group) |>
    dplyr::summarise(rho = .within_arm_rho(dplyr::pick(dplyr::everything())),
                     .groups = "drop") |>
    dplyr::group_by(.data$study_id) |>
    dplyr::summarise(rho = mean(.data$rho, na.rm = TRUE), .groups = "drop")

  cell_stats |>
    dplyr::left_join(rho_est, by = "study_id") |>
    dplyr::transmute(
      study_id            = .data$study_id,
      design              = "did",
      n_control           = as.integer(.data$n_control_pre),
      n_treatment         = as.integer(.data$n_treatment_pre),
      mean_pre_control    = .data$mean_control_pre,
      mean_post_control   = .data$mean_control_post,
      sd_pre_control      = .data$sd_control_pre,
      sd_post_control     = .data$sd_control_post,
      mean_pre_treatment  = .data$mean_treatment_pre,
      mean_post_treatment = .data$mean_treatment_post,
      sd_pre_treatment    = .data$sd_treatment_pre,
      sd_post_treatment   = .data$sd_treatment_post,
      rho                 = .data$rho
    )
}

#' Summarise simulated DiD data as RCT-style post-only statistics
#'
#' Discards all pre-treatment observations and returns post-treatment means and
#' SDs for both arms. Produces data in the format expected by [meta_did()] for
#' design `"rct"`.
#'
#' @param sim A data frame produced by [simulate_meta_did()].
#' @return A data frame with one row per study in the format required by
#'   [validate_summary_data()] for design `"rct"`.
#' @export
#' @examples
#' sim <- sim_meta(n_studies = 3, seed = 1)
#' as_summary_rct(sim)
as_summary_rct <- function(sim) {
  sim |>
    dplyr::filter(.data$time == "post") |>
    dplyr::group_by(.data$study_id, .data$group) |>
    dplyr::summarise(
      n    = dplyr::n(),
      mean = mean(.data$value),
      sd   = stats::sd(.data$value),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = "group", values_from = c("n", "mean", "sd")) |>
    dplyr::transmute(
      study_id            = .data$study_id,
      design              = "rct",
      n_control           = as.integer(.data$n_control),
      n_treatment         = as.integer(.data$n_treatment),
      mean_post_control   = .data$mean_control,
      sd_post_control     = .data$sd_control,
      mean_post_treatment = .data$mean_treatment,
      sd_post_treatment   = .data$sd_treatment
    )
}

#' Summarise simulated DiD data as pre-post (treatment arm only) statistics
#'
#' Discards all control-arm observations and returns pre/post means and SDs for
#' the treatment arm. Produces data in the format expected by [meta_did()] for
#' design `"pp"`.
#'
#' @param sim A data frame produced by [simulate_meta_did()].
#' @return A data frame with one row per study in the format required by
#'   [validate_summary_data()] for design `"pp"`.
#' @export
#' @examples
#' sim <- sim_meta(n_studies = 3, seed = 1)
#' as_summary_pp(sim)
as_summary_pp <- function(sim) {
  trt <- dplyr::filter(sim, .data$group == "treatment")

  cell_stats <- trt |>
    dplyr::group_by(.data$study_id, .data$time) |>
    dplyr::summarise(
      n    = dplyr::n(),
      mean = mean(.data$value),
      sd   = stats::sd(.data$value),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = "time", values_from = c("n", "mean", "sd"))

  rho_est <- trt |>
    dplyr::group_by(.data$study_id) |>
    dplyr::summarise(rho = .within_arm_rho(dplyr::pick(dplyr::everything())),
                     .groups = "drop")

  cell_stats |>
    dplyr::left_join(rho_est, by = "study_id") |>
    dplyr::transmute(
      study_id            = .data$study_id,
      design              = "pp",
      n_treatment         = as.integer(.data$n_pre),
      mean_pre_treatment  = .data$mean_pre,
      sd_pre_treatment    = .data$sd_pre,
      mean_post_treatment = .data$mean_post,
      sd_post_treatment   = .data$sd_post,
      rho                 = .data$rho
    )
}

#' Summarise simulated DiD data as change-score statistics
#'
#' Computes within-individual change scores (post minus pre) matched on
#' `subject_id`, then returns means and SDs for each arm. Produces data in the
#' format expected by [meta_did()] for design `"did_change"`.
#'
#' @param sim A data frame produced by [simulate_meta_did()].
#' @return A data frame with one row per study in the format required by
#'   [validate_summary_data()] for design `"did_change"`.
#' @export
#' @examples
#' sim <- sim_meta(n_studies = 3, seed = 1)
#' as_summary_did_change(sim)
as_summary_did_change <- function(sim) {
  sim |>
    tidyr::pivot_wider(
      id_cols     = c("study_id", "subject_id", "group"),
      names_from  = "time",
      values_from = "value"
    ) |>
    dplyr::mutate(change = .data$post - .data$pre) |>
    dplyr::group_by(.data$study_id, .data$group) |>
    dplyr::summarise(
      n      = dplyr::n(),
      mean   = mean(.data$change),
      sd     = stats::sd(.data$change),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = "group", values_from = c("n", "mean", "sd")) |>
    dplyr::transmute(
      study_id              = .data$study_id,
      design                = "did_change",
      n_control             = as.integer(.data$n_control),
      n_treatment           = as.integer(.data$n_treatment),
      mean_change_control   = .data$mean_control,
      sd_change_control     = .data$sd_control,
      mean_change_treatment = .data$mean_treatment,
      sd_change_treatment   = .data$sd_treatment
    )
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Draw n observations from a bivariate normal via Cholesky decomposition.
# Returns an n x 2 matrix of (pre, post) pairs.
.rbvnorm <- function(n, mu, Sigma) {
  L <- t(chol(Sigma))
  z <- matrix(stats::rnorm(2 * n), nrow = n, ncol = 2)
  sweep(z %*% t(L), 2, mu, "+")
}

# Empirical pre-post Pearson correlation within one arm, matched on subject_id.
.within_arm_rho <- function(arm_data) {
  if (!"subject_id" %in% names(arm_data)) return(NA_real_)
  wide <- arm_data |>
    tidyr::pivot_wider(
      id_cols     = "subject_id",
      names_from  = "time",
      values_from = "value"
    )
  if (nrow(wide) < 3 || !all(c("pre", "post") %in% names(wide))) return(NA_real_)
  stats::cor(wide$pre, wide$post)
}
