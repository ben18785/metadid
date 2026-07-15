# Prior distribution constructors and the set_priors() interface.
#
# Each constructor returns a small S3 object of class "did_prior".
# set_priors() collects these into a "did_priors" list with defaults.
# as_stan_data() translates the list into Stan data hyperparameters.

# ---------------------------------------------------------------------------
# Constructors
# ---------------------------------------------------------------------------

#' Specify a normal prior
#'
#' @param mean Prior mean.
#' @param sd Prior standard deviation (must be positive).
#' @return A `did_prior` object.
#' @export
normal <- function(mean, sd) {
  stopifnot(is.numeric(mean), length(mean) == 1)
  stopifnot(is.numeric(sd), length(sd) == 1, sd > 0)
  structure(list(dist = "normal", mean = mean, sd = sd), class = "did_prior")
}

#' Specify a log-normal prior
#'
#' Used for strictly positive multiplicative factors (the
#' `multiplicative_covariate` effect multiplier). The prior is placed on the
#' natural logarithm of the parameter, so the parameter itself is log-normally
#' distributed with median `exp(meanlog)` and strictly positive support. Unlike
#' a normal prior truncated at zero, a log-normal has no probability mass piling
#' up against a hard boundary and is the conventional choice for a ratio-scale
#' quantity.
#'
#' @param meanlog Mean on the log scale. `meanlog = 0` gives a median of 1.
#' @param sdlog Standard deviation on the log scale (must be positive). Larger
#'   values allow the factor to depart further from 1 in either direction.
#' @return A `did_prior` object.
#' @export
lognormal <- function(meanlog, sdlog) {
  stopifnot(is.numeric(meanlog), length(meanlog) == 1)
  stopifnot(is.numeric(sdlog), length(sdlog) == 1, sdlog > 0)
  structure(list(dist = "lognormal", meanlog = meanlog, sdlog = sdlog),
            class = "did_prior")
}

#' Specify a half-Cauchy prior
#'
#' Used for scale parameters. The distribution is implicitly half-Cauchy
#' (positive support only) when applied to a `<lower=0>` Stan parameter.
#'
#' @param scale Scale parameter (must be positive).
#' @return A `did_prior` object.
#' @export
cauchy <- function(scale) {
  stopifnot(is.numeric(scale), length(scale) == 1, scale > 0)
  structure(list(dist = "cauchy", scale = scale), class = "did_prior")
}

#' Specify a gamma prior
#'
#' Used for the degrees-of-freedom parameter when `robust_heterogeneity = TRUE`.
#'
#' @param shape Shape parameter (must be positive).
#' @param rate Rate parameter (must be positive).
#' @return A `did_prior` object.
#' @export
gamma <- function(shape, rate) {
  stopifnot(is.numeric(shape), length(shape) == 1, shape > 0)
  stopifnot(is.numeric(rate),  length(rate)  == 1, rate  > 0)
  structure(list(dist = "gamma", shape = shape, rate = rate), class = "did_prior")
}

#' Specify an LKJ prior
#'
#' Used for the correlation between treatment effects and time trends when
#' `correlated_effects = TRUE`. The LKJ distribution with concentration
#' parameter `eta` is placed on the Cholesky factor of the correlation matrix.
#' `eta = 1` is uniform over correlation matrices; `eta = 2` gently
#' regularises toward zero correlation.
#'
#' @param eta Concentration parameter (must be positive).
#' @return A `did_prior` object.
#' @export
lkj <- function(eta) {
  stopifnot(is.numeric(eta), length(eta) == 1, eta > 0)
  structure(list(dist = "lkj", eta = eta), class = "did_prior")
}

#' Specify a uniform prior
#'
#' Used for the per-study latent baseline parameter in modelled normalisation
#' modes (`baseline_latent = "treatment"` or `"control"`). Bounds must be
#' non-negative and `lower < upper`. If no `baseline_per_study` prior is
#' supplied to [set_priors()], the upper bound is computed automatically
#' from the observed baseline scale (100x the maximum observed pre-period
#' or contemporaneous-control mean across studies).
#'
#' @param lower Lower bound (must be non-negative; default 0).
#' @param upper Upper bound (must be positive and strictly greater than `lower`).
#' @return A `did_prior` object.
#' @export
uniform <- function(lower = 0, upper) {
  stopifnot(is.numeric(lower), length(lower) == 1, lower >= 0)
  stopifnot(is.numeric(upper), length(upper) == 1, upper > lower)
  structure(list(dist = "uniform", lower = lower, upper = upper),
            class = "did_prior")
}

#' @export
print.did_prior <- function(x, ...) {
  params <- switch(x$dist,
    normal    = paste0("mean = ", x$mean, ", sd = ", x$sd),
    lognormal = paste0("meanlog = ", x$meanlog, ", sdlog = ", x$sdlog),
    cauchy    = paste0("scale = ", x$scale),
    gamma     = paste0("shape = ", x$shape, ", rate = ", x$rate),
    lkj       = paste0("eta = ", x$eta),
    uniform   = paste0("lower = ", x$lower, ", upper = ", x$upper)
  )
  cat(x$dist, "(", params, ")\n", sep = "")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Allowed families per parameter (used by validate_priors)
# ---------------------------------------------------------------------------

.allowed_families <- list(
  treatment_effect_mean    = c("normal"),
  treatment_effect_sd      = c("cauchy", "normal"),
  time_trend_mean          = c("normal"),
  time_trend_sd            = c("cauchy", "normal"),
  rho_mean                 = c("normal"),
  rho_sd                   = c("normal"),
  nu                       = c("gamma"),
  delta_rct                = c("normal"),
  delta_pp                 = c("normal"),
  sigma                    = c("cauchy"),
  beta_cov                 = c("normal"),
  multiplier               = c("lognormal"),
  lkj_eta                  = c("lkj"),
  baseline_difference_mean = c("normal"),
  baseline_difference_sd   = c("cauchy", "normal"),
  baseline_per_study       = c("uniform")
)

# ---------------------------------------------------------------------------
# set_priors()
# ---------------------------------------------------------------------------

#' Set prior distributions for the meta-analysis model
#'
#' Construct a `did_priors` object specifying the prior distribution for each
#' population-level parameter. Any parameter not supplied takes a default.
#'
#' @param treatment_effect_mean Prior on the population treatment effect mean.
#'   Default: `normal(0, 10)`.
#' @param treatment_effect_sd Prior on the between-study SD. Default: `cauchy(5)`.
#' @param time_trend_mean Prior on the population time-trend mean. Default: `normal(0, 10)`.
#' @param time_trend_sd Prior on the between-study time-trend SD. Default: `cauchy(5)`.
#' @param rho_mean Prior on the mean of the Fisher-z transformed pre-post
#'   correlation (only used when `hierarchical_rho = TRUE`). Default: `normal(0, 1)`.
#' @param rho_sd Prior on the SD of the Fisher-z transformed pre-post correlation
#'   (only used when `hierarchical_rho = TRUE`). Default: `normal(0, 0.5)`.
#' @param nu Prior on the degrees of freedom for between-study heterogeneity
#'   (only used when `robust_heterogeneity = TRUE`). Default: `gamma(2, 0.1)`.
#' @param delta_rct Prior on the RCT design offset relative to DiD
#'   (only used when `design_effects = TRUE`). Default: `normal(0, 10)`.
#' @param delta_pp Prior on the Pre-Post design offset relative to DiD
#'   (only used when `design_effects = TRUE`). Default: `normal(0, 10)`.
#' @param sigma Prior on the study-level observation standard deviations
#'   (shared across all designs). Default: `cauchy(5)`.
#' @param beta_cov Prior on the covariate regression coefficients
#'   (only used when `covariates` is specified in [meta_did()]).
#'   Default: `normal(0, 10)`.
#' @param lkj_eta Prior on the Cholesky factor of the correlation matrix
#'   between treatment effects and time trends (only used when
#'   `correlated_effects = TRUE`). Default: `lkj(2)`, which gently
#'   regularises toward zero correlation.
#' @param baseline_difference_mean Prior on the population mean of the
#'   per-study baseline imbalance \eqn{\delta_i = (b_{T,i} - b_{C,i}) / b_{C,i}},
#'   defined as the fractional difference between the treatment-arm and
#'   control-arm pre-treatment baselines, expressed as a fraction of the
#'   control-arm baseline (the control-pre reference convention).
#'   Constrained to \eqn{\delta_i > -1} so that the derived
#'   \eqn{(1 + \delta_i)} factor remains positive in both modelled-mode
#'   parameterisations. Only used when `baseline_imbalance = "estimated"`.
#'   Default: `normal(0, 0.5)`.
#' @param baseline_difference_sd Prior on the between-study SD of the
#'   baseline imbalance. Only used when `baseline_imbalance = "estimated"`.
#'   Default: `cauchy(0.1)`.
#' @param multiplier Prior on the multiplicative-covariate effect multiplier
#'   (only used when `multiplicative_covariate` is specified in [meta_did()]).
#'   With one or two multiplicative covariates the same prior is applied
#'   independently to every estimated non-reference-level factor (of either
#'   covariate). Must be a [lognormal()] prior,
#'   placed on the log of the multiplier so it is strictly positive with no
#'   boundary at zero. Default: `lognormal(0, 0.7)` — a median of 1 (the
#'   no-multiplicative-effect case), with a central 95% range of roughly
#'   `[0.25, 3.9]` on the natural scale.
#'
#' @return A `did_priors` object.
#' @export
#'
#' @examples
#' # Use defaults
#' set_priors()
#'
#' # Override one prior
#' set_priors(treatment_effect_sd = cauchy(2))
set_priors <- function(
    treatment_effect_mean    = normal(0, 10),
    treatment_effect_sd      = cauchy(5),
    time_trend_mean          = normal(0, 10),
    time_trend_sd            = cauchy(5),
    rho_mean                 = normal(0, 1),
    rho_sd                   = normal(0, 0.5),
    nu                       = gamma(2, 0.1),
    delta_rct                = normal(0, 10),
    delta_pp                 = normal(0, 10),
    sigma                    = cauchy(5),
    beta_cov                 = normal(0, 10),
    lkj_eta                  = lkj(2),
    baseline_difference_mean = normal(0, 0.5),
    baseline_difference_sd   = cauchy(0.1),
    baseline_per_study       = NULL,
    multiplier               = lognormal(0, 0.7)
) {
  priors <- list(
    treatment_effect_mean    = treatment_effect_mean,
    treatment_effect_sd      = treatment_effect_sd,
    time_trend_mean          = time_trend_mean,
    time_trend_sd            = time_trend_sd,
    rho_mean                 = rho_mean,
    rho_sd                   = rho_sd,
    nu                       = nu,
    delta_rct                = delta_rct,
    delta_pp                 = delta_pp,
    sigma                    = sigma,
    beta_cov                 = beta_cov,
    lkj_eta                  = lkj_eta,
    baseline_difference_mean = baseline_difference_mean,
    baseline_difference_sd   = baseline_difference_sd,
    baseline_per_study       = baseline_per_study,  # NULL = compute from data
    multiplier               = multiplier
  )
  validate_priors(priors)
  structure(priors, class = "did_priors")
}

#' @export
print.did_priors <- function(x, ...) {
  cat("Prior distributions:\n")
  for (nm in names(x)) {
    cat(" ", nm, "~ ")
    print(x[[nm]])
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# validate_priors()
# ---------------------------------------------------------------------------

validate_priors <- function(priors) {
  for (nm in names(priors)) {
    p <- priors[[nm]]
    # baseline_per_study is allowed to be NULL (meaning "auto-compute from
    # data") so we skip validation entirely when not supplied.
    if (is.null(p) && nm == "baseline_per_study") next
    if (!inherits(p, "did_prior")) {
      stop(
        "Prior for '", nm, "' must be created with normal(), lognormal(), ",
        "cauchy(), gamma(), lkj(), or uniform().",
        call. = FALSE
      )
    }
    allowed <- .allowed_families[[nm]]
    if (!p$dist %in% allowed) {
      stop(
        "Prior for '", nm, "' must be one of: ",
        paste(allowed, collapse = ", "), ". Got: ", p$dist, ".",
        call. = FALSE
      )
    }
  }

  invisible(priors)
}

# ---------------------------------------------------------------------------
# as_stan_data.did_priors()
# ---------------------------------------------------------------------------

#' Convert a did_priors object to a flat Stan data list
#'
#' @param priors A `did_priors` object from [set_priors()].
#' @return A named list of scalar hyperparameter values.
#' @keywords internal
as_stan_data <- function(priors) UseMethod("as_stan_data")

#' @exportS3Method
as_stan_data.did_priors <- function(priors) {
  list(
    # treatment_effect_mean ~ normal(mean, sd)
    treatment_effect_mean_prior_mean = priors$treatment_effect_mean$mean,
    treatment_effect_mean_prior_sd   = priors$treatment_effect_mean$sd,
    # treatment_effect_sd ~ cauchy(0, scale)
    treatment_effect_sd_prior_scale  = priors$treatment_effect_sd$scale %||%
      priors$treatment_effect_sd$sd,
    # time_trend_mean ~ normal(mean, sd)
    time_trend_mean_prior_mean       = priors$time_trend_mean$mean,
    time_trend_mean_prior_sd         = priors$time_trend_mean$sd,
    # time_trend_sd ~ cauchy(0, scale)
    time_trend_sd_prior_scale        = priors$time_trend_sd$scale %||%
      priors$time_trend_sd$sd,
    # hierarchical rho ~ normal(mean, sd) on Fisher-z scale
    mu_z_prior_mean                  = priors$rho_mean$mean,
    mu_z_prior_sd                    = priors$rho_mean$sd,
    tau_z_prior_mean                 = priors$rho_sd$mean,
    tau_z_prior_sd                   = priors$rho_sd$sd,
    # nu ~ gamma(shape, rate) — only active when robust_heterogeneity = TRUE
    nu_prior_shape                   = priors$nu$shape,
    nu_prior_rate                    = priors$nu$rate,
    # design offsets ~ normal(0, sd) — only active when design_effects = TRUE
    delta_rct_prior_sd               = priors$delta_rct$sd,
    delta_pp_prior_sd                = priors$delta_pp$sd,
    # sigma ~ cauchy(0, scale) — study-level observation SDs
    sigma_prior_scale                = priors$sigma$scale,
    # beta_cov ~ normal(0, sd) — covariate regression coefficients
    beta_cov_prior_sd                = priors$beta_cov$sd,
    # LKJ prior on correlation between treatment effects and time trends
    lkj_eta_prior                    = priors$lkj_eta$eta,
    # Baseline imbalance: population mean and SD on normalised fractional scale.
    # Only used when is_baseline_difference_estimated == 1.
    baseline_difference_mean_prior_mean = priors$baseline_difference_mean$mean,
    baseline_difference_mean_prior_sd   = priors$baseline_difference_mean$sd,
    baseline_difference_sd_prior_scale  = priors$baseline_difference_sd$scale %||%
                                          priors$baseline_difference_sd$sd,
    # effect_multiplier ~ lognormal(meanlog, sdlog), i.e. prior on log scale
    effect_multiplier_prior_meanlog         = priors$multiplier$meanlog,
    effect_multiplier_prior_sdlog           = priors$multiplier$sdlog
  )
}

# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) if (!is.null(x)) x else y
