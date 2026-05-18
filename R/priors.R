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
#' @param lower Optional hard lower bound on the parameter (default `-Inf`).
#'   When finite, propagates to a `<lower=...>` constraint on the corresponding
#'   Stan parameter declaration. Currently honoured only by parameters whose
#'   Stan declaration reads bounds from data (e.g. `multiplier`).
#' @param upper Optional hard upper bound on the parameter (default `Inf`).
#'   See `lower`.
#' @return A `did_prior` object.
#' @export
normal <- function(mean, sd, lower = -Inf, upper = Inf) {
  stopifnot(is.numeric(mean), length(mean) == 1)
  stopifnot(is.numeric(sd), length(sd) == 1, sd > 0)
  stopifnot(is.numeric(lower), length(lower) == 1)
  stopifnot(is.numeric(upper), length(upper) == 1)
  if (upper <= lower) {
    stop("normal(): `upper` must be strictly greater than `lower`.", call. = FALSE)
  }
  structure(
    list(dist = "normal", mean = mean, sd = sd, lower = lower, upper = upper),
    class = "did_prior"
  )
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

#' @export
print.did_prior <- function(x, ...) {
  params <- switch(x$dist,
    normal = {
      base <- paste0("mean = ", x$mean, ", sd = ", x$sd)
      lo <- x[["lower"]]
      up <- x[["upper"]]
      bnds <- character()
      if (!is.null(lo) && is.finite(lo)) bnds <- c(bnds, paste0("lower = ", lo))
      if (!is.null(up) && is.finite(up)) bnds <- c(bnds, paste0("upper = ", up))
      if (length(bnds) > 0) paste(c(base, bnds), collapse = ", ") else base
    },
    cauchy = paste0("scale = ", x$scale),
    gamma  = paste0("shape = ", x$shape, ", rate = ", x$rate),
    lkj    = paste0("eta = ", x$eta)
  )
  cat(x$dist, "(", params, ")\n", sep = "")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Allowed families per parameter (used by validate_priors)
# ---------------------------------------------------------------------------

.allowed_families <- list(
  treatment_effect_mean = c("normal"),
  treatment_effect_sd   = c("cauchy", "normal"),
  time_trend_mean       = c("normal"),
  time_trend_sd         = c("cauchy", "normal"),
  rho_mean              = c("normal"),
  rho_sd                = c("normal"),
  nu                    = c("gamma"),
  delta_rct             = c("normal"),
  delta_pp              = c("normal"),
  sigma                 = c("cauchy"),
  beta_cov              = c("normal"),
  multiplier            = c("normal"),
  lkj_eta               = c("lkj")
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
#' @param multiplier Prior on the multiplicative-covariate factor(s)
#'   (only used when `multiplicative_covariates` is specified in
#'   [meta_did()]). A single prior is shared across all multiplicative
#'   covariates, mirroring how `beta_cov` is shared across additive ones.
#'   Default: `normal(1, 0.5, lower = 0)` — centred at 1 (no multiplicative
#'   effect) with positive support. Optionally set an upper bound for
#'   improved MCMC stability, e.g. `normal(1, 0.5, lower = 0, upper = 5)`.
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
    treatment_effect_mean = normal(0, 10),
    treatment_effect_sd   = cauchy(5),
    time_trend_mean       = normal(0, 10),
    time_trend_sd         = cauchy(5),
    rho_mean              = normal(0, 1),
    rho_sd                = normal(0, 0.5),
    nu                    = gamma(2, 0.1),
    delta_rct             = normal(0, 10),
    delta_pp              = normal(0, 10),
    sigma                 = cauchy(5),
    beta_cov              = normal(0, 10),
    lkj_eta               = lkj(2),
    multiplier            = normal(1, 0.5, lower = 0)
) {
  priors <- list(
    treatment_effect_mean = treatment_effect_mean,
    treatment_effect_sd   = treatment_effect_sd,
    time_trend_mean       = time_trend_mean,
    time_trend_sd         = time_trend_sd,
    rho_mean              = rho_mean,
    rho_sd                = rho_sd,
    nu                    = nu,
    delta_rct             = delta_rct,
    delta_pp              = delta_pp,
    sigma                 = sigma,
    beta_cov              = beta_cov,
    lkj_eta               = lkj_eta,
    multiplier            = multiplier
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
    if (!inherits(p, "did_prior")) {
      stop(
        "Prior for '", nm, "' must be created with normal(), cauchy(), or gamma().",
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

  # Multiplier-specific: multipliers must be non-negative; require lower >= 0.
  mp <- priors$multiplier
  if (!is.null(mp)) {
    lo <- mp[["lower"]] %||% -Inf
    if (lo < 0) {
      stop(
        "Prior for 'multiplier' must have lower >= 0 (multipliers are non-negative). ",
        "Got lower = ", lo, ".",
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
  # Translate a possibly-infinite bound into a Stan-acceptable finite number.
  # Stan parameter declarations require finite bounds; passing a very large
  # number is the standard workaround for "effectively unbounded".
  .finite_bound <- function(x, default) {
    if (is.null(x) || !is.finite(x)) default else x
  }
  mp <- priors$multiplier
  gamma_lo <- .finite_bound(mp[["lower"]], -1e6)
  gamma_up <- .finite_bound(mp[["upper"]],  1e6)

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
    # gamma_mult ~ normal(mean, sd) bounded [lower, upper] — only active when
    # multiplicative_covariates is non-NULL (K_mult > 0). Bounds are required
    # by the Stan parameter declaration even when K_mult == 0.
    gamma_mult_prior_mean            = priors$multiplier$mean,
    gamma_mult_prior_sd              = priors$multiplier$sd,
    gamma_mult_lower                 = gamma_lo,
    gamma_mult_upper                 = gamma_up
  )
}

# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) if (!is.null(x)) x else y
