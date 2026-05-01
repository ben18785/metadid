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

#' @export
print.did_prior <- function(x, ...) {
  params <- switch(x$dist,
    normal = paste0("mean = ", x$mean, ", sd = ", x$sd),
    cauchy = paste0("scale = ", x$scale),
    gamma  = paste0("shape = ", x$shape, ", rate = ", x$rate)
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
  delta_pp              = c("normal")
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
    delta_pp              = normal(0, 10)
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
    delta_pp              = delta_pp
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
    delta_pp_prior_sd                = priors$delta_pp$sd
  )
}

# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) if (!is.null(x)) x else y
