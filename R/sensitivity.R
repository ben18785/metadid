# Sensitivity to trend exchangeability.
#
# The model assumes pre-post studies draw their trends from the same population
# as DiD studies. If their trend mean is really mu_beta + Delta, the fitted
# model attributes the excess to the treatment effect in proportion to the
# weight the pre-post arm carries:
#
#   bias(mu_theta_hat) ~= omega_pp * Delta
#
# and inverting gives a breakdown value, the systematic difference between the
# two trend distributions that would be needed to explain the pooled effect
# away. Reporting it is in the spirit of an E-value.
#
# Two routes are provided. sensitivity_trend() refits across a grid of Delta
# and is exact, including the nonlinearity the normalised post-only
# reparameterisation introduces. omega_pp() computes the information share
# analytically from one fit, costing nothing but resting on a local linear
# approximation. The recovery tests check the two agree.

# ---------------------------------------------------------------------------
# Internal: per-study sampling variances
# ---------------------------------------------------------------------------

# Pre/post correlation per study, on the scale the parameters live on. Studies
# that report rho use it; the rest fall back to the posterior median of the
# hierarchical population rho, which is what the model itself imputes for them.
.rho_for_studies <- function(fit, rows) {
  n <- nrow(rows)
  if (n == 0) return(numeric(0))
  rho <- if ("rho" %in% names(rows)) as.numeric(rows$rho) else rep(NA_real_, n)
  if (anyNA(rho)) {
    mu_z <- try(
      fit$fit$draws("mu_z", format = "draws_matrix"), silent = TRUE
    )
    if (inherits(mu_z, "try-error") || length(mu_z) == 0) {
      stop("Studies with missing 'rho' need the hierarchical rho model to ",
           "impute them; this fit has none, so the information share cannot ",
           "be computed. Supply 'rho' for every study, or use ",
           "sensitivity_trend(refit = TRUE).", call. = FALSE)
    }
    rho[is.na(rho)] <- tanh(stats::median(as.numeric(mu_z)))
  }
  rho
}

# Sampling variance of a within-arm change, (sd_pre^2 + sd_post^2
# - 2 rho sd_pre sd_post) / n. This is the quantity the SI calls s^2 for a
# pre-post study and s_b^2 for a DiD study's control arm.
.change_var <- function(sd_pre, sd_post, rho, n) {
  (sd_pre^2 + sd_post^2 - 2 * rho * sd_pre * sd_post) / n
}

# The three variance pieces S3 needs, per study, from summary data only.
.trend_info_pieces <- function(fit) {
  sdata <- fit$summary_data
  if (is.null(sdata) || nrow(sdata) == 0) {
    stop("The analytic information share needs summary-level data. For an ",
         "individual-level fit the per-study sampling variances are ",
         "parameters rather than data; use sensitivity_trend(refit = TRUE).",
         call. = FALSE)
  }
  did <- sdata[sdata$design == "did", , drop = FALSE]
  pp  <- sdata[sdata$design == "pp",  , drop = FALSE]
  if (nrow(pp) == 0) {
    stop("No pre-post studies in this fit, so trend exchangeability is not ",
         "an assumption it relies on.", call. = FALSE)
  }
  if (nrow(did) == 0) {
    stop("No summary-level DiD studies in this fit, so there is no anchor ",
         "whose trend information the pre-post studies borrow.", call. = FALSE)
  }
  rho_did <- .rho_for_studies(fit, did)
  rho_pp  <- .rho_for_studies(fit, pp)
  list(
    # Control-arm change: what a DiD study knows about its own trend.
    v_c = .change_var(did$sd_pre_control, did$sd_post_control,
                      rho_did, did$n_control),
    # Treatment-arm change: the other half of the double difference.
    v_t = .change_var(did$sd_pre_treatment, did$sd_post_treatment,
                      rho_did, did$n_treatment),
    # Pre-post change: informs mu_theta + mu_beta jointly.
    v_p = .change_var(pp$sd_pre_treatment, pp$sd_post_treatment,
                      rho_pp, pp$n_treatment),
    n_did = nrow(did), n_pp = nrow(pp)
  )
}

# ---------------------------------------------------------------------------
# omega_pp(): the pre-post share of the information about mu_theta
# ---------------------------------------------------------------------------

#' Pre-post share of the information about the pooled effect
#'
#' @description
#' The fraction of what is known about \eqn{\mu_\theta} that comes from
#' pre-post studies, which is also the factor by which a systematic difference
#' between their trend distribution and the DiD studies' biases the pooled
#' effect (see `vignette("model-details")`).
#'
#' @details
#' A DiD study supplies an effect estimate and a trend estimate that **share
#' its control arm**, so the two are correlated, with
#' \eqn{\mathrm{Cov} = -\mathrm{var}(\text{control change})}. Treating them as
#' independent overstates the share by a few percent, so the 2x2 information
#' per study is summed rather than two scalars. Writing \eqn{A} for the total
#' pre-post precision,
#' \deqn{I = \sum_i \Sigma_i^{-1} + A \mathbf{1}\mathbf{1}^\top, \qquad
#'       \omega_{P} = \left(I^{-1} [A, A]^\top\right)_1 .}
#'
#' The returned `cap` is S3's saturation result \eqn{h/(g+h)}: the share cannot
#' exceed what the DiD studies know about the trend distribution, however many
#' pre-post studies are added. It uses the scalar (independent) form, so treat
#' it as the approximation it is.
#'
#' Computed per posterior draw, since it depends on \eqn{\tau_\theta} and
#' \eqn{\tau_\beta}. Summary-level data only.
#'
#' @param fit A [meta_did()] fit containing both DiD and pre-post summary studies.
#' @return A list with `draws` (numeric vector of \eqn{\omega_P} per draw),
#'   `median`, `q5`, `q95` and `cap`.
#' @seealso [sensitivity_trend()], [delta_star()]
#' @export
omega_pp <- function(fit) {
  stopifnot(inherits(fit, "meta_did_fit"))
  p <- .trend_info_pieces(fit)
  tau_theta <- as.numeric(fit$fit$draws("treatment_effect_sd", format = "draws_matrix"))
  tau_beta  <- as.numeric(fit$fit$draws("time_trend_sd",       format = "draws_matrix"))
  n_draws <- length(tau_theta)

  # Accumulate the summed 2x2 information over DiD studies, vectorised across
  # draws. Sigma_i = [[v_t + v_c + tau_theta^2, -v_c], [-v_c, v_c + tau_beta^2]].
  g11 <- g12 <- g22 <- numeric(n_draws)
  for (i in seq_along(p$v_c)) {
    a <- p$v_t[i] + p$v_c[i] + tau_theta^2
    b <- -p$v_c[i]
    d <- p$v_c[i] + tau_beta^2
    det <- a * d - b^2
    g11 <- g11 + d / det
    g12 <- g12 - b / det
    g22 <- g22 + a / det
  }
  # Each pre-post study informs mu_theta + mu_beta, contributing A * 11'.
  A <- rowSums(vapply(p$v_p, function(v) 1 / (v + tau_theta^2 + tau_beta^2),
                      numeric(n_draws)))
  i11 <- g11 + A; i12 <- g12 + A; i22 <- g22 + A
  det_i <- i11 * i22 - i12^2
  omega <- A * (i22 - i12) / det_i

  # S3's ceiling, in the scalar form the SI states it.
  g_s <- rowSums(vapply(seq_along(p$v_c),
                        function(i) 1 / (p$v_t[i] + p$v_c[i] + tau_theta^2),
                        numeric(n_draws)))
  h_s <- rowSums(vapply(p$v_c, function(v) 1 / (v + tau_beta^2),
                        numeric(n_draws)))
  list(
    draws  = omega,
    median = stats::median(omega),
    q5     = unname(stats::quantile(omega, 0.05)),
    q95    = unname(stats::quantile(omega, 0.95)),
    cap    = stats::median(h_s / (g_s + h_s))
  )
}

# ---------------------------------------------------------------------------
# delta_star(): the breakdown value
# ---------------------------------------------------------------------------

#' Breakdown value for trend exchangeability
#'
#' @description
#' \eqn{\Delta^* = \hat\mu_\theta / \omega_P}: the systematic difference between
#' the pre-post and DiD trend distributions that would be needed to move the
#' pooled effect to zero. A large value relative to plausible secular trends
#' indicates robustness; a small one flags dependence on the assumption.
#'
#' @details
#' \eqn{\Delta^*} is a ratio of two posterior quantities, so its tails are
#' heavy when \eqn{\omega_P} is near zero --- which is the robust case, since a
#' vanishing share means no shift can explain the effect away. Read the median
#' and the interval, not the mean.
#'
#' Units are those of the fitted effect, per study interval: on the normalised
#' scale a \eqn{\Delta^*} of \eqn{-0.5} means pre-post trends would have to run
#' 50 percentage points of baseline below DiD trends **over each study's own
#' pre-to-post interval**. It is not an annual rate, since interval lengths
#' differ across studies.
#'
#' @param fit A [meta_did()] fit containing both DiD and pre-post summary studies.
#' @return A list with `draws`, `median`, `q5`, `q95` and the `omega` object
#'   it was derived from.
#' @seealso [omega_pp()], [sensitivity_trend()]
#' @export
delta_star <- function(fit) {
  om <- omega_pp(fit)
  mu <- as.numeric(fit$fit$draws("treatment_effect_mean", format = "draws_matrix"))
  d  <- mu / om$draws
  list(
    draws  = d,
    median = stats::median(d),
    q5     = unname(stats::quantile(d, 0.05)),
    q95    = unname(stats::quantile(d, 0.95)),
    omega  = om
  )
}

# ---------------------------------------------------------------------------
# sensitivity_trend(): exact refits across a grid of Delta
# ---------------------------------------------------------------------------

#' Sensitivity of the pooled effect to trend exchangeability
#'
#' @description
#' Refits the model across a grid of \eqn{\Delta}, the systematic difference
#' between the pre-post and DiD trend distributions, and reports how the pooled
#' effect responds. This is the exact version of [delta_star()]'s
#' approximation: it captures the shrinkage and the nonlinearity of the
#' normalised post-only reparameterisation, at the cost of one fit per grid
#' point.
#'
#' @details
#' Each refit sets `pp_trend_shift = delta`, which asserts
#' \eqn{\beta_j \sim N(\mu_\beta + \Delta, \tau_\beta^2)} for pre-post studies
#' only. Every other setting is taken from the original fit, so a sweep cannot
#' differ from it in a sampler setting or a modelling switch.
#'
#' Two breakdown values are returned. `delta_point` is where the posterior mean
#' crosses zero --- the SI's \eqn{\Delta^*}. `delta_ci` is where the interval
#' first includes zero, which is the more useful of the two: it is the shift at
#' which the *conclusion*, rather than the point estimate, stops holding. Both
#' come from linear interpolation on the grid and are `NA` when no crossing
#' occurs inside it.
#'
#' @param fit A [meta_did()] fit containing pre-post studies.
#' @param delta Numeric vector of shifts to refit at. Defaults to a symmetric
#'   grid spanning roughly the population trend, which is the scale on which a
#'   plausible exchangeability failure would sit.
#' @param probs Interval probabilities for the reported bounds.
#' @param quiet Suppress the per-refit progress message.
#' @return A list with `grid` (a data frame of `delta`, `estimate`, `lo`, `hi`,
#'   `divergences`), the fitted `slope`, `omega_empirical` (which is `-slope`,
#'   and is what [omega_pp()] predicts), `delta_point`, `delta_ci`, and the
#'   `fits` themselves.
#' @seealso [omega_pp()], [delta_star()]
#' @export
sensitivity_trend <- function(fit, delta = NULL, probs = c(0.05, 0.95),
                              quiet = FALSE) {
  stopifnot(inherits(fit, "meta_did_fit"))
  if (is.null(fit$fit_args)) {
    stop("This fit does not carry the arguments it was created with, so it ",
         "cannot be refitted reproducibly. Refit the model with the current ",
         "version of the package first.", call. = FALSE)
  }
  n_pp <- sum(c(
    if (!is.null(fit$summary_data))    sum(fit$summary_data$design == "pp"),
    if (!is.null(fit$individual_data)) {
      length(unique(fit$individual_data$study_id[fit$individual_data$design == "pp"]))
    }
  ))
  if (n_pp == 0) {
    stop("No pre-post studies in this fit, so trend exchangeability is not ",
         "an assumption it relies on.", call. = FALSE)
  }

  if (is.null(delta)) {
    mu_beta <- stats::median(as.numeric(
      fit$fit$draws("time_trend_mean", format = "draws_matrix")
    ))
    span  <- max(abs(mu_beta), 0.02) * 2
    delta <- seq(-span, span, length.out = 5L)
  }

  fits <- vector("list", length(delta))
  rows <- vector("list", length(delta))
  for (k in seq_along(delta)) {
    if (!quiet) {
      message(sprintf("sensitivity_trend: refit %d of %d (delta = %+.4f)",
                      k, length(delta), delta[k]))
    }
    args <- utils::modifyList(fit$fit_args, list(pp_trend_shift = delta[k]))
    f <- do.call(.meta_did_core, args)
    fits[[k]] <- f
    draws <- as.numeric(f$fit$draws("treatment_effect_mean", format = "draws_matrix"))
    div <- tryCatch(sum(f$fit$diagnostic_summary(quiet = TRUE)$num_divergent),
                    error = function(e) NA_integer_)
    rows[[k]] <- data.frame(
      delta       = delta[k],
      estimate    = mean(draws),
      lo          = unname(stats::quantile(draws, probs[1])),
      hi          = unname(stats::quantile(draws, probs[2])),
      divergences = div
    )
  }
  grid <- do.call(rbind, rows)

  # d(estimate)/d(delta). Shifting the ASSUMED pre-post trend up by Delta pushes
  # the inferred effect down, so the slope is -omega_pp: raising the trend the
  # model credits to those studies leaves less of their change for treatment.
  # omega_pp() predicts the magnitude; the recovery test checks they agree.
  slope <- unname(stats::coef(stats::lm(estimate ~ delta, data = grid))[2])

  # Where a curve crosses zero, by linear interpolation. approx() needs its x
  # sorted, and the curves here decrease in delta, so order explicitly.
  .cross <- function(y) {
    ok <- !is.na(y)
    if (sum(ok) < 2L || min(y[ok]) > 0 || max(y[ok]) < 0) return(NA_real_)
    o <- order(y[ok])
    stats::approx(x = y[ok][o], y = grid$delta[ok][o], xout = 0)$y
  }
  # The bound that can reach zero is the one nearer to it.
  near_zero <- if (mean(grid$estimate) < 0) grid$hi else grid$lo

  list(
    grid            = grid,
    slope           = slope,
    omega_empirical = -slope,
    delta_point = .cross(grid$estimate),
    delta_ci    = .cross(near_zero),
    fits        = fits
  )
}
