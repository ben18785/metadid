# Trend-exchangeability sensitivity: the analytic information share and the
# exact refits must agree, since each is the other's check. The cheap tests
# below run everywhere; the agreement test needs Stan.

test_that(".change_var matches the paired change-score variance", {
  # sd_pre = sd_post = s, rho = 0 gives 2 s^2 / n; rho = 1 gives 0.
  expect_equal(metadid:::.change_var(2, 2, 0, 10), 8 / 10)
  expect_equal(metadid:::.change_var(2, 2, 1, 10), 0)
  # And the general form, by hand.
  expect_equal(metadid:::.change_var(3, 4, 0.5, 25), (9 + 16 - 12) / 25)
})

# An 8 DiD + 8 pre-post summary frame, for the Stan-backed checks.
make_mixed_summary <- function(seed = 7L) {
  sim <- simulate_meta_did(n_studies = 16, true_effect = -0.15,
                           sigma_effect = 0.03, true_trend = -0.04,
                           sigma_trend = 0.02, seed = seed)
  ids <- unique(sim$study_id)
  sub <- function(k) {
    s <- sim[sim$study_id %in% k, ]
    attr(s, "true_params") <- attr(sim, "true_params")
    s
  }
  dplyr::bind_rows(as_summary_did(sub(ids[1:8])), as_summary_pp(sub(ids[9:16])))
}

# A minimal valid two-study DiD frame, for the checks that never fit anything.
make_did_summary <- function() {
  data.frame(
    study_id            = c("S1", "S2"),
    design              = "did",
    n_control           = 50L,
    mean_pre_control    = 0.45,
    mean_post_control   = 0.42,
    sd_pre_control      = 0.12,
    sd_post_control     = 0.11,
    n_treatment         = 55L,
    mean_pre_treatment  = 0.46,
    mean_post_treatment = 0.30,
    sd_pre_treatment    = 0.13,
    sd_post_treatment   = 0.10,
    rho                 = 0.5
  )
}

test_that("pp_trend_shift must be a single number", {
  d <- make_did_summary()
  expect_error(meta_did_general(summary_data = d, pp_trend_shift = "a"),
               "single number")
  expect_error(meta_did_general(summary_data = d, pp_trend_shift = c(0, 1)),
               "single number")
})

test_that("pp_trend_shift reaches the Stan data and defaults to zero", {
  d <- make_did_summary()
  flags <- list(is_baseline_normalised = 1L)
  base  <- prepare_stan_data(d, NULL, flags, set_priors())
  expect_equal(base$pp_trend_shift, 0)
  shifted <- prepare_stan_data(d, NULL, flags, set_priors(),
                               pp_trend_shift = -0.03)
  expect_equal(shifted$pp_trend_shift, -0.03)
})

test_that("a fit without stored arguments refuses to be swept", {
  obj <- new_meta_did_fit(
    fit = NULL, summary_data = data.frame(design = "pp", study_id = "s1"),
    individual_data = NULL, model_flags = list(), priors = NULL,
    normalisation_factors = NULL
  )
  expect_error(sensitivity_trend(obj), "cannot be refitted reproducibly")
})

test_that("the information share refuses designs it cannot speak to", {
  obj <- new_meta_did_fit(
    fit = NULL, summary_data = data.frame(design = "did", study_id = "s1"),
    individual_data = NULL, model_flags = list(), priors = NULL,
    normalisation_factors = NULL, fit_args = list()
  )
  expect_error(sensitivity_trend(obj), "No pre-post studies")
  expect_error(omega_pp(obj), "No pre-post studies")
})

# ---------------------------------------------------------------------------
# Stan-backed: the two routes must agree
# ---------------------------------------------------------------------------

test_that("pp_trend_shift = 0 reproduces the unshifted fit", {
  skip_if_no_stan()
  sim <- simulate_meta_did(n_studies = 12, true_effect = -0.15,
                           sigma_effect = 0.03, true_trend = -0.04,
                           sigma_trend = 0.02, seed = 99L)
  ids <- unique(sim$study_id)
  sub <- function(k) {
    s <- sim[sim$study_id %in% k, ]
    attr(s, "true_params") <- attr(sim, "true_params")
    s
  }
  # bind_rows, not rbind: the two designs carry different columns and the
  # missing cells must become NA rather than an error.
  dat <- dplyr::bind_rows(as_summary_did(sub(ids[1:6])),
                          as_summary_pp(sub(ids[7:12])))
  common <- list(summary_data = dat, chains = 2L, iter_warmup = 500L,
                 iter_sampling = 500L, seed = 11L, refresh = 0,
                 show_messages = FALSE)
  a <- do.call(meta_did_general, common)
  b <- do.call(meta_did_general, c(common, list(pp_trend_shift = 0)))
  mu <- function(f) mean(as.numeric(
    f$fit$draws("treatment_effect_mean", format = "draws_matrix")))
  # Same data, same seed, a shift of zero: the posteriors must coincide.
  expect_equal(mu(a), mu(b), tolerance = 1e-8)
})

test_that("the analytic information share predicts the refit slope", {
  skip_if_no_stan()
  # The whole point of omega_pp(): it must reproduce, from a single fit, the
  # slope that refitting across a grid of Delta measures directly. The shared
  # control arm matters here -- treating the DiD effect and trend estimates as
  # independent overstates the share by several percent.
  sim <- simulate_meta_did(n_studies = 30, n_control = 80, n_treatment = 80,
                           true_effect = -0.15, sigma_effect = 0.03,
                           true_trend = -0.02, sigma_trend = 0.03,
                           baseline_mean = 0.45, baseline_sd = 0.02,
                           rho = 0.5, seed = 4718L)
  ids <- unique(sim$study_id)
  sub <- function(k) {
    s <- sim[sim$study_id %in% k, ]
    attr(s, "true_params") <- attr(sim, "true_params")
    s
  }
  dat <- dplyr::bind_rows(as_summary_did(sub(ids[1:10])),
                          as_summary_pp(sub(ids[11:30])))
  fit <- meta_did(summary_data = dat, chains = 2L, iter_warmup = 1000L,
                  iter_sampling = 1000L, seed = 1L, refresh = 0,
                  show_messages = FALSE)

  sweep <- sensitivity_trend(fit, delta = c(-0.04, 0, 0.04), quiet = TRUE)
  analytic <- omega_pp(fit)

  # Raising the assumed pre-post trend lowers the inferred effect.
  expect_lt(sweep$slope, 0)
  # Agreement to about a tenth is the real claim. Two things keep it from being
  # exact: omega_pp() is a Fisher-information approximation while the slope is
  # the model's actual response, and omega_pp() takes the median of omega over
  # draws rather than omega at the median variance components, which are not the
  # same number. A careful hand check at four chains and 1000 iterations put the
  # two at 0.346 and 0.343; at the iteration count used here the slope itself
  # carries Monte Carlo error of roughly 0.01.
  expect_equal(sweep$omega_empirical, analytic$median, tolerance = 0.10)
  # The share cannot exceed what the DiD studies know about trends.
  expect_lt(analytic$median, analytic$cap + 1e-8)
  # Delta* is the shift that nulls the effect, and the sweep finds the same one
  # by interpolation when it falls inside the grid.
  ds <- delta_star(fit)
  expect_equal(sign(ds$median), sign(mean(sweep$grid$estimate)))
})

# ---------------------------------------------------------------------------
# lprior, the quantity priorsense consumes
# ---------------------------------------------------------------------------

test_that("lprior matches an independent computation of the priors", {
  skip_if_no_stan()
  # This is the only part of the power-scaling chain that is ours to get wrong:
  # priorsense's importance sampling is its own tested code, but it reweights by
  # exp((alpha - 1) * lprior), so a missing or mis-parameterised term here would
  # silently corrupt every sensitivity it reports. Recomputing lprior in R and
  # comparing draw by draw catches that exactly.
  sim <- simulate_meta_did(n_studies = 16, true_effect = -0.15,
                           sigma_effect = 0.03, true_trend = -0.04,
                           sigma_trend = 0.02, seed = 7L)
  ids <- unique(sim$study_id)
  sub <- function(k) {
    s <- sim[sim$study_id %in% k, ]
    attr(s, "true_params") <- attr(sim, "true_params")
    s
  }
  dat <- dplyr::bind_rows(as_summary_did(sub(ids[1:8])),
                          as_summary_pp(sub(ids[9:16])))
  fit <- meta_did(summary_data = dat, chains = 2L, iter_warmup = 500L,
                  iter_sampling = 500L, seed = 3L, refresh = 0,
                  show_messages = FALSE)

  dr <- function(v) as.numeric(fit$fit$draws(v, format = "draws_matrix"))
  # The terms active for a normalised summary fit with hierarchical rho, no
  # covariates, no multipliers, fixed kappa, mu_gamma pinned and a normal
  # heterogeneity distribution. Note cauchy_lpdf, NOT a half-Cauchy: Stan's
  # <lower=0> truncation drops a constant that cancels in any power-scaling.
  expected <-
    stats::dnorm(dr("time_trend_mean"), 0, 10, log = TRUE) +
    stats::dcauchy(dr("time_trend_sd"), 0, 5, log = TRUE) +
    stats::dnorm(dr("treatment_effect_mean"), 0, 10, log = TRUE) +
    stats::dcauchy(dr("treatment_effect_sd"), 0, 5, log = TRUE) +
    stats::dcauchy(dr("baseline_difference_sd"), 0, 0.1, log = TRUE) +
    stats::dnorm(dr("mu_z"), 0, 1, log = TRUE) +
    stats::dnorm(dr("tau_z"), 0, 0.5, log = TRUE)

  # 1e-6 is the precision CmdStan writes to its CSV, not slack in the check:
  # the two agree to about 5e-7 on every draw.
  expect_equal(dr("lprior"), expected, tolerance = 1e-6)
})

test_that("log_lik carries one entry per summary study", {
  skip_if_no_stan()
  sim <- simulate_meta_did(n_studies = 16, true_effect = -0.15,
                           sigma_effect = 0.03, seed = 7L)
  ids <- unique(sim$study_id)
  sub <- function(k) {
    s <- sim[sim$study_id %in% k, ]
    attr(s, "true_params") <- attr(sim, "true_params")
    s
  }
  dat <- dplyr::bind_rows(as_summary_did(sub(ids[1:8])),
                          as_summary_pp(sub(ids[9:16])))
  fit <- meta_did(summary_data = dat, chains = 2L, iter_warmup = 500L,
                  iter_sampling = 500L, seed = 3L, refresh = 0,
                  show_messages = FALSE)
  ll <- fit$fit$draws("log_lik", format = "draws_matrix")
  expect_equal(ncol(ll), nrow(dat))
  expect_true(all(is.finite(ll)))
})

test_that("priorsense accepts a fit and reports a usable Pareto k", {
  skip_if_no_stan()
  skip_if_not_installed("priorsense")
  # End-to-end, but deliberately modest in what it asserts. A refit against
  # genuinely wider priors is NOT the same model as power-scaling unless every
  # scaled prior is normal: raising a Cauchy to the power alpha gives a
  # Student-t with nu = 2*alpha - 1 and scale s/sqrt(2*alpha - 1), which
  # set_priors() cannot express. The heterogeneity priors here are Cauchy, so
  # the two routes are only approximately comparable and a tight equality test
  # would be asserting a false identity. What is worth pinning down is that the
  # pipeline runs on our fits and that the importance sampling is trustworthy
  # at a mild perturbation.
  sim <- simulate_meta_did(n_studies = 16, true_effect = -0.15,
                           sigma_effect = 0.03, seed = 7L)
  ids <- unique(sim$study_id)
  sub <- function(k) {
    s <- sim[sim$study_id %in% k, ]
    attr(s, "true_params") <- attr(sim, "true_params")
    s
  }
  dat <- dplyr::bind_rows(as_summary_did(sub(ids[1:8])),
                          as_summary_pp(sub(ids[9:16])))
  fit <- meta_did(summary_data = dat, chains = 2L, iter_warmup = 500L,
                  iter_sampling = 500L, seed = 3L, refresh = 0,
                  show_messages = FALSE)

  psd <- priorsense::create_priorsense_data(fit$fit)
  s <- priorsense::powerscale_sensitivity(psd, variable = "treatment_effect_mean")
  expect_true(all(c("prior", "likelihood") %in% names(s)))
  expect_true(is.finite(s$prior[1]) && is.finite(s$likelihood[1]))

  ps <- priorsense::powerscale(psd, component = "prior", alpha = 0.8,
                               variable = "treatment_effect_mean")
  khat <- attr(ps, "powerscaling")$diagnostics$khat
  expect_lt(khat, 0.7)   # importance sampling reliable at this perturbation
})

test_that("the prior family in set_priors() is honoured, not just validated", {
  skip_if_no_stan()
  # set_priors() accepted and validated normal() for treatment_effect_sd while
  # shared_priors.stan hard-coded cauchy, so the family was silently ignored and
  # the user got a half-Cauchy with the normal's SD as its scale. lprior_effect
  # is computed from the same branch the model samples from, so if the family
  # were dropped again this comparison against dnorm would fail.
  dat <- make_mixed_summary()
  fit <- meta_did(summary_data = dat,
                  priors = set_priors(treatment_effect_sd = normal(0, 2)),
                  chains = 2L, iter_warmup = 500L, iter_sampling = 500L,
                  seed = 3L, refresh = 0, show_messages = FALSE)
  dr <- function(v) as.numeric(fit$fit$draws(v, format = "draws_matrix"))
  expect_equal(
    dr("lprior_effect"),
    stats::dnorm(dr("treatment_effect_mean"), 0, 10, log = TRUE) +
      stats::dnorm(dr("treatment_effect_sd"), 0, 2, log = TRUE),
    tolerance = 1e-6
  )
})

test_that("the lprior partitions sum to the total", {
  skip_if_no_stan()
  # priorsense reads the total when no selection is given and lprior_<name> when
  # one is, so the two must describe the same prior.
  fit <- meta_did(summary_data = make_mixed_summary(), chains = 2L,
                  iter_warmup = 500L, iter_sampling = 500L, seed = 3L,
                  refresh = 0, show_messages = FALSE)
  dr <- function(v) as.numeric(fit$fit$draws(v, format = "draws_matrix"))
  parts <- dr("lprior_effect") + dr("lprior_trend") + dr("lprior_imbalance") +
           dr("lprior_rho") + dr("lprior_other")
  expect_equal(dr("lprior"), parts, tolerance = 1e-6)
})

test_that("power-scaling one partition matches a full refit against wider priors", {
  skip_if_no_stan()
  skip_if_not_installed("priorsense")
  # The end-to-end check. Power-scaling a prior by alpha is exactly a scale of
  # 1/sqrt(alpha) for a NORMAL prior, and lprior_rho holds two normal priors
  # (mu_z and tau_z), so scaling that partition has a refit equivalent and the
  # importance sampling can be checked against brute force. The priors are set
  # deliberately tight: with the defaults tau_z is identified by the data and
  # both routes would agree on doing nothing, which tests nothing.
  dat <- make_mixed_summary()
  alpha <- 0.5
  cc <- 1 / sqrt(alpha)
  tight <- set_priors(rho_mean = normal(0, 0.10), rho_sd = normal(0, 0.02))
  wider <- set_priors(rho_mean = normal(0, 0.10 * cc), rho_sd = normal(0, 0.02 * cc))
  common <- list(summary_data = dat, chains = 4L, iter_warmup = 1000L,
                 iter_sampling = 1000L, seed = 3L, refresh = 0,
                 show_messages = FALSE)
  f0 <- do.call(meta_did, c(common, list(priors = tight)))
  f1 <- do.call(meta_did, c(common, list(priors = wider)))

  psd <- priorsense::create_priorsense_data(f0$fit)
  ps <- priorsense::powerscale(psd, component = "prior", alpha = alpha,
                               selection = "rho", variable = "tau_z[1]")
  khat <- attr(ps, "powerscaling")$diagnostics$khat
  expect_lt(khat, 0.7)   # otherwise the comparison below is not meaningful

  w <- exp(ps$.log_weight - max(ps$.log_weight))
  w <- w / sum(w)
  powerscaled <- sum(w * ps[["tau_z[1]"]])
  dr <- function(f) as.numeric(f$fit$draws("tau_z", format = "draws_matrix"))
  original <- mean(dr(f0))
  refitted <- mean(dr(f1))

  # Widening the prior must move tau_z up, and both routes must say so.
  expect_gt(refitted, original)
  expect_gt(powerscaled, original)
  # Absolute, not relative: they agreed to 0.0007 against a Monte Carlo SE of
  # 0.0008 when this was written, which is 3% in relative terms but a twentieth
  # of what matters -- the 0.005 that widening the prior actually moved tau_z.
  expect_lt(abs(powerscaled - refitted), 3e-3)
  # Scale-free version of the same claim: the two routes must agree far better
  # than the movement they are both measuring.
  expect_lt(abs(powerscaled - refitted), 0.5 * (refitted - original))
})

# ---------------------------------------------------------------------------
# study_influence(): does a planted outlier actually get caught?
# ---------------------------------------------------------------------------

# Twelve well-behaved DiD studies plus one discordant study of the chosen size.
make_with_outlier <- function(n_outlier = 20L, mult = 1.25, seed = 21L) {
  sim <- simulate_meta_did(n_studies = 12, true_effect = -0.15,
                           sigma_effect = 0.03, true_trend = -0.04,
                           sigma_trend = 0.02, seed = seed)
  base <- as_summary_did(sim)
  base$rho <- 0.5
  odd <- base[1, ]
  odd$study_id <- "odd_one"
  odd$n_control <- n_outlier
  odd$n_treatment <- n_outlier
  odd$mean_post_treatment <- odd$mean_pre_treatment * mult   # wrong sign, large
  dplyr::bind_rows(base, odd)
}

test_that("a discordant study is caught by Pareto k, and its cost is reported", {
  skip_if_no_stan()
  skip_if_not_installed("loo")
  # The diagnostic has to find a study that a weight table cannot: this one
  # carries the LOWEST share of the evidence, because inflating tau flattens
  # every weight, and yet removing it halves the width of the pooled interval.
  dat <- make_with_outlier()
  fit <- meta_did(summary_data = dat, chains = 4L, iter_warmup = 1000L,
                  iter_sampling = 1000L, seed = 5L, refresh = 0,
                  show_messages = FALSE)
  # loo's own high-k notice is the expected outcome here, not noise: assert it
  # rather than letting it surface as an unexplained warning.
  expect_warning(inf <- study_influence(fit, refit = "flagged", quiet = TRUE),
                 "Pareto k")
  odd <- inf[inf$study_id == "odd_one", ]
  clean <- inf[inf$study_id != "odd_one", ]

  # Caught, and the only one caught.
  expect_true(odd$flagged)
  expect_gt(odd$khat, 0.7)
  expect_lt(max(clean$khat), 0.7)
  expect_equal(inf$study_id[1], "odd_one")   # sorted by decreasing khat

  # elpd_i separates it far more clearly than the weight does: the rest of the
  # evidence predicts it badly, which is the substantive statement.
  expect_lt(odd$elpd_i, min(clean$elpd_i))

  # The weight does NOT single it out -- it is the smallest in the analysis.
  # If this ever fails, information_share() has started doing a job it cannot.
  expect_lt(odd$weight, min(clean$weight))

  # And the refit reports the damage where it actually lands. The effect on the
  # between-study SD is larger than on the mean, so reporting only the mean
  # would understate this study by half.
  expect_gt(odd$d_effect_sd, 0.05)
  expect_gt(odd$d_effect_sd, odd$d_effect_mean)
  expect_gt(odd$d_ci_width, 0.05)
  expect_true(all(is.na(clean$d_effect_sd)))   # unflagged studies not refitted
})

test_that("two outliers mask each other, as leave-one-out must", {
  skip_if_no_stan()
  skip_if_not_installed("loo")
  # A limitation worth pinning down rather than discovering in an analysis:
  # with two discordant studies present, each is explained by the other's
  # inflation of tau, and leave-ONE-out flags neither. The vignette says so.
  dat <- make_with_outlier()
  big <- dat[dat$study_id == "odd_one", ]
  big$study_id <- "odd_two"
  big$n_control <- 2000L
  big$n_treatment <- 2000L
  fit <- meta_did(summary_data = dplyr::bind_rows(dat, big), chains = 4L,
                  iter_warmup = 1000L, iter_sampling = 1000L, seed = 5L,
                  refresh = 0, show_messages = FALSE)
  inf <- study_influence(fit, quiet = TRUE)
  expect_false(any(inf$flagged))
  # And the weights have gone near-uniform, which is the mechanism.
  expect_lt(max(inf$weight) / min(inf$weight), 1.2)
})
