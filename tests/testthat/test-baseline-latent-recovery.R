# Recovery tests for the modelled-mode normalisation parameterisations.
#
# Each test fits the Stan model to a simulated DiD dataset with known
# population parameters and verifies that posterior intervals cover the
# truth. Slow — each fit takes ~30-60 seconds.
#
# Run with:
#   devtools::test(filter = "baseline-latent-recovery")
#
# skip_if_no_stan() and get_compiled_model() live in helper-stan.R.
# posterior_ci(), covers(), and ci_label() are duplicated here so this file
# is self-contained (the equivalents in test-recovery.R aren't exported).

# ---------------------------------------------------------------------------
# Local helpers
# ---------------------------------------------------------------------------

recovery_fit_modelled <- function(summary_data,
                                  baseline_latent_arm = "treatment",
                                  ...) {
  model <- get_compiled_model()
  skip_if(is.null(model), "Stan model could not be compiled")
  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )
  meta_did(
    summary_data        = summary_data,
    normalise           = TRUE,
    baseline_latent_arm = baseline_latent_arm,
    chains              = 2L,
    iter_warmup         = 500L,
    iter_sampling       = 500L,
    seed                = 8153L,
    refresh             = 0,
    ...
  )
}

posterior_ci <- function(fit, param, prob = 0.9) {
  draws <- fit$fit$draws(param, format = "matrix")
  lo_q  <- (1 - prob) / 2
  hi_q  <- 1 - lo_q
  list(
    mean = mean(draws),
    lo   = unname(stats::quantile(draws, lo_q)),
    hi   = unname(stats::quantile(draws, hi_q))
  )
}

covers <- function(fit, param, true_val, prob = 0.9) {
  ci <- posterior_ci(fit, param, prob)
  list(covers = ci$lo < true_val && ci$hi > true_val, ci = ci)
}

ci_label <- function(ci, true_val) {
  sprintf("90%% CI [%.4f, %.4f] (mean %.4f) covers true value %.4f",
          ci$lo, ci$hi, ci$mean, true_val)
}

# ---------------------------------------------------------------------------
# Shared simulation parameters
# ---------------------------------------------------------------------------
#
# All recovery tests below share these population parameters. Under modelled
# modes the hierarchical pop-level treatment_effect_mean is on the canonical
# fractional scale (fraction of treatment-pre baseline). With no baseline
# imbalance (b_T_pre ≈ b_C_pre = MEAN_BASELINE), the canonical truth equals
# the raw effect divided by MEAN_BASELINE.

TRUE_EFFECT_RAW        <- -0.15
TRUE_SIGMA_EFFECT_RAW  <- 0.03
TRUE_TREND_RAW         <- -0.04
TRUE_SIGMA_TREND_RAW   <- 0.02
MEAN_BASELINE          <- 0.45
TRUE_BASELINE_SD       <- 0.05

TRUE_EFFECT_CANONICAL  <- TRUE_EFFECT_RAW       / MEAN_BASELINE
TRUE_TREND_CANONICAL   <- TRUE_TREND_RAW        / MEAN_BASELINE

# Primary simulation — no baseline imbalance, balanced arms, n = 100 per arm.
sim_data_modelled <- simulate_meta_did(
  n_studies     = 40,
  true_effect   = TRUE_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  true_trend    = TRUE_TREND_RAW,
  sigma_trend   = TRUE_SIGMA_TREND_RAW,
  baseline_mean = MEAN_BASELINE,
  baseline_sd   = TRUE_BASELINE_SD,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 8471L
)
summary_data_modelled <- as_summary_did(sim_data_modelled)

# ---------------------------------------------------------------------------
# Fit M-T: treatment-latent recovery
# ---------------------------------------------------------------------------

test_that("treatment-latent mode recovers population parameters", {
  skip_if_no_stan()
  fit <- recovery_fit_modelled(
    summary_data        = summary_data_modelled,
    baseline_latent_arm = "treatment"
  )

  # treatment_effect_mean: 90% CI covers truth (canonical scale)
  res_te <- covers(fit, "treatment_effect_mean", TRUE_EFFECT_CANONICAL)
  expect_true(res_te$covers, label = ci_label(res_te$ci, TRUE_EFFECT_CANONICAL))

  # CI excludes zero — effect is real
  expect_lt(res_te$ci$hi, 0)

  # time_trend_mean: 90% CI covers truth
  res_tt <- covers(fit, "time_trend_mean", TRUE_TREND_CANONICAL)
  expect_true(res_tt$covers, label = ci_label(res_tt$ci, TRUE_TREND_CANONICAL))
})

# ---------------------------------------------------------------------------
# Fit M-C: control-latent recovery
# ---------------------------------------------------------------------------

test_that("control-latent mode recovers population parameters", {
  skip_if_no_stan()
  fit <- recovery_fit_modelled(
    summary_data        = summary_data_modelled,
    baseline_latent_arm = "control"
  )

  res_te <- covers(fit, "treatment_effect_mean", TRUE_EFFECT_CANONICAL)
  expect_true(res_te$covers, label = ci_label(res_te$ci, TRUE_EFFECT_CANONICAL))

  expect_lt(res_te$ci$hi, 0)

  res_tt <- covers(fit, "time_trend_mean", TRUE_TREND_CANONICAL)
  expect_true(res_tt$covers, label = ci_label(res_tt$ci, TRUE_TREND_CANONICAL))
})

# ---------------------------------------------------------------------------
# Fit M-T = Fit M-C: the two modes should give equivalent posteriors
# ---------------------------------------------------------------------------
#
# The treatment-latent and control-latent modes encode the same model with a
# different choice of which baseline is the "free" latent. Under well-identified
# conditions (decent sample sizes, balanced arms, mild imbalance) their
# posteriors on the population parameters should be the same up to MCMC noise.
# We give a 5% relative tolerance on posterior means as a generous bound for
# 500-iteration runs.

test_that("treatment-latent and control-latent agree on well-identified data", {
  skip_if_no_stan()

  fit_t <- recovery_fit_modelled(
    summary_data        = summary_data_modelled,
    baseline_latent_arm = "treatment"
  )
  fit_c <- recovery_fit_modelled(
    summary_data        = summary_data_modelled,
    baseline_latent_arm = "control"
  )

  # Use absolute tolerance — relative tolerance is meaningless near zero
  # (baseline_difference_mean's truth is 0 in this simulation, so any
  # non-zero posterior mean from MCMC noise inflates the relative metric).
  abs_tol <- 0.05
  for (param in c("treatment_effect_mean", "time_trend_mean",
                  "baseline_difference_mean")) {
    m_t <- posterior_ci(fit_t, param)$mean
    m_c <- posterior_ci(fit_c, param)$mean
    diff <- abs(m_t - m_c)
    expect_lt(
      diff, abs_tol,
      label = sprintf(
        "%s posteriors agree across parameterisations: treatment-latent mean = %.4f, control-latent mean = %.4f, |diff| = %.4f",
        param, m_t, m_c, diff
      )
    )
  }
})

# ---------------------------------------------------------------------------
# Fit M-imbalance: recovery under non-trivial baseline imbalance
# ---------------------------------------------------------------------------
#
# When the treatment arm starts systematically higher than the control arm,
# the modelled-mode machinery should still recover treatment_effect_mean AND
# additionally recover a non-zero baseline_difference_mean.

test_that("treatment-latent recovers effect AND imbalance under DiD imbalance", {
  skip_if_no_stan()
  skip_if_not_installed("MASS")

  set.seed(8471L)
  n_studies     <- 30L
  did_thetas    <- rnorm(n_studies, TRUE_EFFECT_RAW,    TRUE_SIGMA_EFFECT_RAW)
  did_betas     <- rnorm(n_studies, TRUE_TREND_RAW,     TRUE_SIGMA_TREND_RAW)
  did_baselines <- rnorm(n_studies, MEAN_BASELINE,      TRUE_BASELINE_SD)
  did_gammas    <- rnorm(n_studies, 0.05, 0.01)   # systematic imbalance: b_T - b_C ≈ 0.05
  within_sd     <- 0.12
  rho           <- 0.5
  n_per_arm     <- 100L

  sim_rows <- lapply(seq_len(n_studies), function(i) {
    Sigma <- within_sd^2 * matrix(c(1, rho, rho, 1), 2, 2)
    ctrl  <- MASS::mvrnorm(n_per_arm,
                           c(did_baselines[i], did_baselines[i] + did_betas[i]),
                           Sigma)
    trt   <- MASS::mvrnorm(n_per_arm,
                           c(did_baselines[i] + did_gammas[i],
                             did_baselines[i] + did_gammas[i] + did_betas[i] + did_thetas[i]),
                           Sigma)
    data.frame(
      study_id            = paste0("study_", i),
      design              = "did",
      n_control           = n_per_arm,
      n_treatment         = n_per_arm,
      mean_pre_control    = mean(ctrl[, 1]),
      mean_post_control   = mean(ctrl[, 2]),
      sd_pre_control      = sd(ctrl[, 1]),
      sd_post_control     = sd(ctrl[, 2]),
      mean_pre_treatment  = mean(trt[, 1]),
      mean_post_treatment = mean(trt[, 2]),
      sd_pre_treatment    = sd(trt[, 1]),
      sd_post_treatment   = sd(trt[, 2]),
      rho                 = rho
    )
  })
  imbalance_data <- do.call(rbind, sim_rows)

  fit <- recovery_fit_modelled(
    summary_data        = imbalance_data,
    baseline_latent_arm = "treatment",
    baseline_imbalance  = "estimated"
  )

  # Treatment effect recovered despite imbalance.
  # NOTE: under treatment-pre canonical scale the truth is θ_raw / b_T_pre,
  # not θ_raw / b_C_pre. With imbalance, b_T_pre = b_C_pre * (1 + δ_truth),
  # so the canonical truth shifts relative to the no-imbalance case.
  TRUE_IMBALANCE_DELTA           <- 0.05 / MEAN_BASELINE      # ≈ 0.111
  TRUE_BT_PRE_IMBALANCE          <- MEAN_BASELINE * (1 + TRUE_IMBALANCE_DELTA)
  TRUE_EFFECT_CANONICAL_IMBALANCE <- TRUE_EFFECT_RAW / TRUE_BT_PRE_IMBALANCE

  res_te <- covers(fit, "treatment_effect_mean", TRUE_EFFECT_CANONICAL_IMBALANCE)
  expect_true(res_te$covers, info = ci_label(res_te$ci, TRUE_EFFECT_CANONICAL_IMBALANCE))

  # baseline_difference_mean should be non-zero and positive.
  # Truth under legacy (b_T - b_C)/b_C convention: 0.05 / 0.45 ≈ 0.111
  TRUE_IMBALANCE_LEGACY <- 0.05 / MEAN_BASELINE
  res_bd <- posterior_ci(fit, "baseline_difference_mean", prob = 0.9)
  expect_true(res_bd$lo > 0,
              info = sprintf("baseline_difference_mean 90%% CI [%.4f, %.4f] excludes zero",
                             res_bd$lo, res_bd$hi))
  # Generous tolerance — recovery under modelled mode introduces some
  # additional posterior spread from the per-study latent baseline uncertainty.
  expect_lt(abs(res_bd$mean - TRUE_IMBALANCE_LEGACY), 0.05,
            label = sprintf("baseline_difference_mean (mean %.4f) is within 0.05 of truth %.4f",
                            res_bd$mean, TRUE_IMBALANCE_LEGACY))
})
