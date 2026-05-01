# Parameter recovery tests for the Stan model.
#
# These tests simulate data from known population parameters using
# simulate_meta_did(), fit the model, and verify that posterior intervals
# cover the true values. They are slower than the smoke tests in test-stan.R;
# run them selectively with
#   devtools::test(filter = "recovery")
#
# To minimise runtime, each test_that block does a single model fit and
# checks multiple parameters from that fit. There are 5 fits total.
#
# skip_if_no_stan() and get_compiled_model() live in helper-stan.R.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

recovery_fit <- function(summary_data = NULL, individual_data = NULL, ...) {
  model <- get_compiled_model()
  skip_if(is.null(model), "Stan model could not be compiled")
  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )
  meta_did(
    summary_data    = summary_data,
    individual_data = individual_data,
    chains          = 2L,
    iter_warmup     = 500L,
    iter_sampling   = 500L,
    seed            = 8153L,
    refresh         = 0,
    ...
  )
}

# 90% CI from posterior draws of a scalar parameter
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

# Check that a 90% CI covers a target value; returns list with $covers and $ci
covers <- function(fit, param, true_val, prob = 0.9) {
  ci <- posterior_ci(fit, param, prob)
  list(covers = ci$lo < true_val && ci$hi > true_val, ci = ci)
}

ci_label <- function(ci, true_val) {
  sprintf("90%% CI [%.4f, %.4f] covers true value %.4f",
          ci$lo, ci$hi, true_val)
}

# ---------------------------------------------------------------------------
# Shared simulation parameters
# ---------------------------------------------------------------------------
#
# All simulations share these population parameters. After normalisation by
# baseline_mean, the Stan model targets normalised quantities:
#   treatment_effect_i ≈ true_effect / baseline_mean
#   time_trend_i       ≈ true_trend  / baseline_mean

TRUE_EFFECT_RAW        <- -0.15
TRUE_SIGMA_EFFECT_RAW  <- 0.03
TRUE_TREND_RAW         <- -0.04
TRUE_SIGMA_TREND_RAW   <- 0.02
MEAN_BASELINE          <- 0.45
TRUE_BASELINE_SD       <- 0.05

TRUE_EFFECT_NORMALISED      <- TRUE_EFFECT_RAW       / MEAN_BASELINE
TRUE_SIGMA_NORMALISED       <- TRUE_SIGMA_EFFECT_RAW / MEAN_BASELINE
TRUE_TREND_NORMALISED       <- TRUE_TREND_RAW        / MEAN_BASELINE
TRUE_SIGMA_TREND_NORMALISED <- TRUE_SIGMA_TREND_RAW  / MEAN_BASELINE

# Primary simulation — heterogeneous trends so time_trend_sd is recoverable
sim_data <- simulate_meta_did(
  n_studies     = 25,
  true_effect   = TRUE_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  true_trend    = TRUE_TREND_RAW,
  sigma_trend   = TRUE_SIGMA_TREND_RAW,
  baseline_mean = MEAN_BASELINE,
  baseline_sd   = TRUE_BASELINE_SD,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 6204L
)

# ---------------------------------------------------------------------------
# Fit 1: DiD summary, normalised
# ---------------------------------------------------------------------------
# Checks: treatment effect (direction, CI, excludes zero, SD),
#         time trend (mean, SD), convergence (R-hat).

test_that("Fit 1: DiD summary normalised recovers all population parameters", {
  skip_if_no_stan()
  fit <- recovery_fit(summary_data = as_summary_did(sim_data))
  te  <- summary(fit)

  # treatment_effect_mean: direction
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(te_row$mean < 0, label = "estimated effect is negative")

  # treatment_effect_mean: 90% CI covers true value
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = ci_label(te_row, TRUE_EFFECT_NORMALISED)
  )

  # treatment_effect_mean: 90% CI excludes zero
  expect_true(te_row$hi < 0, label = "entire 90% CI is negative")

  # treatment_effect_sd
  sd_row <- te[te$parameter == "treatment_effect_sd", ]
  expect_true(sd_row$mean > 0, label = "between-study SD is positive")
  expect_true(
    sd_row$lo < TRUE_SIGMA_NORMALISED && sd_row$hi > TRUE_SIGMA_NORMALISED,
    label = ci_label(sd_row, TRUE_SIGMA_NORMALISED)
  )

  # time_trend_mean
  res_tt <- covers(fit, "time_trend_mean", TRUE_TREND_NORMALISED)
  expect_true(res_tt$covers, label = ci_label(res_tt$ci, TRUE_TREND_NORMALISED))

  # time_trend_sd
  res_tsd <- covers(fit, "time_trend_sd", TRUE_SIGMA_TREND_NORMALISED)
  expect_true(res_tsd$covers, label = ci_label(res_tsd$ci, TRUE_SIGMA_TREND_NORMALISED))

  # Convergence
  rhat <- fit$fit$summary(c("treatment_effect_mean", "treatment_effect_sd",
                            "time_trend_mean", "time_trend_sd"))$rhat
  expect_true(all(rhat < 1.05),
              label = paste("R-hat:", paste(round(rhat, 3), collapse = ", ")))
})

# ---------------------------------------------------------------------------
# Fit 2: DiD individual, normalised
# ---------------------------------------------------------------------------
# Checks: treatment effect (direction, CI), consistency with summary fit.
# We store the summary-fit posterior mean from Fit 1's data so we can
# compare. Rather than caching a fit object, we accept that we need the
# summary point estimate from both fits to appear in the same test_that
# block, so we run both here (2 fits in this block).

test_that("Fit 2: individual DiD recovers treatment effect and agrees with summary", {
  skip_if_no_stan()
  fit_sum <- recovery_fit(summary_data    = as_summary_did(sim_data))
  fit_ind <- recovery_fit(individual_data = as_individual_did(sim_data))

  te_ind <- summary(fit_ind)
  te_row <- te_ind[te_ind$parameter == "treatment_effect_mean", ]

  # Direction
  expect_true(te_row$mean < 0, label = "individual estimate is negative")

  # CI covers true value
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = ci_label(te_row, TRUE_EFFECT_NORMALISED)
  )

  # Consistency: summary and individual posterior means within 0.05
  te_sum <- summary(fit_sum)
  mean_sum <- te_sum$mean[te_sum$parameter == "treatment_effect_mean"]
  mean_ind <- te_row$mean
  expect_true(
    abs(mean_sum - mean_ind) < 0.05,
    label = sprintf("summary %.3f vs individual %.3f differ by %.3f",
                    mean_sum, mean_ind, abs(mean_sum - mean_ind))
  )
})

# ---------------------------------------------------------------------------
# Fit 3: Mixed all designs, normalised
# ---------------------------------------------------------------------------

test_that("Fit 3: mixed DiD + RCT + PP recovers treatment effect", {
  skip_if_no_stan()
  mixed <- dplyr::bind_rows(
    as_summary_did(sim_data),
    as_summary_rct(sim_data),
    as_summary_pp(sim_data)
  )
  fit <- recovery_fit(summary_data = mixed)
  te  <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = ci_label(te_row, TRUE_EFFECT_NORMALISED)
  )
})

# ---------------------------------------------------------------------------
# Fit 4: Design effects
# ---------------------------------------------------------------------------
# Simulate DiD studies with one true effect and RCT/PP studies with shifted
# effects, then verify that design_effects = TRUE recovers the design-
# specific means.

DELTA_RCT_RAW <- 0.10
DELTA_PP_RAW  <- -0.08
TRUE_RCT_EFFECT_RAW <- TRUE_EFFECT_RAW + DELTA_RCT_RAW
TRUE_PP_EFFECT_RAW  <- TRUE_EFFECT_RAW + DELTA_PP_RAW
TRUE_RCT_NORMALISED <- TRUE_RCT_EFFECT_RAW / MEAN_BASELINE
TRUE_PP_NORMALISED  <- TRUE_PP_EFFECT_RAW  / MEAN_BASELINE

sim_did_de <- simulate_meta_did(
  n_studies     = 15,
  true_effect   = TRUE_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  baseline_mean = MEAN_BASELINE,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 3847L
)
sim_rct_de <- simulate_meta_did(
  n_studies     = 15,
  true_effect   = TRUE_RCT_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  baseline_mean = MEAN_BASELINE,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 5921L
)
sim_pp_de <- simulate_meta_did(
  n_studies     = 15,
  true_effect   = TRUE_PP_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  baseline_mean = MEAN_BASELINE,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 7063L
)

rct_summary <- as_summary_rct(sim_rct_de)
rct_summary$study_id <- paste0("rct_", rct_summary$study_id)
pp_summary <- as_summary_pp(sim_pp_de)
pp_summary$study_id <- paste0("pp_", pp_summary$study_id)

mixed_de <- dplyr::bind_rows(
  as_summary_did(sim_did_de),
  rct_summary,
  pp_summary
)

test_that("Fit 4: design effects recover DiD, RCT, and PP treatment effect means", {
  skip_if_no_stan()
  fit <- recovery_fit(summary_data = mixed_de, design_effects = TRUE)
  te  <- summary(fit)

  # DiD mean (reference category)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = ci_label(te_row, TRUE_EFFECT_NORMALISED)
  )

  # RCT mean
  rct_row <- te[te$parameter == "treatment_effect_mean_rct", ]
  expect_true(
    rct_row$lo < TRUE_RCT_NORMALISED && rct_row$hi > TRUE_RCT_NORMALISED,
    label = ci_label(rct_row, TRUE_RCT_NORMALISED)
  )

  # PP mean
  pp_row <- te[te$parameter == "treatment_effect_mean_pp", ]
  expect_true(
    pp_row$lo < TRUE_PP_NORMALISED && pp_row$hi > TRUE_PP_NORMALISED,
    label = ci_label(pp_row, TRUE_PP_NORMALISED)
  )

  # Ordering: RCT is less negative than DiD, PP is more negative
  expect_true(rct_row$mean > te_row$mean,
              label = "RCT mean is less negative than DiD mean")
  expect_true(pp_row$mean < te_row$mean,
              label = "PP mean is more negative than DiD mean")
})

# ---------------------------------------------------------------------------
# Fit 5: Unnormalised — recover baseline, effect, and trend on raw scale
# ---------------------------------------------------------------------------

test_that("Fit 5: unnormalised recovers baseline, treatment effect, and time trend", {
  skip_if_no_stan()
  fit <- recovery_fit(
    summary_data = as_summary_did(sim_data),
    normalise_by_baseline = FALSE
  )

  # baseline_control_mean
  res_bl <- covers(fit, "baseline_control_mean", MEAN_BASELINE)
  expect_true(res_bl$covers, label = ci_label(res_bl$ci, MEAN_BASELINE))

  # treatment_effect_mean (raw scale)
  res_te <- covers(fit, "treatment_effect_mean", TRUE_EFFECT_RAW)
  expect_true(res_te$covers, label = ci_label(res_te$ci, TRUE_EFFECT_RAW))

  # time_trend_mean (raw scale)
  res_tt <- covers(fit, "time_trend_mean", TRUE_TREND_RAW)
  expect_true(res_tt$covers, label = ci_label(res_tt$ci, TRUE_TREND_RAW))
})
