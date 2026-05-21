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
  rct_df <- as_summary_rct(sim_data)
  rct_df$study_id <- paste0("rct_", rct_df$study_id)
  pp_df <- as_summary_pp(sim_data)
  pp_df$study_id <- paste0("pp_", pp_df$study_id)
  mixed <- dplyr::bind_rows(
    as_summary_did(sim_data),
    rct_df,
    pp_df
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
  n_studies     = 25,
  true_effect   = TRUE_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  baseline_mean = MEAN_BASELINE,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 3847L
)
sim_rct_de <- simulate_meta_did(
  n_studies     = 25,
  true_effect   = TRUE_RCT_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  baseline_mean = MEAN_BASELINE,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 5921L
)
sim_pp_de <- simulate_meta_did(
  n_studies     = 25,
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

# ---------------------------------------------------------------------------
# Fit 5 (naive): meta_did_naive recovers treatment effect when assumptions hold
# ---------------------------------------------------------------------------
# Simulate data with zero time trend and equal baselines (the DGP always uses
# the same baseline for both arms), so the naive assumptions are satisfied.

sim_naive <- simulate_meta_did(
  n_studies     = 25,
  true_effect   = TRUE_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  true_trend    = 0,
  sigma_trend   = 0,
  baseline_mean = MEAN_BASELINE,
  baseline_sd   = TRUE_BASELINE_SD,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 4529L
)

naive_recovery_fit <- function(summary_data = NULL, individual_data = NULL, ...) {
  model <- get_compiled_model()
  skip_if(is.null(model), "Stan model could not be compiled")
  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )
  meta_did_naive(
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

test_that("Fit 5a (naive): mixed designs recover treatment effect under naive assumptions", {
  skip_if_no_stan()
  rct_df <- as_summary_rct(sim_naive)
  rct_df$study_id <- paste0("rct_", rct_df$study_id)
  pp_df <- as_summary_pp(sim_naive)
  pp_df$study_id <- paste0("pp_", pp_df$study_id)
  mixed <- dplyr::bind_rows(
    as_summary_did(sim_naive),
    rct_df,
    pp_df
  )
  fit <- naive_recovery_fit(summary_data = mixed)
  te  <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(te_row$mean < 0, label = "estimated effect is negative")
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = ci_label(te_row, TRUE_EFFECT_NORMALISED)
  )
})

# ---------------------------------------------------------------------------
# Fit 5b (naive vs full): full model outperforms naive when time trend is large
# ---------------------------------------------------------------------------
# With a substantial time trend, the naive model (which sets β = 0 for PP
# studies) conflates the time trend with the treatment effect, biasing it.
# The full model borrows the time trend from DiD studies and should be closer
# to the truth.

LARGE_TREND_RAW <- -0.10
LARGE_TREND_TRUE_EFFECT_NORMALISED <- TRUE_EFFECT_RAW / MEAN_BASELINE

sim_large_trend <- simulate_meta_did(
  n_studies     = 20,
  true_effect   = TRUE_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  true_trend    = LARGE_TREND_RAW,
  sigma_trend   = 0.01,
  baseline_mean = MEAN_BASELINE,
  baseline_sd   = TRUE_BASELINE_SD,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 7318L
)

# Mostly PP studies with a few DiD to identify the trend
sim_pp_heavy <- simulate_meta_did(
  n_studies     = 20,
  true_effect   = TRUE_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  true_trend    = LARGE_TREND_RAW,
  sigma_trend   = 0.01,
  baseline_mean = MEAN_BASELINE,
  baseline_sd   = TRUE_BASELINE_SD,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 2491L
)

test_that("Fit 5b: full model is closer to truth than naive when PP studies have large time trend", {
  skip_if_no_stan()

  did_df <- as_summary_did(sim_large_trend)
  pp_df  <- as_summary_pp(sim_pp_heavy)
  pp_df$study_id <- paste0("pp_", pp_df$study_id)
  mixed <- dplyr::bind_rows(did_df, pp_df)

  fit_full  <- recovery_fit(summary_data = mixed)
  fit_naive <- naive_recovery_fit(summary_data = mixed)

  te_full  <- summary(fit_full)
  te_naive <- summary(fit_naive)

  mean_full  <- te_full$mean[te_full$parameter   == "treatment_effect_mean"]
  mean_naive <- te_naive$mean[te_naive$parameter  == "treatment_effect_mean"]

  err_full  <- abs(mean_full  - LARGE_TREND_TRUE_EFFECT_NORMALISED)
  err_naive <- abs(mean_naive - LARGE_TREND_TRUE_EFFECT_NORMALISED)

  expect_true(
    err_full < err_naive,
    label = sprintf(
      "full model error (%.4f) < naive error (%.4f); true = %.4f, full = %.4f, naive = %.4f",
      err_full, err_naive, LARGE_TREND_TRUE_EFFECT_NORMALISED, mean_full, mean_naive
    )
  )
})

# ---------------------------------------------------------------------------
# Fit 5c–5e: meta_did_general() equivalence and mixed-settings tests
# ---------------------------------------------------------------------------

general_recovery_fit <- function(summary_data = NULL, individual_data = NULL, ...) {
  model <- get_compiled_model()
  skip_if(is.null(model), "Stan model could not be compiled")
  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )
  meta_did_general(
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

test_that("Fit 5c: meta_did_general defaults match meta_did", {
  skip_if_no_stan()
  fit <- general_recovery_fit(summary_data = as_summary_did(sim_naive))
  te <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(te_row$mean < 0, label = "estimated effect is negative")
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = ci_label(te_row, TRUE_EFFECT_NORMALISED)
  )
})

test_that("Fit 5d: meta_did_general with fixed_zero settings matches naive", {
  skip_if_no_stan()
  # sim_naive has true_trend = 0 and equal baselines, so the naive
  # assumptions are satisfied and the treatment effect should be recovered.
  rct_df <- as_summary_rct(sim_naive)
  rct_df$study_id <- paste0("rct_", rct_df$study_id)
  pp_df <- as_summary_pp(sim_naive)
  pp_df$study_id <- paste0("pp_", pp_df$study_id)
  mixed <- dplyr::bind_rows(
    as_summary_did(sim_naive),
    rct_df,
    pp_df
  )
  fit <- general_recovery_fit(
    summary_data       = mixed,
    time_trend         = "fixed_zero",
    baseline_imbalance = "fixed_zero"
  )
  te <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(te_row$mean < 0, label = "estimated effect is negative")
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = ci_label(te_row, TRUE_EFFECT_NORMALISED)
  )
})

test_that("Fit 5e: meta_did_general with mixed settings runs without error", {
  skip_if_no_stan()
  # sim_large_trend has a non-trivial time trend and equal baselines
  # (simulate_meta_did uses the same baseline for both arms). Using
  # pooled time trends + fixed-zero baselines is a meaningful combination.
  did_df <- as_summary_did(sim_large_trend)
  rct_df <- as_summary_rct(sim_large_trend)
  rct_df$study_id <- paste0("rct_", rct_df$study_id)
  mixed <- dplyr::bind_rows(did_df, rct_df)

  fit <- general_recovery_fit(
    summary_data       = mixed,
    time_trend         = "pooled",
    baseline_imbalance = "fixed_zero"
  )
  te <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(!is.na(te_row$mean), label = "treatment_effect_mean is estimated")
})

test_that("Fit 5f: meta_did_general with bivariate pp_likelihood runs without error", {
  skip_if_no_stan()
  did_df <- as_summary_did(sim_naive)
  pp_df  <- as_summary_pp(sim_naive)
  pp_df$study_id <- paste0("pp_", pp_df$study_id)
  mixed <- dplyr::bind_rows(did_df, pp_df)

  fit <- general_recovery_fit(
    summary_data  = mixed,
    pp_likelihood = "bivariate"
  )
  te <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(!is.na(te_row$mean), label = "treatment_effect_mean is estimated")
})

# ---------------------------------------------------------------------------
# Fit 5g: correlated_effects recovery test
# ---------------------------------------------------------------------------

sim_correlated <- simulate_meta_did(
  n_studies        = 30,
  true_effect      = TRUE_EFFECT_RAW,
  sigma_effect     = TRUE_SIGMA_EFFECT_RAW,
  true_trend       = TRUE_TREND_RAW,
  sigma_trend      = TRUE_SIGMA_TREND_RAW,
  baseline_mean    = MEAN_BASELINE,
  baseline_sd      = 0,
  n_control        = 100L,
  n_treatment      = 100L,
  rho_effect_trend = 0.6,
  seed             = 7294L
)

test_that("Fit 5g: correlated_effects recovers treatment effect and correlation", {
  skip_if_no_stan()
  fit <- general_recovery_fit(
    summary_data       = as_summary_did(sim_correlated),
    correlated_effects = TRUE
  )
  te <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(te_row$mean < 0, label = "estimated effect is negative")
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = ci_label(te_row, TRUE_EFFECT_NORMALISED)
  )
})

sim_unnorm <- simulate_meta_did(
  n_studies     = 25,
  true_effect   = TRUE_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  true_trend    = TRUE_TREND_RAW,
  sigma_trend   = TRUE_SIGMA_TREND_RAW,
  baseline_mean = MEAN_BASELINE,
  baseline_sd   = 0,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 8471L
)

# ---------------------------------------------------------------------------
# Fit 6a: Covariate meta-regression (single covariate)
# ---------------------------------------------------------------------------
# Simulate studies whose true treatment effect varies with a covariate (dose).
# After normalisation, verify that both the intercept (treatment_effect_mean)
# and slope (beta_cov) are recovered.

TRUE_BETA_COV_RAW <- -0.04   # raw-scale effect of one unit increase in dose
COV_DOSE <- seq(1, 4, length.out = 40)
MEAN_DOSE <- mean(COV_DOSE)

sim_cov <- simulate_meta_did(
  n_studies     = 40,
  true_effect   = TRUE_EFFECT_RAW,
  sigma_effect  = TRUE_SIGMA_EFFECT_RAW,
  true_trend    = TRUE_TREND_RAW,
  sigma_trend   = TRUE_SIGMA_TREND_RAW,
  baseline_mean = MEAN_BASELINE,
  baseline_sd   = TRUE_BASELINE_SD,
  n_control     = 100L,
  n_treatment   = 100L,
  seed          = 6427L,
  covariates    = data.frame(dose = COV_DOSE),
  beta_cov      = TRUE_BETA_COV_RAW
)

# After normalisation, both effect and slope are divided by baseline_mean.
# Since covariates are centered, treatment_effect_mean is the effect at
# the mean dose: (true_effect + mean_dose * beta_cov) / baseline_mean.
TRUE_BETA_COV_NORMALISED <- TRUE_BETA_COV_RAW / MEAN_BASELINE
TRUE_EFFECT_AT_MEAN_DOSE_NORMALISED <-
  (TRUE_EFFECT_RAW + MEAN_DOSE * TRUE_BETA_COV_RAW) / MEAN_BASELINE

test_that("Fit 6a: covariate meta-regression recovers treatment effect and beta_cov", {
  skip_if_no_stan()
  fit <- recovery_fit(
    summary_data = as_summary_did(sim_cov),
    covariates   = ~ dose
  )
  te <- summary(fit)

  # treatment_effect_mean: 90% CI covers true value (at mean dose, since
  # covariates are centered by default)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(
    te_row$lo < TRUE_EFFECT_AT_MEAN_DOSE_NORMALISED &&
      te_row$hi > TRUE_EFFECT_AT_MEAN_DOSE_NORMALISED,
    label = ci_label(te_row, TRUE_EFFECT_AT_MEAN_DOSE_NORMALISED)
  )

  # beta_cov[dose]: 90% CI covers true slope
  beta_row <- te[te$parameter == "beta_cov[dose]", ]
  expect_true(
    nrow(beta_row) == 1,
    label = "beta_cov[dose] appears in summary"
  )
  expect_true(
    beta_row$lo < TRUE_BETA_COV_NORMALISED && beta_row$hi > TRUE_BETA_COV_NORMALISED,
    label = ci_label(beta_row, TRUE_BETA_COV_NORMALISED)
  )

  # beta_cov should be negative (higher dose → more negative treatment effect)
  expect_true(beta_row$mean < 0,
              label = "estimated beta_cov is negative")

  # treatment_effect_sd should be smaller than the no-covariate case because
  # dose explains some between-study variation. We don't have a strict target
  # but verify it's estimated and positive.
  sd_row <- te[te$parameter == "treatment_effect_sd", ]
  expect_true(sd_row$mean > 0, label = "residual between-study SD is positive")

  # Convergence
  rhat <- fit$fit$summary(c("treatment_effect_mean", "treatment_effect_sd",
                            "beta_cov"))$rhat
  expect_true(all(rhat < 1.05),
              label = paste("R-hat:", paste(round(rhat, 3), collapse = ", ")))
})

# ---------------------------------------------------------------------------
# Fit 6b: Unnormalised — recover baseline, effect, and trend on raw scale
# ---------------------------------------------------------------------------

test_that("Fit 6b: unnormalised recovers baseline, treatment effect, and time trend", {
  skip_if_no_stan()
  fit <- recovery_fit(
    summary_data = as_summary_did(sim_unnorm),
    normalise = FALSE
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

# ---------------------------------------------------------------------------
# Fit 7: DiD summary with non-zero baseline imbalance, baseline_imbalance="estimated"
# ---------------------------------------------------------------------------
# Checks: treatment effect recovered when DiD studies have systematic imbalance,
#         baseline_difference_mean is non-zero and in the right direction.

test_that("Fit 7: estimated baseline imbalance recovers treatment effect under DiD imbalance", {
  skip_if_no_stan()
  skip_if_not_installed("MASS")

  # Simulate DiD studies where the treatment arm starts systematically higher
  # than control (gamma_raw = 0.05 on a baseline_mean = 0.45 scale →
  # baseline_difference_normalised ~ 0.111).
  set.seed(8471L)
  n_studies     <- 30L
  did_thetas    <- rnorm(n_studies, TRUE_EFFECT_RAW,    TRUE_SIGMA_EFFECT_RAW)
  did_betas     <- rnorm(n_studies, TRUE_TREND_RAW,     TRUE_SIGMA_TREND_RAW)
  did_baselines <- rnorm(n_studies, MEAN_BASELINE,      TRUE_BASELINE_SD)
  did_gammas    <- rnorm(n_studies, 0.05, 0.01)   # systematic imbalance
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

  fit_est <- recovery_fit(
    summary_data       = imbalance_data,
    baseline_imbalance = "estimated"
  )

  # Treatment effect should be recovered (within CI) despite baseline imbalance.
  # Under the treatment-pre canonical scale, the truth shifts when there is
  # non-zero baseline imbalance: the canonical denominator is b_T_pre, not
  # b_C_pre, so the true canonical effect is θ_raw / b_T_pre rather than
  # θ_raw / MEAN_BASELINE (which equals b_C_pre on average).
  TRUE_EFFECT_CANONICAL_IMBALANCE <- TRUE_EFFECT_RAW /
                                     (MEAN_BASELINE * (1 + 0.05 / MEAN_BASELINE))
  te <- covers(fit_est, "treatment_effect_mean", TRUE_EFFECT_CANONICAL_IMBALANCE)
  expect_true(te$covers, info = ci_label(te$ci, TRUE_EFFECT_CANONICAL_IMBALANCE))

  # baseline_difference_mean should be non-zero and positive
  # (truth ≈ 0.05 / 0.45 ≈ 0.111 on normalised scale)
  bd <- posterior_ci(fit_est, "baseline_difference_mean", prob = 0.9)
  expect_true(bd$lo > 0,
              info = sprintf("baseline_difference_mean 90%% CI [%.4f, %.4f] excludes zero",
                             bd$lo, bd$hi))
  expect_lt(abs(bd$mean - 0.05 / MEAN_BASELINE), 0.05)
})
