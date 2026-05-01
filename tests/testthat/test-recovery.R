# Parameter recovery tests for the Stan model.
#
# These tests simulate data from known population parameters using
# simulate_meta_did(), fit the model with enough iterations to get reliable
# posteriors, and verify that the estimates are sensible. They are slower than
# the smoke tests in test-stan.R; run them selectively with
#   devtools::test(filter = "recovery")
#
# skip_if_no_stan() and get_compiled_model() live in helper-stan.R.

# ---------------------------------------------------------------------------
# Fitting helper (more iterations than smoke tests)
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

# ---------------------------------------------------------------------------
# Shared simulation parameters
# ---------------------------------------------------------------------------

# true_effect_normalised is the quantity treatment_effect_mean targets when
# normalise_by_baseline = TRUE. With equal pre-treatment means for both arms
# (simulate_meta_did() default), the Stan model's baseline is fixed at 1 for
# both groups, and:
#
#   treatment_effect_i ≈ DiD_i / mean_baseline = true_effect / mean_baseline
#
# See did_summary_model_functions.stan for the exact likelihood.
TRUE_EFFECT_RAW        <- -0.15
TRUE_SIGMA_EFFECT_RAW  <- 0.03
MEAN_BASELINE          <- 0.45
TRUE_EFFECT_NORMALISED <- TRUE_EFFECT_RAW       / MEAN_BASELINE   # ≈ -0.333
TRUE_SIGMA_NORMALISED  <- TRUE_SIGMA_EFFECT_RAW / MEAN_BASELINE   # ≈ 0.067

sim_data <- simulate_meta_did(
  n_studies    = 25,
  true_effect  = TRUE_EFFECT_RAW,
  sigma_effect = TRUE_SIGMA_EFFECT_RAW,
  baseline_mean = MEAN_BASELINE,
  n_control    = 100L,
  n_treatment  = 100L,
  seed         = 6204L
)

# ---------------------------------------------------------------------------
# DiD summary data: parameter recovery
# ---------------------------------------------------------------------------

test_that("treatment_effect_mean is in the correct direction (DiD summary)", {
  skip_if_no_stan()
  fit    <- recovery_fit(summary_data = as_summary_did(sim_data))
  te     <- summary(fit)
  te_mean <- te$mean[te$parameter == "treatment_effect_mean"]
  expect_true(te_mean < 0, label = "estimated effect is negative")
})

test_that("90% CI covers true normalised treatment_effect_mean (DiD summary)", {
  skip_if_no_stan()
  fit    <- recovery_fit(summary_data = as_summary_did(sim_data))
  te     <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = sprintf("90%% CI [%.3f, %.3f] covers true value %.3f",
                    te_row$lo, te_row$hi, TRUE_EFFECT_NORMALISED)
  )
})

test_that("90% CI excludes zero for a clearly detectable effect (DiD summary)", {
  skip_if_no_stan()
  fit    <- recovery_fit(summary_data = as_summary_did(sim_data))
  te     <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(te_row$hi < 0, label = "entire 90% CI is negative")
})

test_that("treatment_effect_sd 90% CI covers true between-study SD (DiD summary)", {
  skip_if_no_stan()
  fit    <- recovery_fit(summary_data = as_summary_did(sim_data))
  te     <- summary(fit)
  sd_row <- te[te$parameter == "treatment_effect_sd", ]
  expect_true(sd_row$mean > 0, label = "between-study SD posterior is positive")
  expect_true(
    sd_row$lo < TRUE_SIGMA_NORMALISED && sd_row$hi > TRUE_SIGMA_NORMALISED,
    label = sprintf("90%% CI [%.3f, %.3f] covers true SD %.3f",
                    sd_row$lo, sd_row$hi, TRUE_SIGMA_NORMALISED)
  )
})

test_that("key parameters converge (R-hat < 1.05) with DiD summary data", {
  skip_if_no_stan()
  fit  <- recovery_fit(summary_data = as_summary_did(sim_data))
  rhat <- fit$fit$summary(c("treatment_effect_mean", "treatment_effect_sd"))$rhat
  expect_true(all(rhat < 1.05),
              label = paste("R-hat:", paste(round(rhat, 3), collapse = ", ")))
})

# ---------------------------------------------------------------------------
# Individual-level DiD data: parameter recovery
# ---------------------------------------------------------------------------

test_that("treatment_effect_mean is in the correct direction (individual DiD)", {
  skip_if_no_stan()
  fit    <- recovery_fit(individual_data = as_individual_did(sim_data))
  te     <- summary(fit)
  te_mean <- te$mean[te$parameter == "treatment_effect_mean"]
  expect_true(te_mean < 0, label = "estimated effect is negative")
})

test_that("90% CI covers true normalised treatment_effect_mean (individual DiD)", {
  skip_if_no_stan()
  fit    <- recovery_fit(individual_data = as_individual_did(sim_data))
  te     <- summary(fit)
  te_row <- te[te$parameter == "treatment_effect_mean", ]
  expect_true(
    te_row$lo < TRUE_EFFECT_NORMALISED && te_row$hi > TRUE_EFFECT_NORMALISED,
    label = sprintf("90%% CI [%.3f, %.3f] covers true value %.3f",
                    te_row$lo, te_row$hi, TRUE_EFFECT_NORMALISED)
  )
})

# ---------------------------------------------------------------------------
# Consistency check: summary vs individual data should give similar estimates
# ---------------------------------------------------------------------------

test_that("summary and individual DiD estimates are consistent", {
  skip_if_no_stan()
  fit_sum <- recovery_fit(summary_data    = as_summary_did(sim_data))
  fit_ind <- recovery_fit(individual_data = as_individual_did(sim_data))

  te_sum <- summary(fit_sum)$mean[summary(fit_sum)$parameter == "treatment_effect_mean"]
  te_ind <- summary(fit_ind)$mean[summary(fit_ind)$parameter == "treatment_effect_mean"]

  # Posterior means should agree within 0.05 normalised units
  expect_true(
    abs(te_sum - te_ind) < 0.05,
    label = sprintf(
      "summary estimate %.3f and individual estimate %.3f differ by %.3f",
      te_sum, te_ind, abs(te_sum - te_ind)
    )
  )
})
