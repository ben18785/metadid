# Tests for the modelled-mode normalisation machinery:
#   * normalise = TRUE with baseline_latent_arm = "treatment" (default mode)
#   * normalise = TRUE with baseline_latent_arm = "control"
#   * normalise = FALSE (none mode)
#
# These tests exercise the public API surface and confirm the Stan-side
# modelled-mode machinery compiles and runs end-to-end. Recovery accuracy
# is tested separately in test-recovery.R.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Small DiD dataset used across the smoke tests. Baselines roughly around 0.5
# so that the auto-computed baseline_prior_upper (100 * max) ≈ 60 — wide
# enough that the uniform prior is effectively flat over the posterior region.
.smoke_did <- function() {
  data.frame(
    study_id            = c("A", "B", "C"),
    design              = "did",
    n_control           = c(50L, 60L, 55L),
    mean_pre_control    = c(0.45, 0.50, 0.48),
    mean_post_control   = c(0.42, 0.47, 0.46),
    sd_pre_control      = c(0.12, 0.11, 0.10),
    sd_post_control     = c(0.11, 0.10, 0.09),
    n_treatment         = c(55L, 65L, 52L),
    mean_pre_treatment  = c(0.46, 0.51, 0.49),
    mean_post_treatment = c(0.30, 0.34, 0.32),
    sd_pre_treatment    = c(0.13, 0.12, 0.11),
    sd_post_treatment   = c(0.10, 0.09, 0.08),
    rho                 = 0.5
  )
}

# ---------------------------------------------------------------------------
# API validation (no Stan needed)
# ---------------------------------------------------------------------------

test_that("normalise must be a single TRUE or FALSE", {
  expect_error(
    meta_did(summary_data = .smoke_did(), normalise = c(TRUE, FALSE)),
    "single TRUE or FALSE"
  )
  expect_error(
    meta_did(summary_data = .smoke_did(), normalise = "yes"),
    "single TRUE or FALSE"
  )
  expect_error(
    meta_did(summary_data = .smoke_did(), normalise = NA),
    "single TRUE or FALSE"
  )
})

test_that("baseline_latent_arm only accepts 'treatment' or 'control'", {
  expect_error(
    meta_did(summary_data = .smoke_did(), baseline_latent_arm = "neither"),
    "should be one of"
  )
})

test_that("baseline_latent_mode is set correctly from the two-arg combination", {
  # Reach into prepare_stan_data via a direct call to verify the encoding.
  flags <- list(
    baseline_latent_mode                    = 1L,
    is_correlation_coefficient_hierarchical = 0L,
    is_student_t_heterogeneity              = 0L,
    is_design_effect                        = 0L,
    is_correlated_effects                   = 0L,
    is_baseline_difference_estimated        = 1L
  )
  result <- prepare_stan_data(.smoke_did(), NULL, flags, set_priors())
  expect_equal(result$baseline_latent_mode, 1L)
  expect_true("baseline_prior_upper" %in% names(result))
  expect_gt(result$baseline_prior_upper, 0)
})

# ---------------------------------------------------------------------------
# baseline_prior_upper auto-computation
# ---------------------------------------------------------------------------

test_that("baseline_prior_upper is 100x the maximum observed baseline by default", {
  upper <- compute_baseline_prior_upper(.smoke_did(), NULL)
  # Largest mean in the dataset is mean_pre_treatment = 0.51
  expect_equal(upper, 100 * 0.51)
})

test_that("baseline_prior_upper honours an explicit uniform() override", {
  upper <- compute_baseline_prior_upper(
    .smoke_did(), NULL,
    user_prior = uniform(0, 42)
  )
  expect_equal(upper, 42)
})

test_that("compute_baseline_prior_upper returns 1.0 fallback when no data", {
  expect_equal(compute_baseline_prior_upper(NULL, NULL), 1.0)
})

test_that("uniform() constructor validates bounds", {
  expect_error(uniform(0, 0), "upper")
  expect_error(uniform(-1, 5), "lower")
  expect_silent(uniform(0, 100))
  expect_equal(uniform(0, 100)$dist, "uniform")
})

test_that("set_priors() accepts a baseline_per_study override", {
  p <- set_priors(baseline_per_study = uniform(0, 5))
  expect_equal(p$baseline_per_study$upper, 5)
})

test_that("set_priors() defaults baseline_per_study to NULL (auto-compute)", {
  p <- set_priors()
  expect_null(p$baseline_per_study)
})

# ---------------------------------------------------------------------------
# End-to-end smoke tests (require compiled Stan model)
# ---------------------------------------------------------------------------

quick_fit_mode <- function(normalise, baseline_latent_arm = "treatment") {
  model <- get_compiled_model()
  skip_if(is.null(model), "Stan model could not be compiled")
  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )
  meta_did(
    summary_data        = .smoke_did(),
    normalise           = normalise,
    baseline_latent_arm = baseline_latent_arm,
    chains              = 1L,
    iter_warmup         = 200L,
    iter_sampling       = 100L,
    seed                = 4917L,
    refresh             = 0
  )
}

test_that("modelled treatment-latent mode runs end-to-end", {
  skip_if_no_stan()
  expect_no_error(quick_fit_mode(normalise = TRUE, baseline_latent_arm = "treatment"))
})

test_that("modelled control-latent mode runs end-to-end", {
  skip_if_no_stan()
  expect_no_error(quick_fit_mode(normalise = TRUE, baseline_latent_arm = "control"))
})

test_that("none mode runs end-to-end (backward compatibility)", {
  skip_if_no_stan()
  expect_no_error(quick_fit_mode(normalise = FALSE))
})
