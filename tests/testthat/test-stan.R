# Tests for Stan model integration.
#
# These are smoke tests: the goal is to confirm the model compiles, samples
# without error, and returns a structurally sound result — not to validate
# posterior values. See test-recovery.R for parameter recovery tests.
#
# skip_if_no_stan() and get_compiled_model() live in helper-stan.R and are
# sourced automatically by testthat before this file runs.

# ---------------------------------------------------------------------------
# Helpers: minimal datasets and a low-iteration fitting wrapper
# ---------------------------------------------------------------------------

quick_fit <- function(summary_data = NULL, individual_data = NULL, ...) {
  model <- get_compiled_model()
  skip_if(is.null(model), "Stan model could not be compiled")
  # Inject the compiled model so meta_did() doesn't call instantiate, which
  # only works against the installed package — not devtools::load_all().
  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )
  meta_did(
    summary_data    = summary_data,
    individual_data = individual_data,
    chains          = 1L,
    iter_warmup     = 200L,
    iter_sampling   = 100L,
    seed            = 4917L,
    refresh         = 0,
    ...
  )
}

did_summary <- function() {
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
    sd_post_treatment   = c(0.10, 0.09, 0.08)
  )
}

rct_summary <- function() {
  data.frame(
    study_id            = c("R1", "R2"),
    design              = "rct",
    n_control           = c(60L, 70L),
    mean_post_control   = c(0.48, 0.52),
    sd_post_control     = c(0.12, 0.11),
    n_treatment         = c(65L, 72L),
    mean_post_treatment = c(0.35, 0.38),
    sd_post_treatment   = c(0.11, 0.10)
  )
}

individual_did <- function(n_per_group = 5) {
  set.seed(8321)
  data.frame(
    study_id = rep("ind_study", n_per_group * 4),
    design   = "did",
    group    = rep(c("control", "treatment"), each = n_per_group * 2),
    time     = rep(rep(c("pre", "post"), each = n_per_group), 2),
    value    = c(
      rnorm(n_per_group, 0.45, 0.12),  # control pre
      rnorm(n_per_group, 0.42, 0.11),  # control post
      rnorm(n_per_group, 0.46, 0.13),  # treatment pre
      rnorm(n_per_group, 0.30, 0.10)   # treatment post
    )
  )
}

# ---------------------------------------------------------------------------
# Smoke tests: model runs without error
# ---------------------------------------------------------------------------

test_that("meta_did() runs on DiD summary data", {
  skip_if_no_stan()
  expect_no_error(quick_fit(summary_data = did_summary()))
})

test_that("meta_did() runs on RCT summary data", {
  skip_if_no_stan()
  expect_no_error(quick_fit(summary_data = rct_summary()))
})

test_that("meta_did() runs on mixed DiD + RCT summary data", {
  skip_if_no_stan()
  mixed <- dplyr::bind_rows(did_summary(), rct_summary())
  expect_no_error(quick_fit(summary_data = mixed))
})

test_that("meta_did() runs on individual-level DiD data", {
  skip_if_no_stan()
  expect_no_error(quick_fit(individual_data = individual_did()))
})

test_that("meta_did() runs with summary + individual data combined", {
  skip_if_no_stan()
  expect_no_error(
    quick_fit(summary_data = rct_summary(), individual_data = individual_did())
  )
})

# ---------------------------------------------------------------------------
# Output structure tests
# ---------------------------------------------------------------------------

test_that("meta_did() returns a meta_did_fit object", {
  skip_if_no_stan()
  fit <- quick_fit(summary_data = did_summary())
  expect_s3_class(fit, "meta_did_fit")
})

test_that("summary() returns a data frame with expected columns", {
  skip_if_no_stan()
  fit <- quick_fit(summary_data = did_summary())
  s   <- summary(fit)
  expect_true(is.data.frame(s))
  expect_true(all(c("parameter", "mean", "sd", "lo", "hi") %in% names(s)))
})

test_that("summary() includes population-level parameters", {
  skip_if_no_stan()
  fit    <- quick_fit(summary_data = did_summary())
  params <- summary(fit)$parameter
  expect_true("treatment_effect_mean" %in% params)
  expect_true("treatment_effect_sd" %in% params)
})

# ---------------------------------------------------------------------------
# Posterior sanity tests
# ---------------------------------------------------------------------------

test_that("posterior means are all finite", {
  skip_if_no_stan()
  fit <- quick_fit(summary_data = did_summary())
  s   <- summary(fit)
  expect_true(all(is.finite(s$mean)), label = "all posterior means finite")
  expect_true(all(is.finite(s$sd)),   label = "all posterior SDs finite")
})

test_that("credible intervals are ordered (lo < hi)", {
  skip_if_no_stan()
  fit <- quick_fit(summary_data = did_summary())
  s   <- summary(fit)
  expect_true(all(s$lo < s$hi))
})

# ---------------------------------------------------------------------------
# Flag / option tests
# ---------------------------------------------------------------------------

test_that("robust_heterogeneity = TRUE runs without error", {
  skip_if_no_stan()
  expect_no_error(
    quick_fit(summary_data = did_summary(), robust_heterogeneity = TRUE)
  )
})

test_that("design_effects = TRUE runs without error on mixed data", {
  skip_if_no_stan()
  mixed <- dplyr::bind_rows(did_summary(), rct_summary())
  expect_no_error(
    quick_fit(summary_data = mixed, design_effects = TRUE)
  )
})

test_that("normalise_by_baseline = FALSE runs without error", {
  skip_if_no_stan()
  expect_no_error(
    quick_fit(summary_data = did_summary(), normalise_by_baseline = FALSE)
  )
})

# ---------------------------------------------------------------------------
# Optimization (MAP) tests
# ---------------------------------------------------------------------------

# Thin wrapper: optimization needs no warmup/sampling args.
quick_optimize <- function(summary_data = NULL, individual_data = NULL, ...) {
  model <- get_compiled_model()
  skip_if(is.null(model), "Stan model could not be compiled")
  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )
  meta_did(
    summary_data    = summary_data,
    individual_data = individual_data,
    method          = "optimize",
    seed            = 4917L,
    refresh         = 0,
    ...
  )
}

test_that("method = 'optimize' runs without error on DiD summary data", {
  skip_if_no_stan()
  expect_no_error(quick_optimize(summary_data = did_summary()))
})

test_that("method = 'optimize' runs without error on mixed data", {
  skip_if_no_stan()
  mixed <- dplyr::bind_rows(did_summary(), rct_summary())
  expect_no_error(quick_optimize(summary_data = mixed))
})

test_that("method = 'optimize' returns a meta_did_fit with method 'optimize'", {
  skip_if_no_stan()
  fit <- quick_optimize(summary_data = did_summary())
  expect_s3_class(fit, "meta_did_fit")
  expect_equal(fit$method, "optimize")
})

test_that("summary() returns NA sd/lo/hi for optimize fit", {
  skip_if_no_stan()
  fit <- quick_optimize(summary_data = did_summary())
  s   <- summary(fit)
  expect_true(is.data.frame(s))
  expect_true(all(is.na(s$sd)))
  expect_true(all(is.na(s$lo)))
  expect_true(all(is.na(s$hi)))
})

test_that("summary() MAP estimate is finite for optimize fit", {
  skip_if_no_stan()
  fit <- quick_optimize(summary_data = did_summary())
  s   <- summary(fit)
  expect_true(all(is.finite(s$mean)), label = "all MAP estimates are finite")
})
