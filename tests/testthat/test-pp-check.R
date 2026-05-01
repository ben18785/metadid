# Tests for posterior predictive check functions.

# ============================================================
# Non-Stan tests (mock objects)
# ============================================================

mock_summary_data <- data.frame(
  study_id = c("s1", "s2"),
  design = c("did", "did"),
  n_control = c(50L, 60L),
  n_treatment = c(55L, 65L),
  mean_pre_control = c(1.0, 1.0),
  mean_post_control = c(0.93, 0.95),
  sd_pre_control = c(0.25, 0.28),
  sd_post_control = c(0.24, 0.27),
  mean_pre_treatment = c(1.0, 1.0),
  mean_post_treatment = c(0.68, 0.72),
  sd_pre_treatment = c(0.27, 0.29),
  sd_post_treatment = c(0.22, 0.25),
  rho = c(0.5, 0.6),
  stringsAsFactors = FALSE
)

mock_fit_sample <- structure(
  list(
    summary_data = mock_summary_data,
    individual_data = NULL,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_correlation_coefficient_hierarchical = 1L,
      is_student_t_heterogeneity = 0L,
      is_design_effect = 0L
    ),
    method = "sample"
  ),
  class = "meta_did_fit"
)

mock_fit_map <- mock_fit_sample
mock_fit_map$method <- "optimize"


test_that("compute_observed_effects returns correct DiD values", {
  obs <- compute_observed_effects(mock_fit_sample)

  expect_equal(nrow(obs), 2)
  expect_equal(obs$study_id, c("s1", "s2"))
  expect_equal(obs$design_type, rep("did_summary", 2))

  # s1: (0.68 - 1.0) - (0.93 - 1.0) = -0.32 - (-0.07) = -0.25
  expect_equal(obs$y_obs[1], -0.25)
  # s2: (0.72 - 1.0) - (0.95 - 1.0) = -0.28 - (-0.05) = -0.23
  expect_equal(obs$y_obs[2], -0.23)
})


test_that("compute_observed_effects handles RCT and PP", {
  rct_data <- data.frame(
    study_id = "r1", design = "rct",
    n_control = 40L, n_treatment = 45L,
    mean_post_control = 0.9, mean_post_treatment = 0.7,
    sd_post_control = 0.2, sd_post_treatment = 0.18,
    stringsAsFactors = FALSE
  )
  pp_data <- data.frame(
    study_id = "p1", design = "pp",
    n_treatment = 50L,
    mean_pre_treatment = 1.0, mean_post_treatment = 0.85,
    sd_pre_treatment = 0.3, sd_post_treatment = 0.25,
    rho = 0.5,
    stringsAsFactors = FALSE
  )

  fit <- structure(list(
    summary_data = dplyr::bind_rows(rct_data, pp_data),
    individual_data = NULL,
    model_flags = mock_fit_sample$model_flags,
    method = "sample"
  ), class = "meta_did_fit")

  obs <- compute_observed_effects(fit)
  expect_equal(obs$study_id, c("r1", "p1"))
  expect_equal(obs$y_obs[1], 0.7 - 0.9)
  expect_equal(obs$y_obs[2], 0.85 - 1.0)
})


test_that("compute_observed_effects handles individual DiD data", {
  ind <- data.frame(
    study_id = rep("i1", 8),
    design = "did",
    group = rep(c("control", "treatment"), each = 4),
    time = rep(c("pre", "post"), 4),
    value = c(1.0, 0.9, 1.1, 1.0,   # control pre, post, pre, post
              1.0, 0.7, 1.1, 0.8),   # treatment pre, post, pre, post
    stringsAsFactors = FALSE
  )

  fit <- structure(list(
    summary_data = NULL,
    individual_data = ind,
    model_flags = mock_fit_sample$model_flags,
    method = "sample"
  ), class = "meta_did_fit")

  obs <- compute_observed_effects(fit)
  expect_equal(nrow(obs), 1)
  expect_equal(obs$design_type, "did_individual")

  ctrl_pre  <- mean(c(1.0, 1.1))
  ctrl_post <- mean(c(0.9, 1.0))
  trt_pre   <- mean(c(1.0, 1.1))
  trt_post  <- mean(c(0.7, 0.8))
  expected  <- (trt_post - trt_pre) - (ctrl_post - ctrl_pre)
  expect_equal(obs$y_obs, expected)
})


test_that("pp_check_effects errors for MAP fits", {
  expect_error(
    pp_check_effects(mock_fit_map),
    "method = 'sample'"
  )
})


test_that("pp_check_cdf errors for MAP fits", {
  expect_error(
    pp_check_cdf(mock_fit_map),
    "method = 'sample'"
  )
})


test_that("pp_check_cdf individual errors when no individual data", {
  expect_error(
    pp_check_cdf(mock_fit_sample, type = "individual"),
    "individual-level data"
  )
})


# ============================================================
# Stan integration tests
# ============================================================

test_that("pp_check_effects runs on MCMC fit with summary data", {
  skip_if_no_stan()

  model <- get_compiled_model()

  sim <- simulate_meta_did(
    n_studies = 5, n_control = 50, n_treatment = 50,
    true_effect = -0.15, sigma_effect = 0.03,
    baseline_mean = 0.45, rho = 0.5, seed = 7831
  )
  studies <- as_summary_did(sim)

  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )

  fit <- meta_did(
    summary_data = studies,
    method = "sample",
    seed = 7831,
    chains = 2,
    iter_warmup = 200,
    iter_sampling = 200,
    refresh = 0
  )

  p <- pp_check_effects(fit)
  expect_s3_class(p, "ggplot")

  # Test study_id filter
  p2 <- pp_check_effects(fit, study_id = studies$study_id[1:2])
  expect_s3_class(p2, "ggplot")
})


test_that("pp_check_cdf summary runs on MCMC fit with summary data", {
  skip_if_no_stan()

  model <- get_compiled_model()

  sim <- simulate_meta_did(
    n_studies = 5, n_control = 50, n_treatment = 50,
    true_effect = -0.15, sigma_effect = 0.03,
    baseline_mean = 0.45, rho = 0.5, seed = 7831
  )
  studies <- as_summary_did(sim)

  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )

  fit <- meta_did(
    summary_data = studies,
    method = "sample",
    seed = 7831,
    chains = 2,
    iter_warmup = 200,
    iter_sampling = 200,
    refresh = 0
  )

  p <- pp_check_cdf(fit, type = "summary")
  expect_s3_class(p, "ggplot")

  # Test study_id filter
  p2 <- pp_check_cdf(fit, type = "summary",
                      study_id = studies$study_id[1:2])
  expect_s3_class(p2, "ggplot")
})


test_that("pp_check_cdf runs on MCMC fit with individual data", {
  skip_if_no_stan()

  model <- get_compiled_model()

  sim <- simulate_meta_did(
    n_studies = 3, n_control = 30, n_treatment = 30,
    true_effect = -0.15, sigma_effect = 0.03,
    baseline_mean = 0.45, rho = 0.5, seed = 2194
  )
  ind <- as_individual_did(sim)

  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )

  fit <- meta_did(
    individual_data = ind,
    method = "sample",
    seed = 2194,
    chains = 2,
    iter_warmup = 200,
    iter_sampling = 200,
    refresh = 0
  )

  p <- pp_check_cdf(fit)
  expect_s3_class(p, "ggplot")

  # Test study_id filter
  p2 <- pp_check_cdf(fit, study_id = unique(ind$study_id)[1])
  expect_s3_class(p2, "ggplot")
})
