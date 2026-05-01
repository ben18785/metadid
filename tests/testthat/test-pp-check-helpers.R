# Tests for internal helper functions in R/pp_check.R.
# All tests use hand-crafted mock data — no Stan required.

# ---------------------------------------------------------------------------
# compute_observed_effects() — gap coverage
# ---------------------------------------------------------------------------
# The existing test-pp-check.R covers DiD summary, RCT summary, PP summary,
# and individual DiD. Here we add: did_change, individual RCT, individual PP,
# and mixed data.

test_that("compute_observed_effects: did_change design", {
  sdata <- data.frame(
    study_id = "c1", design = "did_change",
    n_control = 50L, n_treatment = 55L,
    mean_change_control = -0.02, sd_change_control = 0.10,
    mean_change_treatment = -0.20, sd_change_treatment = 0.12,
    stringsAsFactors = FALSE
  )
  fit <- structure(list(
    summary_data = sdata,
    individual_data = NULL,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_student_t_heterogeneity = 0L,
      is_design_effect = 0L
    ),
    method = "sample"
  ), class = "meta_did_fit")

  obs <- compute_observed_effects(fit)
  expect_equal(nrow(obs), 1)
  expect_equal(obs$design_type, "did_change")
  expect_equal(obs$y_obs, -0.20 - (-0.02))
})

test_that("compute_observed_effects: individual RCT", {
  ind <- data.frame(
    study_id = rep("r1", 6),
    design = "rct",
    group = c(rep("control", 3), rep("treatment", 3)),
    time  = "post",
    value = c(0.9, 1.0, 1.1,   # control
              0.6, 0.7, 0.8),  # treatment
    stringsAsFactors = FALSE
  )
  fit <- structure(list(
    summary_data = NULL,
    individual_data = ind,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_student_t_heterogeneity = 0L,
      is_design_effect = 0L
    ),
    method = "sample"
  ), class = "meta_did_fit")

  obs <- compute_observed_effects(fit)
  expect_equal(nrow(obs), 1)
  expect_equal(obs$design_type, "rct_individual")
  expect_equal(obs$y_obs, mean(c(0.6, 0.7, 0.8)) - mean(c(0.9, 1.0, 1.1)))
})

test_that("compute_observed_effects: individual PP", {
  ind <- data.frame(
    study_id = rep("p1", 6),
    design = "pp",
    group = "treatment",
    time  = rep(c("pre", "post"), 3),
    value = c(1.0, 0.8, 1.1, 0.9, 0.9, 0.7),
    stringsAsFactors = FALSE
  )
  fit <- structure(list(
    summary_data = NULL,
    individual_data = ind,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_student_t_heterogeneity = 0L,
      is_design_effect = 0L
    ),
    method = "sample"
  ), class = "meta_did_fit")

  obs <- compute_observed_effects(fit)
  expect_equal(nrow(obs), 1)
  expect_equal(obs$design_type, "pp_individual")
  expect_equal(obs$y_obs, mean(c(0.8, 0.9, 0.7)) - mean(c(1.0, 1.1, 0.9)))
})

test_that("compute_observed_effects: mixed summary + individual data", {
  sdata <- data.frame(
    study_id = "d1", design = "did",
    n_control = 50L, n_treatment = 50L,
    mean_pre_control = 1.0, mean_post_control = 0.93,
    sd_pre_control = 0.25, sd_post_control = 0.24,
    mean_pre_treatment = 1.0, mean_post_treatment = 0.68,
    sd_pre_treatment = 0.27, sd_post_treatment = 0.22,
    rho = 0.5,
    stringsAsFactors = FALSE
  )
  ind <- data.frame(
    study_id = rep("i1", 6),
    design = "rct",
    group = c(rep("control", 3), rep("treatment", 3)),
    time  = "post",
    value = c(0.9, 1.0, 1.1, 0.6, 0.7, 0.8),
    stringsAsFactors = FALSE
  )
  fit <- structure(list(
    summary_data = sdata,
    individual_data = ind,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_student_t_heterogeneity = 0L,
      is_design_effect = 0L
    ),
    method = "sample"
  ), class = "meta_did_fit")

  obs <- compute_observed_effects(fit)
  expect_equal(nrow(obs), 2)
  expect_setequal(obs$design_type, c("did_summary", "rct_individual"))
})

# ---------------------------------------------------------------------------
# build_pop_mean_matrix()
# ---------------------------------------------------------------------------

test_that("build_pop_mean_matrix uses correct param per design family", {
  obs <- data.frame(
    study_id = c("d1", "r1", "p1"),
    design_family = c("did", "rct", "pp"),
    design_type = c("did_summary", "rct_summary", "pp_summary"),
    design_index = 1:3,
    y_obs = c(-0.25, -0.20, -0.15),
    stringsAsFactors = FALSE
  )

  n_draws <- 10
  draws <- matrix(0, nrow = n_draws, ncol = 3)
  colnames(draws) <- c("treatment_effect_mean",
                        "treatment_effect_mean_rct",
                        "treatment_effect_mean_pp")
  draws[, 1] <- -0.30
  draws[, 2] <- -0.20
  draws[, 3] <- -0.40

  mu_mat <- build_pop_mean_matrix(obs, draws)

  expect_equal(dim(mu_mat), c(n_draws, 3))
  # DiD studies → treatment_effect_mean
  expect_true(all(mu_mat[, 1] == -0.30))
  # RCT studies → treatment_effect_mean_rct
  expect_true(all(mu_mat[, 2] == -0.20))
  # PP studies → treatment_effect_mean_pp
  expect_true(all(mu_mat[, 3] == -0.40))
})

# ---------------------------------------------------------------------------
# SE helper functions
# ---------------------------------------------------------------------------

test_that("se_did_summary computes correct SE with known rho", {
  sdata <- data.frame(
    study_id = "d1", design = "did",
    n_control = 100L, n_treatment = 100L,
    mean_pre_control = 1.0, mean_post_control = 0.93,
    sd_pre_control = 0.20, sd_post_control = 0.25,
    mean_pre_treatment = 1.0, mean_post_treatment = 0.68,
    sd_pre_treatment = 0.22, sd_post_treatment = 0.24,
    rho = 0.5,
    stringsAsFactors = FALSE
  )
  draws <- matrix(0, nrow = 1, ncol = 1)
  colnames(draws) <- "dummy"

  result <- se_did_summary(sdata, 1, draws)

  # Hand calculation:
  # var_ctrl_change = 0.20^2 + 0.25^2 - 2*0.5*0.20*0.25 = 0.04 + 0.0625 - 0.05 = 0.0525
  # var_trt_change  = 0.22^2 + 0.24^2 - 2*0.5*0.22*0.24 = 0.0484 + 0.0576 - 0.0528 = 0.0532
  # SE = sqrt(0.0525/100 + 0.0532/100) = sqrt(0.001057) ≈ 0.03251
  expected <- sqrt((0.20^2 + 0.25^2 - 2*0.5*0.20*0.25) / 100 +
                   (0.22^2 + 0.24^2 - 2*0.5*0.22*0.24) / 100)
  expect_equal(result, expected)
})

test_that("se_did_change computes correct SE", {
  sdata <- data.frame(
    study_id = "c1", design = "did_change",
    n_control = 80L, n_treatment = 90L,
    mean_change_control = -0.02, sd_change_control = 0.15,
    mean_change_treatment = -0.20, sd_change_treatment = 0.18,
    stringsAsFactors = FALSE
  )

  result <- se_did_change(sdata, 1)
  expected <- sqrt(0.15^2 / 80 + 0.18^2 / 90)
  expect_equal(result, expected)
})

test_that("se_rct_summary computes correct SE", {
  sdata <- data.frame(
    study_id = "r1", design = "rct",
    n_control = 60L, n_treatment = 70L,
    mean_post_control = 0.9, sd_post_control = 0.20,
    mean_post_treatment = 0.7, sd_post_treatment = 0.22,
    stringsAsFactors = FALSE
  )

  result <- se_rct_summary(sdata, 1)
  expected <- sqrt(0.20^2 / 60 + 0.22^2 / 70)
  expect_equal(result, expected)
})

test_that("se_pp_summary computes correct SE with known rho", {
  sdata <- data.frame(
    study_id = "p1", design = "pp",
    n_treatment = 50L,
    mean_pre_treatment = 1.0, sd_pre_treatment = 0.25,
    mean_post_treatment = 0.85, sd_post_treatment = 0.22,
    rho = 0.6,
    stringsAsFactors = FALSE
  )
  draws <- matrix(0, nrow = 1, ncol = 1)
  colnames(draws) <- "dummy"

  result <- se_pp_summary(sdata, 1, draws)
  expected <- sqrt((0.25^2 + 0.22^2 - 2*0.6*0.25*0.22) / 50)
  expect_equal(result, expected)
})

# ---------------------------------------------------------------------------
# individual_sample_sizes()
# ---------------------------------------------------------------------------

test_that("individual_sample_sizes extracts correct counts for DiD", {
  idata <- data.frame(
    study_id = rep(c("s1", "s2"), each = 12),
    design = "did",
    group = rep(c(rep("control", 6), rep("treatment", 6)), 2),
    time  = rep(rep(c("pre", "post"), each = 3), 4),
    value = rnorm(24),
    stringsAsFactors = FALSE
  )
  # study s1, design_idx = 1: 3 control subjects (pre), 3 treatment (pre)
  ns <- individual_sample_sizes(idata, "did", 1)
  expect_equal(ns$n_c, 3)
  expect_equal(ns$n_t, 3)
})

test_that("individual_sample_sizes extracts correct counts for RCT (post-only)", {
  idata <- data.frame(
    study_id = rep("r1", 8),
    design = "rct",
    group = c(rep("control", 4), rep("treatment", 4)),
    time  = "post",
    value = rnorm(8),
    stringsAsFactors = FALSE
  )
  # No pre rows, so fallback to post counts
  ns <- individual_sample_sizes(idata, "rct", 1)
  expect_equal(ns$n_c, 4)
  expect_equal(ns$n_t, 4)
})

test_that("individual_sample_sizes extracts correct counts for PP", {
  idata <- data.frame(
    study_id = rep("p1", 10),
    design = "pp",
    group = "treatment",
    time  = rep(c("pre", "post"), each = 5),
    value = rnorm(10),
    stringsAsFactors = FALSE
  )
  ns <- individual_sample_sizes(idata, "pp", 1)
  expect_equal(ns$n_t, 5)
})

# ---------------------------------------------------------------------------
# get_missing_rho_draws()
# ---------------------------------------------------------------------------

test_that("get_missing_rho_draws applies tanh transform correctly", {
  n_draws <- 20
  z_vals <- seq(-1, 1, length.out = n_draws)

  draws <- matrix(z_vals, nrow = n_draws, ncol = 1)
  colnames(draws) <- "z_rho_missing_did_summary[1]"

  # All rho values are NA, so study 1 is the 1st missing

  design_data <- data.frame(
    study_id = "d1",
    rho = NA_real_,
    stringsAsFactors = FALSE
  )

  result <- get_missing_rho_draws(draws, design_data, 1, "z_rho_missing_did_summary")
  expect_equal(result, tanh(z_vals))
})

test_that("get_missing_rho_draws handles mixed known/missing rho", {
  n_draws <- 5
  z_vals <- rep(0.5, n_draws)

  draws <- matrix(z_vals, nrow = n_draws, ncol = 1)
  colnames(draws) <- "z_rho_missing_did_summary[1]"

  # 3 studies: first has rho, second and third are missing
  design_data <- data.frame(
    study_id = c("d1", "d2", "d3"),
    rho = c(0.5, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )

  # study_idx = 2 is the 1st missing study
  result <- get_missing_rho_draws(draws, design_data, 2, "z_rho_missing_did_summary")
  expect_equal(result, tanh(z_vals))
})
