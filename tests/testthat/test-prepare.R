# Tests for R/prepare.R

# ---------------------------------------------------------------------------
# filter_design()
# ---------------------------------------------------------------------------

test_that("filter_design() returns empty data frame for NULL", {
  result <- filter_design(NULL, "did")
  expect_equal(nrow(result), 0)
})

test_that("filter_design() filters correctly", {
  df <- data.frame(
    study_id = c("A", "B", "C"),
    design   = c("did", "rct", "did")
  )
  result <- filter_design(df, "did")
  expect_equal(nrow(result), 2)
  expect_equal(result$study_id, c("A", "C"))
})

test_that("filter_design() returns empty when no match", {
  df <- data.frame(study_id = "A", design = "rct")
  result <- filter_design(df, "did")
  expect_equal(nrow(result), 0)
})

# ---------------------------------------------------------------------------
# split_rho()
# ---------------------------------------------------------------------------

test_that("split_rho() handles all known", {
  r <- split_rho(c(0.5, 0.7, 0.9))
  expect_equal(r$n_known, 3)
  expect_equal(r$n_missing, 0)
  expect_equal(r$idx_known, 1:3)
  expect_equal(r$idx_missing, integer(0))
  expect_equal(r$rho_known, c(0.5, 0.7, 0.9))
})

test_that("split_rho() handles all missing", {
  r <- split_rho(c(NA, NA))
  expect_equal(r$n_known, 0)
  expect_equal(r$n_missing, 2)
  expect_equal(r$idx_known, integer(0))
  expect_equal(r$idx_missing, 1:2)
})

test_that("split_rho() handles mix of known and missing", {
  r <- split_rho(c(0.6, NA, 0.8, NA))
  expect_equal(r$n_known, 2)
  expect_equal(r$n_missing, 2)
  expect_equal(r$idx_known, c(1L, 3L))
  expect_equal(r$idx_missing, c(2L, 4L))
  expect_equal(r$rho_known, c(0.6, 0.8))
})

# ---------------------------------------------------------------------------
# adapt_individual()
# ---------------------------------------------------------------------------

test_that("adapt_individual() returns empty for NULL input", {
  expect_equal(nrow(adapt_individual(NULL)), 0)
})

test_that("adapt_individual() pivots DiD long to wide using subject_id", {
  df <- data.frame(
    study_id   = rep("S1", 4),
    subject_id = rep(1, 4),
    design     = "did",
    group      = rep(c("control", "treatment"), each = 2),
    time       = rep(c("pre", "post"), 2),
    value      = c(10, 12, 11, 8)
  )
  result <- adapt_individual(df)
  expect_true("type" %in% names(result))
  expect_true("before" %in% names(result))
  expect_true("after" %in% names(result))
  expect_equal(nrow(result), 2)

  ctrl <- result[result$type == "control", ]
  expect_equal(ctrl$before, 10)
  expect_equal(ctrl$after, 12)
})

test_that("adapt_individual() pairs by subject_id, not row order", {
  # Subject 1 has pre=10, post=15; Subject 2 has pre=20, post=25
  # Rows are deliberately shuffled so row_number() would mismatch
  df <- data.frame(
    study_id   = "S1",
    subject_id = c(2, 1, 1, 2),
    design     = "did",
    group      = "control",
    time       = c("pre", "post", "pre", "post"),
    value      = c(20, 15, 10, 25)
  )
  result <- adapt_individual(df)
  # Sort by before value to get deterministic order
  result <- result[order(result$before), ]
  expect_equal(result$before, c(10, 20))
  expect_equal(result$after,  c(15, 25))
})

test_that("adapt_individual() works for RCT without subject_id", {
  df <- data.frame(
    study_id = rep("S1", 4),
    design   = "rct",
    group    = rep(c("control", "treatment"), each = 2),
    time     = "post",
    value    = c(50, 60, 40, 35)
  )
  result <- adapt_individual(df)
  expect_equal(nrow(result), 4)
})

# ---------------------------------------------------------------------------
# Null Stan data functions
# ---------------------------------------------------------------------------

test_that("null_stan_data_did() returns n_studies = 0", {
  nd <- null_stan_data_did()
  expect_equal(nd$n_studies_did, 0L)
  expect_equal(length(nd$x_control_before_did), 0)
})

test_that("null_stan_data_rct_summary() returns n_studies = 0", {
  nd <- null_stan_data_rct_summary()
  expect_equal(nd$n_studies_rct_summary, 0L)
})

test_that("null_stan_data_did_change_only() returns n_studies = 0", {
  nd <- null_stan_data_did_change_only()
  expect_equal(nd$n_studies_did_change_only, 0L)
})

# ---------------------------------------------------------------------------
# Summary adapters
# ---------------------------------------------------------------------------

test_that("adapt_summary_did() produces correct Stan data list", {
  df <- data.frame(
    study_id            = c("S1", "S2"),
    design              = "did",
    n_control           = c(50L, 60L),
    mean_pre_control    = c(0.45, 0.50),
    mean_post_control   = c(0.42, 0.48),
    sd_pre_control      = c(0.12, 0.13),
    sd_post_control     = c(0.11, 0.12),
    n_treatment         = c(55L, 65L),
    mean_pre_treatment  = c(0.46, 0.51),
    mean_post_treatment = c(0.30, 0.35),
    sd_pre_treatment    = c(0.13, 0.14),
    sd_post_treatment   = c(0.10, 0.11),
    rho                 = c(0.75, NA)
  )
  result <- adapt_summary_did(df)
  expect_equal(result$n_studies_did_summary, 2)
  expect_equal(result$x_bar_control_before_did_summary, c(0.45, 0.50))
  expect_equal(result$sample_size_control_did_summary, c(50L, 60L))
  # rho split

  expect_equal(result$n_rho_known_did_summary, 1)
  expect_equal(result$n_rho_missing_did_summary, 1)
  expect_equal(result$idx_rho_known_did_summary, 1L)
  expect_equal(result$idx_rho_missing_did_summary, 2L)
  expect_equal(result$rho_known_did_summary, 0.75)
})

test_that("adapt_summary_did() handles no rho column", {
  df <- data.frame(
    study_id            = "S1",
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
    sd_post_treatment   = 0.10
  )
  result <- adapt_summary_did(df)
  # All rho should be missing
  expect_equal(result$n_rho_known_did_summary, 0)
  expect_equal(result$n_rho_missing_did_summary, 1)
})

test_that("adapt_summary_rct() produces correct Stan data list", {
  df <- data.frame(
    study_id            = "R1",
    design              = "rct",
    n_control           = 60L,
    mean_post_control   = 0.48,
    sd_post_control     = 0.12,
    n_treatment         = 65L,
    mean_post_treatment = 0.35,
    sd_post_treatment   = 0.11
  )
  result <- adapt_summary_rct(df)
  expect_equal(result$n_studies_rct_summary, 1)
  expect_equal(result$x_bar_control_after_rct_summary, 0.48)
  expect_equal(result$x_bar_treatment_after_rct_summary, 0.35)
})

test_that("adapt_summary_pp() handles rho correctly", {
  df <- data.frame(
    study_id            = c("P1", "P2"),
    design              = "pp",
    n_treatment         = c(70L, 80L),
    mean_pre_treatment  = c(0.48, 0.50),
    mean_post_treatment = c(0.32, 0.33),
    sd_pre_treatment    = c(0.12, 0.11),
    sd_post_treatment   = c(0.09, 0.08),
    rho                 = c(0.68, NA)
  )
  result <- adapt_summary_pp(df)
  expect_equal(result$n_studies_pp_summary, 2)
  expect_equal(result$n_rho_known_pp_summary, 1)
  expect_equal(result$n_rho_missing_pp_summary, 1)
})

test_that("adapt_summary_did_change() produces correct Stan data", {
  df <- data.frame(
    study_id              = "DC1",
    design                = "did_change",
    n_control             = 40L,
    mean_change_control   = -0.02,
    sd_change_control     = 0.08,
    n_treatment           = 45L,
    mean_change_treatment = -0.10,
    sd_change_treatment   = 0.09
  )
  result <- adapt_summary_did_change(df)
  expect_equal(result$n_studies_did_change_only, 1)
  expect_equal(result$x_bar_change_control_did_change_only, -0.02)
  expect_equal(result$sample_size_treatment_did_change_only, 45L)
})

test_that("adapt_summary_did_change() returns null for empty data", {
  df <- data.frame(design = character(0))
  result <- adapt_summary_did_change(df)
  expect_equal(result$n_studies_did_change_only, 0L)
})

# ---------------------------------------------------------------------------
# Individual-level prep
# ---------------------------------------------------------------------------

test_that("prepare_individual_did() handles empty data", {
  result <- prepare_individual_did(data.frame())
  expect_equal(result$n_studies_did, 0L)
})

test_that("prepare_individual_did() produces correct structure", {
  df <- data.frame(
    study_id = rep("S1", 6),
    type     = rep(c("control", "treatment"), each = 3),
    before   = c(10, 11, 12, 10, 11, 9),
    after    = c(12, 13, 14, 8, 7, 6)
  )
  result <- prepare_individual_did(df)
  expect_equal(result$n_studies_did, 1)
  expect_equal(result$sample_size_control_did, 3L)
  expect_equal(result$sample_size_treatment_did, 3L)
  expect_equal(result$study_start_control_did, 1L)
  expect_equal(result$study_end_control_did, 3L)
  expect_equal(result$x_control_before_did, c(10, 11, 12))
  expect_equal(result$x_treatment_after_did, c(8, 7, 6))
})

test_that("prepare_individual_did() handles multiple studies", {
  df <- data.frame(
    study_id = c(rep("S1", 4), rep("S2", 6)),
    type     = c(rep(c("control", "treatment"), each = 2),
                 rep(c("control", "treatment"), each = 3)),
    before   = 1:10,
    after    = 11:20
  )
  result <- prepare_individual_did(df)
  expect_equal(result$n_studies_did, 2)
  expect_equal(result$sample_size_control_did, c(2L, 3L))
  expect_equal(result$study_start_control_did, c(1L, 3L))
  expect_equal(result$study_end_control_did, c(2L, 5L))
})

test_that("prepare_individual_rct() handles empty data", {
  result <- prepare_individual_rct(data.frame())
  expect_equal(result$n_studies_rct, 0L)
})

test_that("prepare_individual_pp() handles empty data", {
  result <- prepare_individual_pp(data.frame())
  expect_equal(result$n_studies_pp, 0L)
})

# ---------------------------------------------------------------------------
# Out-of-order study_id regression tests
# ---------------------------------------------------------------------------

test_that("prepare_individual_did() is correct when study_id arrives out of order", {
  # S2 rows appear before S1 rows

  df <- data.frame(
    study_id = c(rep("S2", 6), rep("S1", 4)),
    type     = c(rep(c("control", "treatment"), each = 3),
                 rep(c("control", "treatment"), each = 2)),
    before   = c(20, 21, 22, 30, 31, 32, 1, 2, 10, 11),
    after    = c(23, 24, 25, 33, 34, 35, 3, 4, 12, 13)
  )
  result <- prepare_individual_did(df)

  # S1 (2 control, 2 treatment) should come first after sorting

  expect_equal(result$n_studies_did, 2)
  expect_equal(result$sample_size_control_did, c(2L, 3L))
  expect_equal(result$sample_size_treatment_did, c(2L, 3L))

  # Indices into data vectors

  expect_equal(result$study_start_control_did, c(1L, 3L))
  expect_equal(result$study_end_control_did, c(2L, 5L))
  expect_equal(result$study_start_treatment_did, c(1L, 3L))
  expect_equal(result$study_end_treatment_did, c(2L, 5L))

  # Data vectors should be sorted: S1 values then S2 values

  expect_equal(result$x_control_before_did, c(1, 2, 20, 21, 22))
  expect_equal(result$x_control_after_did, c(3, 4, 23, 24, 25))
  expect_equal(result$x_treatment_before_did, c(10, 11, 30, 31, 32))
  expect_equal(result$x_treatment_after_did, c(12, 13, 33, 34, 35))
})

test_that("prepare_individual_rct() is correct when study_id arrives out of order", {
  df <- data.frame(
    study_id = c(rep("S2", 4), rep("S1", 2)),
    type     = c(rep(c("control", "treatment"), each = 2),
                 rep(c("control", "treatment"), each = 1)),
    after    = c(20, 21, 30, 31, 1, 10)
  )
  result <- prepare_individual_rct(df)

  expect_equal(result$n_studies_rct, 2)
  expect_equal(result$sample_size_control_rct, c(1L, 2L))
  expect_equal(result$sample_size_treatment_rct, c(1L, 2L))
  expect_equal(result$study_start_control_rct, c(1L, 2L))
  expect_equal(result$study_end_control_rct, c(1L, 3L))
  expect_equal(result$x_control_after_rct, c(1, 20, 21))
  expect_equal(result$x_treatment_after_rct, c(10, 30, 31))
})

test_that("prepare_individual_pp() is correct when study_id arrives out of order", {
  df <- data.frame(
    study_id = c(rep("S2", 3), rep("S1", 2)),
    type     = rep("treatment", 5),
    before   = c(20, 21, 22, 1, 2),
    after    = c(30, 31, 32, 3, 4)
  )
  result <- prepare_individual_pp(df)

  expect_equal(result$n_studies_pp, 2)
  expect_equal(result$sample_size_treatment_pp, c(2L, 3L))
  expect_equal(result$study_start_treatment_pp, c(1L, 3L))
  expect_equal(result$study_end_treatment_pp, c(2L, 5L))
  expect_equal(result$x_treatment_before_pp, c(1, 2, 20, 21, 22))
  expect_equal(result$x_treatment_after_pp, c(3, 4, 30, 31, 32))
})

# ---------------------------------------------------------------------------
# prepare_stan_data() dispatcher
# ---------------------------------------------------------------------------

test_that("prepare_stan_data() includes shared flags and priors", {
  df <- data.frame(
    study_id            = "S1",
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
    sd_post_treatment   = 0.10
  )
  flags  <- list(
    is_baseline_normalised                  = 1L,
    is_correlation_coefficient_hierarchical = 1L,
    is_student_t_heterogeneity              = 0L,
    is_design_effect                        = 0L
  )
  priors <- set_priors()
  result <- prepare_stan_data(df, NULL, flags, priors)

  # Flags present
  expect_equal(result$is_baseline_normalised, 1L)
  expect_equal(result$is_student_t_heterogeneity, 0L)

  # Prior hyperparameters present
  expect_equal(result$treatment_effect_mean_prior_mean, 0)
  expect_equal(result$nu_prior_shape, 2)

  # DiD summary data present
  expect_equal(result$n_studies_did_summary, 1)

  # Other designs zeroed out
  expect_equal(result$n_studies_rct, 0L)
  expect_equal(result$n_studies_rct_summary, 0L)
  expect_equal(result$n_studies_pp, 0L)
  expect_equal(result$n_studies_pp_summary, 0L)
  expect_equal(result$n_studies_did, 0L)
  expect_equal(result$n_studies_did_change_only, 0L)
})

test_that("prepare_stan_data() handles mixed summary and individual", {
  summary_df <- data.frame(
    study_id            = "sum_rct",
    design              = "rct",
    n_control           = 60L,
    mean_post_control   = 0.48,
    sd_post_control     = 0.12,
    n_treatment         = 65L,
    mean_post_treatment = 0.35,
    sd_post_treatment   = 0.11
  )
  individual_df <- data.frame(
    study_id   = rep("ind_did", 4),
    subject_id = rep(1, 4),
    design     = "did",
    group      = rep(c("control", "treatment"), each = 2),
    time       = rep(c("pre", "post"), 2),
    value      = c(10, 12, 11, 8)
  )
  flags  <- list(
    is_baseline_normalised                  = 0L,
    is_correlation_coefficient_hierarchical = 0L,
    is_student_t_heterogeneity              = 0L,
    is_design_effect                        = 0L
  )
  result <- prepare_stan_data(summary_df, individual_df, flags, set_priors())

  expect_equal(result$n_studies_rct_summary, 1)
  expect_equal(result$n_studies_did, 1)
  expect_equal(result$n_studies_did_summary, 0L)
})
