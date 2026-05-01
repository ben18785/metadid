# Tests for R/simulate.R — simulation engine and extractors.
# All tests are pure R, no Stan required.

# ---------------------------------------------------------------------------
# Shared fixture
# ---------------------------------------------------------------------------

N_STUDIES <- 5L
N_CTRL    <- 30L
N_TRT     <- 40L

sim <- simulate_meta_did(
  n_studies     = N_STUDIES,
  n_control     = N_CTRL,
  n_treatment   = N_TRT,
  true_effect   = -0.15,
  sigma_effect  = 0.03,
  true_trend    = -0.02,
  sigma_trend   = 0.01,
  baseline_mean = 0.45,
  baseline_sd   = 0.05,
  within_sd     = 0.12,
  rho           = 0.5,
  seed          = 7294
)

# ---------------------------------------------------------------------------
# simulate_meta_did()
# ---------------------------------------------------------------------------

test_that("simulate_meta_did returns correct structure and dimensions", {
  expect_s3_class(sim, "data.frame")
  expect_true(all(c("study_id", "subject_id", "group", "time", "value") %in% names(sim)))

  expected_rows <- N_STUDIES * (N_CTRL + N_TRT) * 2L
  expect_equal(nrow(sim), expected_rows)

  expect_setequal(unique(sim$group), c("control", "treatment"))
  expect_setequal(unique(sim$time), c("pre", "post"))
  expect_equal(length(unique(sim$study_id)), N_STUDIES)
})

test_that("simulate_meta_did attaches true_params attribute", {
  params <- attr(sim, "true_params")
  expect_s3_class(params, "data.frame")
  expect_true(all(c("study_id", "theta", "gamma", "baseline") %in% names(params)))
  expect_equal(nrow(params), N_STUDIES)
})

test_that("simulate_meta_did is reproducible with seed", {
  a <- simulate_meta_did(n_studies = 3, seed = 1234)
  b <- simulate_meta_did(n_studies = 3, seed = 1234)
  expect_identical(a, b)
})

test_that("simulate_meta_did sample sizes match per study", {
  per_study <- split(sim, sim$study_id)
  for (s in per_study) {
    n_ctrl_rows <- sum(s$group == "control")
    n_trt_rows  <- sum(s$group == "treatment")
    # Each subject has pre + post = 2 rows
    expect_equal(n_ctrl_rows, N_CTRL * 2L)
    expect_equal(n_trt_rows, N_TRT * 2L)
  }
})

# ---------------------------------------------------------------------------
# Individual extractors
# ---------------------------------------------------------------------------

test_that("as_individual_did preserves all rows and adds design", {
  ind <- as_individual_did(sim)
  expect_equal(nrow(ind), nrow(sim))
  expect_true(all(ind$design == "did"))
  expect_true("subject_id" %in% names(ind))
  expect_true(all(c("study_id", "design", "group", "time", "value") %in% names(ind)))
})

test_that("as_individual_rct keeps only post rows", {
  ind <- as_individual_rct(sim)
  expect_true(all(ind$time == "post"))
  expect_true(all(ind$design == "rct"))
  expect_setequal(unique(ind$group), c("control", "treatment"))
  # No subject_id for RCT (cross-sectional)
  expect_false("subject_id" %in% names(ind))
  expected_rows <- N_STUDIES * (N_CTRL + N_TRT)
  expect_equal(nrow(ind), expected_rows)
})

test_that("as_individual_pp keeps only treatment rows", {
  ind <- as_individual_pp(sim)
  expect_true(all(ind$group == "treatment"))
  expect_true(all(ind$design == "pp"))
  expect_setequal(unique(ind$time), c("pre", "post"))
  expect_true("subject_id" %in% names(ind))
  expected_rows <- N_STUDIES * N_TRT * 2L
  expect_equal(nrow(ind), expected_rows)
})

# ---------------------------------------------------------------------------
# Summary extractors
# ---------------------------------------------------------------------------

test_that("as_summary_did returns one row per study with correct columns", {
  s <- as_summary_did(sim)
  expect_equal(nrow(s), N_STUDIES)
  expect_true(all(s$design == "did"))
  required_cols <- c(
    "study_id", "design", "n_control", "n_treatment",
    "mean_pre_control", "mean_post_control", "sd_pre_control", "sd_post_control",
    "mean_pre_treatment", "mean_post_treatment", "sd_pre_treatment", "sd_post_treatment",
    "rho"
  )
  expect_true(all(required_cols %in% names(s)))
  expect_true(all(s$n_control == N_CTRL))
  expect_true(all(s$n_treatment == N_TRT))
  expect_true(all(s$rho > -1 & s$rho < 1))
})

test_that("as_summary_rct returns correct post-only columns", {
  s <- as_summary_rct(sim)
  expect_equal(nrow(s), N_STUDIES)
  expect_true(all(s$design == "rct"))
  required_cols <- c(
    "study_id", "design", "n_control", "n_treatment",
    "mean_post_control", "sd_post_control",
    "mean_post_treatment", "sd_post_treatment"
  )
  expect_true(all(required_cols %in% names(s)))
  # Should NOT have pre columns
  expect_false(any(grepl("pre", names(s))))
})

test_that("as_summary_pp returns correct treatment-arm columns", {
  s <- as_summary_pp(sim)
  expect_equal(nrow(s), N_STUDIES)
  expect_true(all(s$design == "pp"))
  required_cols <- c(
    "study_id", "design", "n_treatment",
    "mean_pre_treatment", "sd_pre_treatment",
    "mean_post_treatment", "sd_post_treatment",
    "rho"
  )
  expect_true(all(required_cols %in% names(s)))
  # Should NOT have control columns
  expect_false(any(grepl("control", names(s))))
  expect_true(all(s$rho > -1 & s$rho < 1))
})

test_that("as_summary_did_change returns correct change-score columns", {
  s <- as_summary_did_change(sim)
  expect_equal(nrow(s), N_STUDIES)
  expect_true(all(s$design == "did_change"))
  required_cols <- c(
    "study_id", "design", "n_control", "n_treatment",
    "mean_change_control", "sd_change_control",
    "mean_change_treatment", "sd_change_treatment"
  )
  expect_true(all(required_cols %in% names(s)))
  expect_true(all(s$sd_change_control > 0))
  expect_true(all(s$sd_change_treatment > 0))
})

# ---------------------------------------------------------------------------
# Cross-checks between extractors
# ---------------------------------------------------------------------------

test_that("summary means are consistent with individual-level data", {
  s <- as_summary_did(sim)
  # Check first study manually
  s1 <- sim[sim$study_id == s$study_id[1], ]

  ctrl_pre  <- s1$value[s1$group == "control" & s1$time == "pre"]
  ctrl_post <- s1$value[s1$group == "control" & s1$time == "post"]
  trt_pre   <- s1$value[s1$group == "treatment" & s1$time == "pre"]
  trt_post  <- s1$value[s1$group == "treatment" & s1$time == "post"]

  expect_equal(s$mean_pre_control[1],    mean(ctrl_pre))
  expect_equal(s$mean_post_control[1],   mean(ctrl_post))
  expect_equal(s$mean_pre_treatment[1],  mean(trt_pre))
  expect_equal(s$mean_post_treatment[1], mean(trt_post))
  expect_equal(s$sd_pre_control[1],      sd(ctrl_pre))
})

test_that("change-score means equal post - pre computed from individual data", {
  chg <- as_summary_did_change(sim)
  did <- as_summary_did(sim)

  # Mean change should be close to post - pre from summary
  # (Not exact because change score is computed per individual, but should match)
  expect_equal(
    chg$mean_change_control,
    did$mean_post_control - did$mean_pre_control,
    tolerance = 1e-10
  )
  expect_equal(
    chg$mean_change_treatment,
    did$mean_post_treatment - did$mean_pre_treatment,
    tolerance = 1e-10
  )
})

# ---------------------------------------------------------------------------
# .within_arm_rho (internal helper)
# ---------------------------------------------------------------------------

test_that(".within_arm_rho returns valid correlation", {
  arm <- data.frame(
    subject_id = rep(1:20, each = 2),
    time  = rep(c("pre", "post"), 20),
    value = c(rbind(rnorm(20), rnorm(20)))
  )
  rho <- metadid:::.within_arm_rho(arm)
  expect_true(is.numeric(rho))
  expect_true(rho >= -1 && rho <= 1)
})

test_that(".within_arm_rho returns NA when subject_id is missing", {
  arm <- data.frame(
    time  = c("pre", "post", "pre", "post"),
    value = c(1, 2, 3, 4)
  )
  expect_true(is.na(metadid:::.within_arm_rho(arm)))
})

test_that(".within_arm_rho returns NA with fewer than 3 subjects", {
  arm <- data.frame(
    subject_id = rep(1:2, each = 2),
    time  = rep(c("pre", "post"), 2),
    value = c(1, 2, 3, 4)
  )
  expect_true(is.na(metadid:::.within_arm_rho(arm)))
})
