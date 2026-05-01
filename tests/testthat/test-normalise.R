# Tests for normalise_summary() in R/meta_did.R

test_that("normalise_summary() divides DiD by mean_pre_control", {
  df <- data.frame(
    study_id            = "S1",
    design              = "did",
    n_control           = 50L,
    mean_pre_control    = 100,
    mean_post_control   = 90,
    sd_pre_control      = 20,
    sd_post_control     = 18,
    n_treatment         = 55L,
    mean_pre_treatment  = 105,
    mean_post_treatment = 80,
    sd_pre_treatment    = 22,
    sd_post_treatment   = 15
  )
  result <- normalise_summary(df, NULL)

  expect_equal(result$summary_data$mean_pre_control, 1.0)
  expect_equal(result$summary_data$mean_post_control, 0.9)
  expect_equal(result$summary_data$sd_pre_control, 0.2)
  expect_equal(result$summary_data$mean_post_treatment, 0.8)
  expect_equal(result$factors$did, c(S1 = 100))
})

test_that("normalise_summary() divides RCT by mean_post_control", {
  df <- data.frame(
    study_id            = "R1",
    design              = "rct",
    n_control           = 60L,
    mean_post_control   = 50,
    sd_post_control     = 10,
    n_treatment         = 65L,
    mean_post_treatment = 40,
    sd_post_treatment   = 8
  )
  result <- normalise_summary(df, NULL)

  expect_equal(result$summary_data$mean_post_control, 1.0)
  expect_equal(result$summary_data$mean_post_treatment, 0.8)
  expect_equal(result$summary_data$sd_post_control, 0.2)
  expect_equal(result$factors$rct, c(R1 = 50))
})

test_that("normalise_summary() divides PP by mean_pre_treatment", {
  df <- data.frame(
    study_id            = "P1",
    design              = "pp",
    n_treatment         = 70L,
    mean_pre_treatment  = 200,
    mean_post_treatment = 180,
    sd_pre_treatment    = 40,
    sd_post_treatment   = 30
  )
  result <- normalise_summary(df, NULL)

  expect_equal(result$summary_data$mean_pre_treatment, 1.0)
  expect_equal(result$summary_data$mean_post_treatment, 0.9)
  expect_equal(result$factors$pp, c(P1 = 200))
})

test_that("normalise_summary() returns NULL factors for NULL data", {
  result <- normalise_summary(NULL, NULL)
  expect_equal(length(result$factors), 0)
})

test_that("normalise_summary() normalises individual data by pre control mean", {
  individual_df <- data.frame(
    study_id = rep("S1", 4),
    design   = "did",
    group    = rep(c("control", "treatment"), each = 2),
    time     = rep(c("pre", "post"), 2),
    value    = c(100, 90, 110, 80)
  )
  result <- normalise_summary(NULL, individual_df)

  # Pre-control mean = 100; all values divided by 100
  expect_equal(result$individual_data$value, c(1.0, 0.9, 1.1, 0.8))
})
