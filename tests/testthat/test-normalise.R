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

test_that("normalise_summary() divides did_change by grand mean of DiD pre-control", {
  # DiD study with mean_pre_control = 100, plus a did_change study
  did_df <- data.frame(
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
  chg_df <- data.frame(
    study_id              = "C1",
    design                = "did_change",
    n_control             = 40L,
    n_treatment           = 45L,
    mean_change_control   = -10,
    sd_change_control     = 15,
    mean_change_treatment = -30,
    sd_change_treatment   = 18
  )
  df <- dplyr::bind_rows(did_df, chg_df)
  result <- normalise_summary(df, NULL)

  # Grand mean of DiD pre-control means = 100 (one DiD study)
  chg_row <- result$summary_data[result$summary_data$design == "did_change", ]
  expect_equal(chg_row$mean_change_control,   -10 / 100)
  expect_equal(chg_row$mean_change_treatment, -30 / 100)
  expect_equal(chg_row$sd_change_control,      15 / 100)
  expect_equal(chg_row$sd_change_treatment,    18 / 100)
  expect_equal(result$factors$did_change, 100)
})

test_that("normalise_summary() uses grand mean across multiple DiD studies for did_change", {
  did_df <- data.frame(
    study_id            = c("S1", "S2"),
    design              = "did",
    n_control           = c(50L, 60L),
    mean_pre_control    = c(80, 120),
    mean_post_control   = c(75, 110),
    sd_pre_control      = c(20, 25),
    sd_post_control     = c(18, 22),
    n_treatment         = c(55L, 65L),
    mean_pre_treatment  = c(82, 118),
    mean_post_treatment = c(60, 90),
    sd_pre_treatment    = c(22, 24),
    sd_post_treatment   = c(15, 20)
  )
  chg_df <- data.frame(
    study_id              = "C1",
    design                = "did_change",
    n_control             = 40L,
    n_treatment           = 45L,
    mean_change_control   = -5,
    sd_change_control     = 10,
    mean_change_treatment = -20,
    sd_change_treatment   = 12
  )
  df <- dplyr::bind_rows(did_df, chg_df)
  result <- normalise_summary(df, NULL)

  # Grand mean = mean(80, 120) = 100
  chg_row <- result$summary_data[result$summary_data$design == "did_change", ]
  expect_equal(chg_row$mean_change_control,   -5 / 100)
  expect_equal(chg_row$mean_change_treatment, -20 / 100)
  expect_equal(result$factors$did_change, 100)
})

test_that("normalise_summary() returns NULL factors for NULL data", {
  result <- normalise_summary(NULL, NULL)
  expect_equal(length(result$factors), 0)
})

test_that("normalise_summary() normalises individual DiD by pre-control mean", {
  individual_df <- data.frame(
    study_id   = rep("S1", 4),
    subject_id = rep(1, 4),
    design     = "did",
    group      = rep(c("control", "treatment"), each = 2),
    time       = rep(c("pre", "post"), 2),
    value      = c(100, 90, 110, 80)
  )
  result <- normalise_summary(NULL, individual_df)

  # Pre-control mean = 100; all values divided by 100
  expect_equal(result$individual_data$value, c(1.0, 0.9, 1.1, 0.8))
})

test_that("normalise_summary() normalises individual RCT by post-control mean", {
  individual_df <- data.frame(
    study_id = rep("R1", 4),
    design   = "rct",
    group    = rep(c("control", "treatment"), each = 2),
    time     = "post",
    value    = c(50, 60, 40, 35)
  )
  result <- normalise_summary(NULL, individual_df)

  # Post-control mean = (50 + 60) / 2 = 55; all values divided by 55
  expect_equal(result$individual_data$value, c(50, 60, 40, 35) / 55)
  expect_false(any(is.nan(result$individual_data$value)))
})

test_that("normalise_summary() normalises individual PP by pre-treatment mean", {
  individual_df <- data.frame(
    study_id   = rep("P1", 4),
    subject_id = rep(1:2, 2),
    design     = "pp",
    group      = "treatment",
    time       = rep(c("pre", "post"), each = 2),
    value      = c(200, 210, 180, 170)
  )
  result <- normalise_summary(NULL, individual_df)

  # Pre-treatment mean = (200 + 210) / 2 = 205; all values divided by 205
  expect_equal(result$individual_data$value, c(200, 210, 180, 170) / 205)
  expect_false(any(is.nan(result$individual_data$value)))
})
