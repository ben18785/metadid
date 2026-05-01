# Tests for R/validate.R

# ---------------------------------------------------------------------------
# Helpers: minimal valid data frames
# ---------------------------------------------------------------------------

make_did_summary <- function(n = 1, ...) {
  data.frame(
    study_id            = paste0("did_", seq_len(n)),
    design              = "did",
    n_control           = rep(50L, n),
    mean_pre_control    = rep(0.45, n),
    mean_post_control   = rep(0.42, n),
    sd_pre_control      = rep(0.12, n),
    sd_post_control     = rep(0.11, n),
    n_treatment         = rep(55L, n),
    mean_pre_treatment  = rep(0.46, n),
    mean_post_treatment = rep(0.30, n),
    sd_pre_treatment    = rep(0.13, n),
    sd_post_treatment   = rep(0.10, n),
    ...
  )
}

make_rct_summary <- function(n = 1) {
  data.frame(
    study_id            = paste0("rct_", seq_len(n)),
    design              = "rct",
    n_control           = rep(60L, n),
    mean_post_control   = rep(0.48, n),
    sd_post_control     = rep(0.12, n),
    n_treatment         = rep(65L, n),
    mean_post_treatment = rep(0.35, n),
    sd_post_treatment   = rep(0.11, n)
  )
}

make_pp_summary <- function(n = 1) {
  data.frame(
    study_id            = paste0("pp_", seq_len(n)),
    design              = "pp",
    n_treatment         = rep(70L, n),
    mean_pre_treatment  = rep(0.48, n),
    mean_post_treatment = rep(0.32, n),
    sd_pre_treatment    = rep(0.12, n),
    sd_post_treatment   = rep(0.09, n)
  )
}

make_did_change_summary <- function(n = 1) {
  data.frame(
    study_id              = paste0("dc_", seq_len(n)),
    design                = "did_change",
    n_control             = rep(40L, n),
    mean_change_control   = rep(-0.02, n),
    sd_change_control     = rep(0.08, n),
    n_treatment           = rep(45L, n),
    mean_change_treatment = rep(-0.10, n),
    sd_change_treatment   = rep(0.09, n)
  )
}

# ---------------------------------------------------------------------------
# validate_summary_data()
# ---------------------------------------------------------------------------

test_that("validate_summary_data() accepts NULL", {
  expect_invisible(validate_summary_data(NULL))
})

test_that("validate_summary_data() passes valid DiD data", {
  expect_invisible(validate_summary_data(make_did_summary()))
})

test_that("validate_summary_data() passes valid RCT data", {
  expect_invisible(validate_summary_data(make_rct_summary()))
})

test_that("validate_summary_data() passes valid PP data", {
  expect_invisible(validate_summary_data(make_pp_summary()))
})

test_that("validate_summary_data() passes valid did_change data", {
  expect_invisible(validate_summary_data(make_did_change_summary()))
})

test_that("validate_summary_data() passes mixed designs", {
  mixed <- dplyr::bind_rows(
    make_did_summary(),
    make_rct_summary(),
    make_pp_summary()
  )
  expect_invisible(validate_summary_data(mixed))
})

test_that("validate_summary_data() rejects missing study_id", {
  df <- make_did_summary()
  df$study_id <- NULL
  expect_error(validate_summary_data(df), "missing required columns.*study_id")
})

test_that("validate_summary_data() rejects missing design", {
  df <- make_did_summary()
  df$design <- NULL
  expect_error(validate_summary_data(df), "missing required columns.*design")
})

test_that("validate_summary_data() rejects unrecognised design", {
  df <- make_did_summary()
  df$design <- "crossover"
  expect_error(validate_summary_data(df), "unrecognised values.*crossover")
})

test_that("validate_summary_data() rejects duplicate study_ids", {
  df <- make_did_summary(2)
  df$study_id <- c("A", "A")
  expect_error(validate_summary_data(df), "Duplicate study_id")
})

test_that("validate_summary_data() rejects missing required columns", {
  df <- make_did_summary()
  df$sd_pre_control <- NULL
  expect_error(validate_summary_data(df), "missing required columns.*sd_pre_control")
})

test_that("validate_summary_data() rejects non-positive SDs", {
  df <- make_did_summary()
  df$sd_pre_control <- 0
  expect_error(validate_summary_data(df), "sd_pre_control.*strictly positive")
})

test_that("validate_summary_data() rejects non-positive sample sizes", {
  df <- make_did_summary()
  df$n_control <- -1
  expect_error(validate_summary_data(df), "n_control.*strictly positive")
})

test_that("validate_summary_data() checks columns per design type", {
  # RCT data missing RCT-required column
  df <- make_rct_summary()
  df$sd_post_treatment <- NULL
  expect_error(validate_summary_data(df), "missing required.*sd_post_treatment")
})

# ---------------------------------------------------------------------------
# validate_individual_data()
# ---------------------------------------------------------------------------

make_individual_did <- function(n_per_group = 3) {
  data.frame(
    study_id   = "study1",
    subject_id = rep(rep(seq_len(n_per_group), each = 2), 2),
    design     = "did",
    group      = rep(c("control", "treatment"), each = n_per_group * 2),
    time       = rep(c("pre", "post"), times = n_per_group * 2),
    value      = rnorm(n_per_group * 4, mean = 100, sd = 10)
  )
}

make_individual_rct <- function(n_per_group = 3) {
  data.frame(
    study_id = "study_rct",
    design   = "rct",
    group    = rep(c("control", "treatment"), each = n_per_group),
    time     = "post",
    value    = rnorm(n_per_group * 2, mean = 50, sd = 5)
  )
}

make_individual_pp <- function(n = 3) {
  data.frame(
    study_id   = "study_pp",
    subject_id = rep(seq_len(n), 2),
    design     = "pp",
    group      = "treatment",
    time       = rep(c("pre", "post"), each = n),
    value      = rnorm(n * 2, mean = 50, sd = 5)
  )
}

test_that("validate_individual_data() accepts NULL", {
  expect_invisible(validate_individual_data(NULL))
})

test_that("validate_individual_data() passes valid DiD data", {
  expect_invisible(validate_individual_data(make_individual_did()))
})

test_that("validate_individual_data() passes valid RCT data", {
  expect_invisible(validate_individual_data(make_individual_rct()))
})

test_that("validate_individual_data() passes valid PP data", {
  expect_invisible(validate_individual_data(make_individual_pp()))
})

test_that("validate_individual_data() rejects missing columns", {
  df <- make_individual_did()
  df$value <- NULL
  expect_error(validate_individual_data(df), "missing required columns.*value")
})

test_that("validate_individual_data() rejects unrecognised design", {
  df <- make_individual_did()
  df$design <- "bad"
  expect_error(validate_individual_data(df), "unrecognised values.*bad")
})

test_that("validate_individual_data() rejects bad group values", {
  df <- make_individual_did()
  df$group[1] <- "placebo"
  expect_error(validate_individual_data(df), "must be 'control' or 'treatment'")
})

test_that("validate_individual_data() rejects bad time values", {
  df <- make_individual_did()
  df$time[1] <- "during"
  expect_error(validate_individual_data(df), "must be 'pre' or 'post'")
})

test_that("validate_individual_data() rejects pp with control group", {
  df <- make_individual_pp()
  df$group[1] <- "control"
  expect_error(validate_individual_data(df), "design 'pp' but contains control")
})

test_that("validate_individual_data() rejects rct with pre-treatment rows", {
  df <- make_individual_rct()
  df$time[1] <- "pre"
  expect_error(validate_individual_data(df), "design 'rct' but contains pre")
})

test_that("validate_individual_data() rejects mixed designs per study", {
  df <- make_individual_did()
  df$design[1] <- "rct"
  expect_error(validate_individual_data(df), "multiple design values")
})

test_that("validate_individual_data() rejects unbalanced pre/post", {
  df <- make_individual_did()
  # Drop one post observation
  post_idx <- which(df$group == "control" & df$time == "post")
  df <- df[-post_idx[1], ]
  expect_error(validate_individual_data(df), "unequal number.*pre.*post")
})

test_that("validate_individual_data() rejects DiD missing a group", {
  df <- make_individual_did()
  df <- df[df$group == "control", ]
  expect_error(validate_individual_data(df), "must have both.*control.*treatment")
})

test_that("validate_individual_data() rejects DiD missing a time point", {
  df <- make_individual_did()
  df <- df[df$time == "pre", ]
  expect_error(validate_individual_data(df), "must have both.*pre.*post")
})

test_that("validate_individual_data() rejects RCT with only one arm", {
  df <- make_individual_rct()
  df <- df[df$group == "treatment", ]
  expect_error(validate_individual_data(df), "must have both.*control.*treatment")
})

test_that("validate_individual_data() rejects PP missing a time point", {
  df <- make_individual_pp()
  df <- df[df$time == "pre", ]
  expect_error(validate_individual_data(df), "must have both.*pre.*post")
})

test_that("validate_individual_data() rejects DiD without subject_id", {
  df <- make_individual_did()
  df$subject_id <- NULL
  expect_error(validate_individual_data(df), "subject_id")
})

test_that("validate_individual_data() rejects PP without subject_id", {
  df <- make_individual_pp()
  df$subject_id <- NULL
  expect_error(validate_individual_data(df), "subject_id")
})

test_that("validate_individual_data() accepts RCT without subject_id", {
  df <- make_individual_rct()
  expect_invisible(validate_individual_data(df))
})

test_that("validate_individual_data() rejects duplicate subject_id within group/time", {
  df <- make_individual_did()
  # Make two subjects share the same subject_id in the same group/time
  df$subject_id[df$group == "control" & df$time == "pre"][2] <-
    df$subject_id[df$group == "control" & df$time == "pre"][1]
  expect_error(validate_individual_data(df), "duplicate subject_id")
})
