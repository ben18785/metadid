# Tests for S3 methods in R/fit.R.
# All tests use mock fit objects — no Stan required.

# ---------------------------------------------------------------------------
# Mock helpers
# ---------------------------------------------------------------------------

# Fake draws: matrix with named columns, returned by $draws(var, format="matrix")
# params should be a named list; each element is either a scalar (replicated
# n_draws times), a vector of length n_draws, or a matrix (multiple Stan
# variables, columns named name[1], name[2], ...).
make_mock_draws <- function(params, n_draws = 50) {
  cols <- list()
  col_names <- character()
  for (nm in names(params)) {
    v <- params[[nm]]
    if (is.matrix(v)) {
      for (j in seq_len(ncol(v))) {
        cols[[length(cols) + 1]] <- v[, j]
        col_names <- c(col_names, paste0(nm, "[", j, "]"))
      }
    } else {
      cols[[length(cols) + 1]] <- if (length(v) == 1) rep(v, n_draws) else v
      col_names <- c(col_names, nm)
    }
  }
  mat <- do.call(cbind, cols)
  colnames(mat) <- col_names
  mat
}

# Minimal mock CmdStan fit with $draws() and $summary() for "sample" method
mock_cmdstan_sample <- function(params) {
  draws_mat <- make_mock_draws(params)
  list(
    draws = function(variables = NULL, format = "matrix") {
      if (format == "draws_matrix") return(draws_mat)
      if (is.null(variables)) return(draws_mat)
      # Support both exact matches and prefix matches (e.g. "foo" → "foo[1]")
      matched <- variables[variables %in% colnames(draws_mat)]
      prefix <- setdiff(variables, matched)
      for (p in prefix) {
        pattern <- paste0("^", p, "(\\[|$)")
        matched <- c(matched, grep(pattern, colnames(draws_mat), value = TRUE))
      }
      draws_mat[, matched, drop = FALSE]
    },
    summary = function(variables) {
      data.frame(
        variable = variables,
        rhat = rep(1.0, length(variables))
      )
    }
  )
}

# Minimal mock CmdStan fit with $mle() for "optimize" method
mock_cmdstan_optimize <- function(params) {
  mle_vec <- unlist(params)
  names(mle_vec) <- names(params)
  list(mle = function() mle_vec)
}

# Build a mock meta_did_fit object
mock_meta_did_fit <- function(
    method = "sample",
    summary_data = NULL,
    individual_data = NULL,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_correlation_coefficient_hierarchical = 0L,
      is_student_t_heterogeneity = 0L,
      is_design_effect = 0L
    ),
    params = list(
      treatment_effect_mean = -0.3,
      treatment_effect_sd = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32)
    ),
    covariate_names = NULL,
    multiplicative_covariate = NULL,
    center_covariates = FALSE
) {
  if (method == "sample") {
    fake_fit <- mock_cmdstan_sample(params)
  } else {
    fake_fit <- mock_cmdstan_optimize(params)
  }

  structure(
    list(
      fit = fake_fit,
      summary_data = summary_data,
      individual_data = individual_data,
      model_flags = model_flags,
      priors = NULL,
      normalisation_factors = NULL,
      method = method,
      covariate_names = covariate_names,
      multiplicative_covariate = multiplicative_covariate,
      center_covariates = center_covariates
    ),
    class = "meta_did_fit"
  )
}

# Minimal summary data for 2 DiD studies
did_summary <- data.frame(
  study_id = c("s1", "s2"),
  design = c("did", "did"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# print() — sample method
# ---------------------------------------------------------------------------

test_that("print (sample): shows study counts and CI", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary
  )
  out <- capture.output(print(fit))
  expect_true(any(grepl("DiD = 2", out)))
  expect_true(any(grepl("RCT = 0", out)))
  expect_true(any(grepl("90% CI", out)))
})

test_that("print (sample): shows Student-t df when robust_heterogeneity on", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_correlation_coefficient_hierarchical = 0L,
      is_student_t_heterogeneity = 1L,
      is_design_effect = 0L
    ),
    params = list(
      treatment_effect_mean = -0.3,
      treatment_effect_sd = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32),
      `nu_treatment_vec[1]` = 5.0
    )
  )
  out <- capture.output(print(fit))
  expect_true(any(grepl("Student-t", out)))
  expect_true(any(grepl("df", out)))
})

test_that("print (sample): shows design offsets when design_effects on", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_correlation_coefficient_hierarchical = 0L,
      is_student_t_heterogeneity = 0L,
      is_design_effect = 1L
    ),
    params = list(
      treatment_effect_mean = -0.3,
      treatment_effect_sd = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32),
      treatment_effect_mean_rct = -0.2,
      treatment_effect_mean_pp = -0.4
    )
  )
  out <- capture.output(print(fit))
  expect_true(any(grepl("Design offsets", out)))
  expect_true(any(grepl("RCT", out)))
  expect_true(any(grepl("Pre-Post", out)))
})

test_that("print returns the fit object invisibly", {
  fit <- mock_meta_did_fit(summary_data = did_summary)
  result <- print(fit)
  expect_identical(result, fit)
})

# ---------------------------------------------------------------------------
# print() — optimize method
# ---------------------------------------------------------------------------

test_that("print (optimize): shows MAP estimate without CI", {
  fit <- mock_meta_did_fit(
    method = "optimize",
    summary_data = did_summary,
    params = list(
      treatment_effect_mean = -0.3,
      treatment_effect_sd = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32)
    )
  )
  out <- capture.output(print(fit))
  expect_true(any(grepl("MAP", out)))
  expect_false(any(grepl("CI", out)))
})

test_that("print (optimize): shows design offsets when design_effects on", {
  fit <- mock_meta_did_fit(
    method = "optimize",
    summary_data = did_summary,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_correlation_coefficient_hierarchical = 0L,
      is_student_t_heterogeneity = 0L,
      is_design_effect = 1L
    ),
    params = list(
      treatment_effect_mean = -0.3,
      treatment_effect_sd = 0.05,
      treatment_effect_mean_rct = -0.2,
      treatment_effect_mean_pp = -0.4,
      treatment_effect_did_summary = c(-0.28, -0.32)
    )
  )
  out <- capture.output(print(fit))
  expect_true(any(grepl("Design offsets", out)))
})

# ---------------------------------------------------------------------------
# print() — correct study counts with mixed designs
# ---------------------------------------------------------------------------

test_that("print counts studies across designs", {
  sdata <- data.frame(
    study_id = c("d1", "d2", "r1", "p1", "c1"),
    design   = c("did", "did", "rct", "pp", "did_change"),
    stringsAsFactors = FALSE
  )
  fit <- mock_meta_did_fit(
    summary_data = sdata,
    params = list(
      treatment_effect_mean = -0.3,
      treatment_effect_sd = 0.05,
      treatment_effect_did_summary = c(-0.3, -0.28)
    )
  )
  out <- capture.output(print(fit))
  expect_true(any(grepl("DiD = 2", out)))
  expect_true(any(grepl("RCT = 1", out)))
  expect_true(any(grepl("Pre-Post = 1", out)))
  expect_true(any(grepl("DiD \\(change only\\) = 1", out)))
})

# ---------------------------------------------------------------------------
# summary() — sample method
# ---------------------------------------------------------------------------

test_that("summary (sample): returns correct structure", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary
  )
  s <- summary(fit)
  expect_s3_class(s, "data.frame")
  expect_true(all(c("parameter", "mean", "sd", "lo", "hi") %in% names(s)))
  expect_true("treatment_effect_mean" %in% s$parameter)
  expect_true("treatment_effect_sd" %in% s$parameter)
})

test_that("summary (sample): includes design effects when enabled", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    model_flags = list(
      is_baseline_normalised = 1L,
      is_correlation_coefficient_hierarchical = 0L,
      is_student_t_heterogeneity = 0L,
      is_design_effect = 1L
    ),
    params = list(
      treatment_effect_mean = -0.3,
      treatment_effect_sd = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32),
      treatment_effect_mean_rct = -0.2,
      treatment_effect_mean_pp = -0.4
    )
  )
  s <- summary(fit)
  expect_true("treatment_effect_mean_rct" %in% s$parameter)
  expect_true("treatment_effect_mean_pp" %in% s$parameter)
})

test_that("summary (sample): study-level params per design type", {
  sdata <- data.frame(
    study_id = c("d1", "r1"),
    design   = c("did", "rct"),
    stringsAsFactors = FALSE
  )
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = sdata,
    params = list(
      treatment_effect_mean = -0.3,
      treatment_effect_sd = 0.05,
      treatment_effect_did_summary = -0.3,
      treatment_effect_rct_summary_derived = -0.25
    )
  )
  s <- summary(fit)
  expect_true("treatment_effect_did_summary" %in% s$parameter)
  expect_true("treatment_effect_rct_summary_derived" %in% s$parameter)
})

test_that("summary (sample): prob argument affects intervals", {
  set.seed(4821)
  n <- 100
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    params = list(
      treatment_effect_mean = rnorm(n, -0.3, 0.05),
      treatment_effect_sd = rep(0.05, n),
      treatment_effect_did_summary = matrix(
        c(rnorm(n, -0.28, 0.02), rnorm(n, -0.32, 0.02)),
        nrow = n, ncol = 2
      )
    )
  )
  s50 <- summary(fit, prob = 0.5)
  s90 <- summary(fit, prob = 0.9)
  te50 <- s50[s50$parameter == "treatment_effect_mean", ]
  te90 <- s90[s90$parameter == "treatment_effect_mean", ]
  # 90% CI should be wider than 50% CI
  expect_true((te90$hi - te90$lo) > (te50$hi - te50$lo))
})

# ---------------------------------------------------------------------------
# summary() — optimize method
# ---------------------------------------------------------------------------

test_that("summary (optimize): returns NAs for sd/lo/hi", {
  fit <- mock_meta_did_fit(
    method = "optimize",
    summary_data = did_summary,
    params = list(
      treatment_effect_mean = -0.3,
      treatment_effect_sd = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32)
    )
  )
  s <- summary(fit)
  expect_true(all(is.na(s$sd)))
  expect_true(all(is.na(s$lo)))
  expect_true(all(is.na(s$hi)))
  expect_equal(s$mean[s$parameter == "treatment_effect_mean"], -0.3)
})

# ---------------------------------------------------------------------------
# print() / summary() — multiplicative covariate (no Stan)
# ---------------------------------------------------------------------------

test_that("print (sample): renders the multiplier ladder with reference and level names", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    params = list(
      treatment_effect_mean        = -0.3,
      treatment_effect_sd          = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32),
      effect_multiplier            = matrix(rep(c(0.6, 0.3), each = 50), ncol = 2)
    ),
    multiplicative_covariate = list(
      name   = "setting",
      levels = c("experimental", "rw_lab", "rw_field")
    )
  )
  out <- capture.output(print(fit))
  expect_true(any(grepl("Multiplicative covariate \\(setting\\)", out)))
  expect_true(any(grepl("experimental: 1  \\(reference\\)", out)))
  expect_true(any(grepl("rw_lab:", out)))
  expect_true(any(grepl("rw_field:", out)))
})

test_that("print (sample): labels a bare-string multiplicative_covariate as 'level k'", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    params = list(
      treatment_effect_mean        = -0.3,
      treatment_effect_sd          = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32),
      effect_multiplier            = matrix(rep(0.5, 50), ncol = 1)
    ),
    multiplicative_covariate = "real_world"
  )
  out <- capture.output(print(fit))
  expect_true(any(grepl("Multiplicative covariate \\(real_world\\)", out)))
  expect_true(any(grepl("level 1:", out)))
})

test_that("print (optimize): renders multiplier MAP estimates with reference", {
  fit <- mock_meta_did_fit(
    method = "optimize",
    summary_data = did_summary,
    params = list(
      "treatment_effect_mean" = -0.3,
      "effect_multiplier[1]"  = 0.6,
      "effect_multiplier[2]"  = 0.3
    ),
    multiplicative_covariate = list(
      name   = "setting",
      levels = c("experimental", "rw_lab", "rw_field")
    )
  )
  out <- capture.output(print(fit))
  expect_true(any(grepl("Multiplicative covariate \\(setting\\)", out)))
  expect_true(any(grepl("experimental: 1  \\(reference\\)", out)))
  expect_true(any(grepl("rw_lab:.*MAP estimate", out)))
})

test_that("summary (sample): labels effect_multiplier rows by level name", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    params = list(
      treatment_effect_mean        = -0.3,
      treatment_effect_sd          = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32),
      effect_multiplier            = matrix(rep(c(0.6, 0.3), each = 50), ncol = 2)
    ),
    multiplicative_covariate = list(
      name   = "setting",
      levels = c("experimental", "rw_lab", "rw_field")
    )
  )
  s <- summary(fit)
  expect_true(all(c("effect_multiplier[rw_lab]",
                    "effect_multiplier[rw_field]") %in% s$parameter))
})

test_that("summary (sample): falls back to the bare covariate name for a single-level string mc", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    params = list(
      treatment_effect_mean        = -0.3,
      treatment_effect_sd          = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32),
      effect_multiplier            = matrix(rep(0.5, 50), ncol = 1)
    ),
    multiplicative_covariate = "real_world"
  )
  s <- summary(fit)
  expect_true("effect_multiplier[real_world]" %in% s$parameter)
})

test_that("summary (sample): falls back to 'level' labels when level count mismatches draws", {
  fit <- mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    params = list(
      treatment_effect_mean        = -0.3,
      treatment_effect_sd          = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32),
      effect_multiplier            = matrix(rep(c(0.6, 0.3, 0.4), each = 50), ncol = 3)
    ),
    # only 2 levels declared but 3 multipliers in the draws -> generic labels
    multiplicative_covariate = list(name = "setting", levels = c("a", "b"))
  )
  s <- summary(fit)
  expect_true(all(c("effect_multiplier[level1]", "effect_multiplier[level2]",
                    "effect_multiplier[level3]") %in% s$parameter))
})

test_that("summary (optimize): emits effect_multiplier rows with NA uncertainty", {
  fit <- mock_meta_did_fit(
    method = "optimize",
    summary_data = did_summary,
    params = list(
      "treatment_effect_mean" = -0.3,
      "treatment_effect_sd"   = 0.05,
      "effect_multiplier[1]"  = 0.6,
      "effect_multiplier[2]"  = 0.3
    ),
    multiplicative_covariate = list(
      name   = "setting",
      levels = c("experimental", "rw_lab", "rw_field")
    )
  )
  s <- summary(fit)
  mult_rows <- s[grepl("^effect_multiplier", s$parameter), ]
  expect_equal(nrow(mult_rows), 2)
  expect_true(all(is.na(mult_rows$sd)))
  expect_true(all(c("effect_multiplier[rw_lab]",
                    "effect_multiplier[rw_field]") %in% mult_rows$parameter))
})

test_that("print()/summary() omit the multiplier section when multiplicative_covariate is NULL", {
  fit <- mock_meta_did_fit(method = "sample", summary_data = did_summary)
  out <- capture.output(print(fit))
  expect_false(any(grepl("Multiplicative covariate", out)))
  expect_false(any(grepl("effect_multiplier", summary(fit)$parameter)))
})

two_cov_fit <- function() {
  mock_meta_did_fit(
    method = "sample",
    summary_data = did_summary,
    params = list(
      treatment_effect_mean        = -0.3,
      treatment_effect_sd          = 0.05,
      treatment_effect_did_summary = c(-0.28, -0.32),
      effect_multiplier            = matrix(rep(0.6, 50), ncol = 1),
      effect_multiplier2           = matrix(rep(c(0.8, 0.5), each = 50), ncol = 2)
    ),
    multiplicative_covariate = list(
      list(name = "delivery",  levels = c("in_person", "remote")),
      list(name = "intensity", levels = c("low", "medium", "high"))
    )
  )
}

test_that("print (sample): renders both covariates of a two-covariate fit", {
  out <- capture.output(print(two_cov_fit()))
  expect_true(any(grepl("Multiplicative covariate \\(delivery\\)", out)))
  expect_true(any(grepl("Multiplicative covariate \\(intensity\\)", out)))
  expect_true(any(grepl("in_person: 1  \\(reference\\)", out)))
  expect_true(any(grepl("low: 1  \\(reference\\)", out)))
  expect_true(any(grepl("remote:", out)))
  expect_true(any(grepl("medium:", out)))
  expect_true(any(grepl("high:", out)))
})

test_that("summary (sample): prefixes two-covariate multiplier rows with the covariate name", {
  s <- summary(two_cov_fit())
  expect_true("effect_multiplier[delivery:remote]" %in% s$parameter)
  expect_true(all(c("effect_multiplier[intensity:medium]",
                    "effect_multiplier[intensity:high]") %in% s$parameter))
})

# ---------------------------------------------------------------------------
# tidy()
# ---------------------------------------------------------------------------

test_that("tidy returns NULL with a message", {
  fit <- mock_meta_did_fit(summary_data = did_summary)
  expect_message(result <- tidy.meta_did_fit(fit), "not yet implemented")
  expect_null(result)
})
