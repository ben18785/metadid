# Tests for study-level covariate meta-regression.
# Covers validate_covariates(), covariate data preparation, simulation with
# covariates, and print/summary output. Pure R tests (no Stan required)
# unless noted.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_did_summary_with_cov <- function(n = 3) {
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
    dose                = c(1.0, 2.0, 3.0)[seq_len(n)],
    year                = c(2018, 2019, 2020)[seq_len(n)]
  )
}

make_individual_did_with_cov <- function(n_per_group = 10, n_studies = 2) {
  rows <- list()
  for (i in seq_len(n_studies)) {
    for (grp in c("control", "treatment")) {
      for (subj in seq_len(n_per_group)) {
        for (tm in c("pre", "post")) {
          rows[[length(rows) + 1]] <- data.frame(
            study_id   = paste0("ind_did_", i),
            subject_id = subj,
            design     = "did",
            group      = grp,
            time       = tm,
            value      = rnorm(1, 0.45, 0.12),
            dose       = i * 1.5,
            year       = 2018 + i
          )
        }
      }
    }
  }
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# validate_covariates()
# ---------------------------------------------------------------------------

test_that("validate_covariates() passes with valid covariates in summary_data", {
  df <- make_did_summary_with_cov()
  expect_invisible(validate_covariates(c("dose", "year"), df, NULL))
})

test_that("validate_covariates() passes with NULL covariate_names", {
  expect_invisible(validate_covariates(NULL, make_did_summary_with_cov(), NULL))
})

test_that("validate_covariates() passes with empty covariate_names", {
  expect_invisible(validate_covariates(character(0), make_did_summary_with_cov(), NULL))
})

test_that("validate_covariates() errors on missing column in summary_data", {
  df <- make_did_summary_with_cov()
  expect_error(
    validate_covariates(c("dose", "missing_col"), df, NULL),
    "not found in summary_data"
  )
})

test_that("validate_covariates() errors on non-numeric covariate in summary_data", {
  df <- make_did_summary_with_cov()
  df$dose <- as.character(df$dose)
  expect_error(
    validate_covariates("dose", df, NULL),
    "must be numeric"
  )
})

test_that("validate_covariates() errors on NA in summary_data covariate", {
  df <- make_did_summary_with_cov()
  df$dose[2] <- NA
  expect_error(
    validate_covariates("dose", df, NULL),
    "contains NA"
  )
})

test_that("validate_covariates() errors on missing column in individual_data", {
  ind <- make_individual_did_with_cov()
  expect_error(
    validate_covariates(c("dose", "nonexistent"), NULL, ind),
    "not found in individual_data"
  )
})

test_that("validate_covariates() errors on non-numeric covariate in individual_data", {
  ind <- make_individual_did_with_cov()
  ind$dose <- as.character(ind$dose)
  expect_error(
    validate_covariates("dose", NULL, ind),
    "must be numeric"
  )
})

test_that("validate_covariates() errors on NA in individual_data covariate", {
  ind <- make_individual_did_with_cov()
  ind$dose[5] <- NA
  expect_error(
    validate_covariates("dose", NULL, ind),
    "contains NA"
  )
})

test_that("validate_covariates() errors when covariate varies within study", {
  ind <- make_individual_did_with_cov()
  # Make dose vary within a study
  ind$dose[ind$study_id == "ind_did_1"] <- seq_len(sum(ind$study_id == "ind_did_1"))
  expect_error(
    validate_covariates("dose", NULL, ind),
    "varies within study"
  )
})

test_that("validate_covariates() passes when covariate is constant within study", {
  ind <- make_individual_did_with_cov()
  expect_invisible(validate_covariates(c("dose", "year"), NULL, ind))
})

# ---------------------------------------------------------------------------
# simulate_meta_did() with covariates
# ---------------------------------------------------------------------------

test_that("simulate_meta_did() with covariates adds covariate columns", {
  cov_df <- data.frame(dose = c(1.0, 2.0, 3.0))
  sim <- simulate_meta_did(
    n_studies = 3, seed = 6138,
    covariates = cov_df, beta_cov = c(0.05)
  )
  expect_true("dose" %in% names(sim))
  # Dose should be constant within study
  by_study <- tapply(sim$dose, sim$study_id, function(x) length(unique(x)))
  expect_true(all(by_study == 1))
})

test_that("simulate_meta_did() errors when covariates has wrong nrow", {
  cov_df <- data.frame(dose = c(1.0, 2.0))
  expect_error(
    simulate_meta_did(n_studies = 3, covariates = cov_df, beta_cov = c(0.05)),
    "must have 3 rows"
  )
})

test_that("simulate_meta_did() errors when beta_cov is missing", {
  cov_df <- data.frame(dose = c(1.0, 2.0, 3.0))
  expect_error(
    simulate_meta_did(n_studies = 3, covariates = cov_df),
    "must be provided"
  )
})

test_that("simulate_meta_did() errors when beta_cov has wrong length", {
  cov_df <- data.frame(dose = c(1.0, 2.0, 3.0))
  expect_error(
    simulate_meta_did(n_studies = 3, covariates = cov_df, beta_cov = c(0.05, 0.1)),
    "must have length 1"
  )
})

test_that("simulate_meta_did() without covariates still works (backward compat)", {
  sim <- simulate_meta_did(n_studies = 3, seed = 3891)
  expect_false("dose" %in% names(sim))
  expect_equal(length(unique(sim$study_id)), 3)
})

# ---------------------------------------------------------------------------
# Extractors propagate covariate columns
# ---------------------------------------------------------------------------

test_that("as_individual_did() propagates covariates", {
  cov_df <- data.frame(dose = c(1.0, 2.0))
  sim <- simulate_meta_did(n_studies = 2, seed = 7712, covariates = cov_df, beta_cov = c(0.1))
  ind <- as_individual_did(sim)
  expect_true("dose" %in% names(ind))
  expect_true("design" %in% names(ind))
})

test_that("as_individual_rct() propagates covariates", {
  cov_df <- data.frame(dose = c(1.0, 2.0))
  sim <- simulate_meta_did(n_studies = 2, seed = 7712, covariates = cov_df, beta_cov = c(0.1))
  ind <- as_individual_rct(sim)
  expect_true("dose" %in% names(ind))
})

test_that("as_individual_pp() propagates covariates", {
  cov_df <- data.frame(dose = c(1.0, 2.0))
  sim <- simulate_meta_did(n_studies = 2, seed = 7712, covariates = cov_df, beta_cov = c(0.1))
  ind <- as_individual_pp(sim)
  expect_true("dose" %in% names(ind))
})

test_that("as_summary_did() propagates covariates", {
  cov_df <- data.frame(dose = c(1.0, 2.0))
  sim <- simulate_meta_did(n_studies = 2, seed = 7712, covariates = cov_df, beta_cov = c(0.1))
  summ <- as_summary_did(sim)
  expect_true("dose" %in% names(summ))
  expect_equal(nrow(summ), 2)
})

test_that("as_summary_rct() propagates covariates", {
  cov_df <- data.frame(dose = c(1.0, 2.0))
  sim <- simulate_meta_did(n_studies = 2, seed = 7712, covariates = cov_df, beta_cov = c(0.1))
  summ <- as_summary_rct(sim)
  expect_true("dose" %in% names(summ))
})

test_that("as_summary_pp() propagates covariates", {
  cov_df <- data.frame(dose = c(1.0, 2.0))
  sim <- simulate_meta_did(n_studies = 2, seed = 7712, covariates = cov_df, beta_cov = c(0.1))
  summ <- as_summary_pp(sim)
  expect_true("dose" %in% names(summ))
})

test_that("as_summary_did_change() propagates covariates", {
  cov_df <- data.frame(dose = c(1.0, 2.0))
  sim <- simulate_meta_did(n_studies = 2, seed = 7712, covariates = cov_df, beta_cov = c(0.1))
  summ <- as_summary_did_change(sim)
  expect_true("dose" %in% names(summ))
})

# ---------------------------------------------------------------------------
# prepare_stan_data() with covariates
# ---------------------------------------------------------------------------

test_that("prepare_stan_data() includes K_cov = 0 when no covariates", {
  df <- make_did_summary_with_cov(2)
  model_flags <- list(
    baseline_latent_mode                     = 3L,
    is_correlation_coefficient_hierarchical = 0L,
    is_student_t_heterogeneity = 0L,
    is_design_effect = 0L
  )
  sd <- metadid:::prepare_stan_data(df, NULL, model_flags, set_priors())
  expect_equal(sd$K_cov, 0L)
  # X_cov matrices should have 0 columns
  expect_equal(ncol(sd$X_cov_did_summary), 0)
})

test_that("prepare_stan_data() includes correct K_cov and matrices with covariates", {
  df <- make_did_summary_with_cov(3)
  model_flags <- list(
    baseline_latent_mode                     = 3L,
    is_correlation_coefficient_hierarchical = 0L,
    is_student_t_heterogeneity = 0L,
    is_design_effect = 0L
  )
  sd <- metadid:::prepare_stan_data(df, NULL, model_flags, set_priors(),
                                     covariate_names = c("dose", "year"))
  expect_equal(sd$K_cov, 2L)
  expect_equal(nrow(sd$X_cov_did_summary), 3)
  expect_equal(ncol(sd$X_cov_did_summary), 2)
})

test_that("prepare_stan_data() centers covariates by default", {
  df <- make_did_summary_with_cov(3)
  model_flags <- list(
    baseline_latent_mode                     = 3L,
    is_correlation_coefficient_hierarchical = 0L,
    is_student_t_heterogeneity = 0L,
    is_design_effect = 0L
  )
  sd <- metadid:::prepare_stan_data(df, NULL, model_flags, set_priors(),
                                     covariate_names = c("dose"),
                                     center_covariates = TRUE)
  # Centered dose values should sum to approximately 0
  expect_equal(sum(sd$X_cov_did_summary[, 1]), 0, tolerance = 1e-10)
  # Centering values should be stored
  centers <- attr(sd, "cov_centers")
  expect_equal(centers[["dose"]], mean(c(1.0, 2.0, 3.0)))
})

test_that("prepare_stan_data() skips centering when center_covariates = FALSE", {
  df <- make_did_summary_with_cov(3)
  model_flags <- list(
    baseline_latent_mode                     = 3L,
    is_correlation_coefficient_hierarchical = 0L,
    is_student_t_heterogeneity = 0L,
    is_design_effect = 0L
  )
  sd <- metadid:::prepare_stan_data(df, NULL, model_flags, set_priors(),
                                     covariate_names = c("dose"),
                                     center_covariates = FALSE)
  # Raw dose values: 1, 2, 3
  expect_equal(as.numeric(sd$X_cov_did_summary[, 1]), c(1.0, 2.0, 3.0))
  expect_null(attr(sd, "cov_centers"))
})

test_that("prepare_stan_data() produces empty matrices for designs with 0 studies", {
  df <- make_did_summary_with_cov(2)
  model_flags <- list(
    baseline_latent_mode                     = 3L,
    is_correlation_coefficient_hierarchical = 0L,
    is_student_t_heterogeneity = 0L,
    is_design_effect = 0L
  )
  sd <- metadid:::prepare_stan_data(df, NULL, model_flags, set_priors(),
                                     covariate_names = c("dose"))
  # RCT, PP, etc. should have 0-row matrices with 1 column
  expect_equal(nrow(sd$X_cov_rct_summary), 0)
  expect_equal(ncol(sd$X_cov_rct_summary), 1)
  expect_equal(nrow(sd$X_cov_pp_summary), 0)
  expect_equal(ncol(sd$X_cov_pp_summary), 1)
  expect_equal(nrow(sd$X_cov_did_change_only), 0)
  expect_equal(ncol(sd$X_cov_did_change_only), 1)
})

# ---------------------------------------------------------------------------
# priors: beta_cov
# ---------------------------------------------------------------------------

test_that("set_priors() includes beta_cov with correct default", {
  p <- set_priors()
  expect_true("beta_cov" %in% names(p))
  expect_equal(p$beta_cov$dist, "normal")
  expect_equal(p$beta_cov$sd, 10)
})

test_that("set_priors() accepts custom beta_cov prior", {
  p <- set_priors(beta_cov = normal(0, 2))
  expect_equal(p$beta_cov$sd, 2)
})

test_that("set_priors() rejects wrong family for beta_cov", {
  expect_error(
    set_priors(beta_cov = cauchy(5)),
    "must be one of: normal"
  )
})

test_that("as_stan_data() includes beta_cov_prior_sd", {
  sd <- as_stan_data(set_priors())
  expect_true("beta_cov_prior_sd" %in% names(sd))
  expect_equal(sd$beta_cov_prior_sd, 10)
})

# ---------------------------------------------------------------------------
# fit.R: print and summary with covariates (mock-based)
# ---------------------------------------------------------------------------

test_that("print.meta_did_fit shows covariate coefficients (sample)", {
  # Mock a fit with covariates
  mock_draws <- function(variables, format = "draws_array") {
    if (grepl("beta_cov", variables)) {
      matrix(rnorm(100, 0.05, 0.01), ncol = 1,
             dimnames = list(NULL, "beta_cov[1]"))
    } else if (variables == "treatment_effect_mean") {
      matrix(rnorm(100, -0.15, 0.02), ncol = 1,
             dimnames = list(NULL, "treatment_effect_mean"))
    } else {
      matrix(rnorm(100), ncol = 1)
    }
  }

  fit_obj <- structure(list(
    fit = list(draws = mock_draws),
    summary_data = make_did_summary_with_cov(2),
    individual_data = NULL,
    model_flags = list(
      is_student_t_heterogeneity = 0L,
      is_design_effect = 0L
    ),
    method = "sample",
    covariate_names = c("dose"),
    cov_centers = c(dose = 2.0),
    center_covariates = TRUE
  ), class = "meta_did_fit")

  out <- capture.output(print(fit_obj))
  expect_true(any(grepl("dose", out)))
  expect_true(any(grepl("mean-centered", out)))
})

test_that("print.meta_did_fit hides covariates when none specified", {
  mock_draws <- function(variables, format = "draws_array") {
    matrix(rnorm(100, -0.15, 0.02), ncol = 1,
           dimnames = list(NULL, variables))
  }

  fit_obj <- structure(list(
    fit = list(draws = mock_draws),
    summary_data = make_did_summary_with_cov(2),
    individual_data = NULL,
    model_flags = list(
      is_student_t_heterogeneity = 0L,
      is_design_effect = 0L
    ),
    method = "sample",
    covariate_names = NULL,
    cov_centers = NULL,
    center_covariates = TRUE
  ), class = "meta_did_fit")

  out <- capture.output(print(fit_obj))
  expect_false(any(grepl("Covariate", out)))
})

# ---------------------------------------------------------------------------
# Stan smoke test: model compiles and runs with covariates
# ---------------------------------------------------------------------------

test_that("meta_did() runs with covariates (optimize)", {
  skip_if_no_stan()

  cov_df <- data.frame(dose = c(1.0, 2.0, 3.0, 4.0, 5.0))
  sim <- simulate_meta_did(
    n_studies = 5, n_control = 30, n_treatment = 30,
    true_effect = -0.10, sigma_effect = 0.02, seed = 5821,
    covariates = cov_df, beta_cov = c(-0.02)
  )
  studies <- as_summary_did(sim)

  fit <- meta_did(
    summary_data = studies,
    covariates = ~ dose,
    method = "optimize",
    seed = 5821
  )

  expect_s3_class(fit, "meta_did_fit")
  expect_equal(fit$covariate_names, "dose")
  expect_true(!is.null(fit$cov_centers))

  s <- summary(fit)
  expect_true(any(grepl("beta_cov", s$parameter)))
})

test_that("meta_did() runs without covariates (backward compat, optimize)", {
  skip_if_no_stan()

  sim <- simulate_meta_did(n_studies = 3, n_control = 30, n_treatment = 30,
                           seed = 2947)
  studies <- as_summary_did(sim)

  fit <- meta_did(
    summary_data = studies,
    method = "optimize",
    seed = 2947
  )

  expect_s3_class(fit, "meta_did_fit")
  expect_null(fit$covariate_names)

  s <- summary(fit)
  expect_false(any(grepl("beta_cov", s$parameter)))
})

test_that("meta_did() with center_covariates = FALSE works", {
  skip_if_no_stan()

  cov_df <- data.frame(dose = c(1.0, 2.0, 3.0))
  sim <- simulate_meta_did(
    n_studies = 3, n_control = 30, n_treatment = 30,
    true_effect = -0.10, sigma_effect = 0.02, seed = 3316,
    covariates = cov_df, beta_cov = c(-0.02)
  )
  studies <- as_summary_did(sim)

  fit <- meta_did(
    summary_data = studies,
    covariates = ~ dose,
    center_covariates = FALSE,
    method = "optimize",
    seed = 3316
  )

  expect_s3_class(fit, "meta_did_fit")
  expect_null(fit$cov_centers)
  expect_false(fit$center_covariates)
})

test_that("meta_did() errors on invalid covariates argument", {
  sim <- simulate_meta_did(n_studies = 3, seed = 1123)
  studies <- as_summary_did(sim)
  expect_error(
    meta_did(summary_data = studies, covariates = "dose"),
    "must be a one-sided formula"
  )
})

test_that("meta_did() with mixed designs and covariates (optimize)", {
  skip_if_no_stan()

  cov_df <- data.frame(dose = c(1.0, 2.0, 3.0, 4.0))
  sim <- simulate_meta_did(
    n_studies = 4, n_control = 30, n_treatment = 30,
    true_effect = -0.10, sigma_effect = 0.02, seed = 8452,
    covariates = cov_df, beta_cov = c(-0.01)
  )
  did_studies <- as_summary_did(sim[sim$study_id %in% c("study_1", "study_2"), ])
  rct_studies <- as_summary_rct(sim[sim$study_id %in% c("study_3", "study_4"), ])
  studies <- dplyr::bind_rows(did_studies, rct_studies)

  fit <- meta_did(
    summary_data = studies,
    covariates = ~ dose,
    method = "optimize",
    seed = 8452
  )

  expect_s3_class(fit, "meta_did_fit")
  s <- summary(fit)
  expect_true(any(grepl("beta_cov", s$parameter)))
})
