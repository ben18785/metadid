# Tests for multiplicative-covariate meta-regression.
# Covers validate_multiplicative_covariates(), Stan-data preparation,
# prior plumbing, and (when Stan is available) parameter recovery.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_did_summary_with_mult <- function(n = 4) {
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
    real_world          = rep(c(0, 1), length.out = n)
  )
}

# ---------------------------------------------------------------------------
# validate_multiplicative_covariates()
# ---------------------------------------------------------------------------

test_that("validate_multiplicative_covariates() passes valid binary column", {
  df <- make_did_summary_with_mult()
  expect_invisible(validate_multiplicative_covariates("real_world", df, NULL))
})

test_that("validate_multiplicative_covariates() passes with NULL names", {
  expect_invisible(validate_multiplicative_covariates(NULL, make_did_summary_with_mult(), NULL))
})

test_that("validate_multiplicative_covariates() errors on missing column", {
  df <- make_did_summary_with_mult()
  expect_error(
    validate_multiplicative_covariates(c("real_world", "missing_col"), df, NULL),
    "not found in summary_data"
  )
})

test_that("validate_multiplicative_covariates() errors on non-binary values", {
  df <- make_did_summary_with_mult()
  df$real_world[1] <- 2
  expect_error(
    validate_multiplicative_covariates("real_world", df, NULL),
    "must be binary"
  )
})

test_that("validate_multiplicative_covariates() errors on continuous covariate", {
  df <- make_did_summary_with_mult()
  df$real_world <- runif(nrow(df))
  expect_error(
    validate_multiplicative_covariates("real_world", df, NULL),
    "must be binary"
  )
})

test_that("validate_multiplicative_covariates() errors on NA values", {
  df <- make_did_summary_with_mult()
  df$real_world[2] <- NA
  expect_error(
    validate_multiplicative_covariates("real_world", df, NULL),
    "contains NA values"
  )
})

test_that("validate_multiplicative_covariates() errors when varying within study (individual)", {
  ind <- data.frame(
    study_id   = c("s1", "s1", "s1", "s1"),
    subject_id = c(1, 1, 2, 2),
    design     = "did",
    group      = c("control", "control", "treatment", "treatment"),
    time       = c("pre", "post", "pre", "post"),
    value      = rnorm(4),
    real_world = c(0, 0, 1, 1)  # varies within study
  )
  expect_error(
    validate_multiplicative_covariates("real_world", NULL, ind),
    "varies within study"
  )
})

# ---------------------------------------------------------------------------
# normal() with bounds
# ---------------------------------------------------------------------------

test_that("normal() accepts optional lower/upper", {
  p <- normal(1, 0.5, lower = 0, upper = 5)
  expect_equal(p$lower, 0)
  expect_equal(p$upper, 5)
})

test_that("normal() defaults to unbounded (-Inf, Inf)", {
  p <- normal(0, 1)
  expect_equal(p$lower, -Inf)
  expect_equal(p$upper, Inf)
})

test_that("normal() errors when upper <= lower", {
  expect_error(normal(1, 0.5, lower = 1, upper = 0), "strictly greater")
  expect_error(normal(1, 0.5, lower = 0, upper = 0), "strictly greater")
})

# ---------------------------------------------------------------------------
# set_priors() — multiplier slot
# ---------------------------------------------------------------------------

test_that("set_priors() has a multiplier slot with sensible default", {
  p <- set_priors()
  expect_true("multiplier" %in% names(p))
  expect_equal(p$multiplier$dist, "normal")
  expect_equal(p$multiplier$mean, 1)
  expect_equal(p$multiplier$lower, 0)
})

test_that("set_priors() rejects multiplier prior with negative lower bound", {
  expect_error(
    set_priors(multiplier = normal(1, 0.5, lower = -1)),
    "lower >= 0"
  )
})

test_that("set_priors() accepts user-specified multiplier bounds", {
  p <- set_priors(multiplier = normal(1, 0.5, lower = 0, upper = 5))
  expect_equal(p$multiplier$upper, 5)
})

# ---------------------------------------------------------------------------
# as_stan_data.did_priors() — multiplier hyperparameters
# ---------------------------------------------------------------------------

test_that("as_stan_data() exports gamma_mult hyperparameters", {
  sd <- as_stan_data(set_priors())
  expect_true(all(c("gamma_mult_prior_mean", "gamma_mult_prior_sd",
                    "gamma_mult_lower", "gamma_mult_upper") %in% names(sd)))
  expect_equal(sd$gamma_mult_prior_mean, 1)
  expect_equal(sd$gamma_mult_lower, 0)
})

test_that("as_stan_data() translates infinite upper bound to a large finite number", {
  sd <- as_stan_data(set_priors())  # default: upper = Inf
  expect_true(is.finite(sd$gamma_mult_upper))
  expect_gt(sd$gamma_mult_upper, 1e5)
})

test_that("as_stan_data() passes through finite bounds verbatim", {
  sd <- as_stan_data(set_priors(multiplier = normal(1, 0.5, lower = 0, upper = 5)))
  expect_equal(sd$gamma_mult_upper, 5)
})

# ---------------------------------------------------------------------------
# prepare_stan_data() — K_mult and X_mult matrices
# ---------------------------------------------------------------------------

test_that("prepare_stan_data() sets K_mult = 0 when no multiplicative covariates", {
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL,
    model_flags = list(),
    priors = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate_names = NULL
  )
  expect_equal(sd$K_mult, 0L)
  # All X_mult matrices should have 0 columns
  expect_equal(ncol(sd$X_mult_did_summary), 0L)
  expect_equal(nrow(sd$X_mult_did_summary), nrow(df))
})

test_that("prepare_stan_data() sets K_mult and X_mult correctly with one mult covariate", {
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL,
    model_flags = list(),
    priors = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate_names = "real_world"
  )
  expect_equal(sd$K_mult, 1L)
  expect_equal(ncol(sd$X_mult_did_summary), 1L)
  expect_equal(nrow(sd$X_mult_did_summary), nrow(df))
  expect_equal(as.numeric(sd$X_mult_did_summary[, 1]), df$real_world)
})

test_that("prepare_stan_data() does not center multiplicative covariates", {
  df <- make_did_summary_with_mult()  # real_world has both 0s and 1s
  sd <- prepare_stan_data(df, NULL,
    model_flags = list(),
    priors = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate_names = "real_world",
    center_covariates = TRUE  # would center additive covariates
  )
  # X_mult values must remain exactly 0 / 1
  expect_setequal(as.numeric(sd$X_mult_did_summary[, 1]), c(0, 1))
})

# ---------------------------------------------------------------------------
# meta_did() plumbing — formula parsing and overlap check
# ---------------------------------------------------------------------------

test_that("meta_did() errors when same column listed in covariates and multiplicative_covariates", {
  df <- make_did_summary_with_mult()
  df$real_world <- as.numeric(df$real_world)
  expect_error(
    meta_did(
      summary_data = df,
      covariates = ~ real_world,
      multiplicative_covariates = ~ real_world
    ),
    "listed in both"
  )
})

test_that("meta_did() errors when multiplicative_covariates is not a formula", {
  df <- make_did_summary_with_mult()
  expect_error(
    meta_did(summary_data = df, multiplicative_covariates = "real_world"),
    "one-sided formula"
  )
})

test_that("meta_did() errors when multiplicative_covariates formula is empty", {
  df <- make_did_summary_with_mult()
  expect_error(
    meta_did(summary_data = df, multiplicative_covariates = ~ 1),
    "no variables"
  )
})

# ---------------------------------------------------------------------------
# Backwards compatibility: K_mult == 0 path
# ---------------------------------------------------------------------------

test_that("Adding K_mult = 0 leaves existing keys unchanged", {
  # The Stan data list from a fit without multiplicative_covariates should be
  # a strict superset of the prior structure (adds K_mult, X_mult_*, and
  # gamma_mult prior hyperparameters but does not modify existing keys).
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL,
    model_flags = list(),
    priors = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate_names = NULL
  )
  # Sanity: existing keys present, X_cov matrices empty for K_cov = 0
  expect_true("X_cov_did_summary" %in% names(sd))
  expect_equal(ncol(sd$X_cov_did_summary), 0L)
  # New keys present at correct (degenerate) values
  expect_equal(sd$K_mult, 0L)
  expect_true("gamma_mult_lower" %in% names(sd))
  expect_true("gamma_mult_upper" %in% names(sd))
})

# ---------------------------------------------------------------------------
# Stan-gated recovery test (only runs when CmdStan is available)
# ---------------------------------------------------------------------------
# Constructs two groups of studies with effects e0 and e0 * gamma_true,
# fits with multiplicative_covariates = ~ rw, and checks the 90% CI
# of gamma_mult covers gamma_true.

test_that("Stan fit recovers a known multiplier on simulated data", {
  skip_if_no_stan()

  set.seed(20251212L)
  gamma_true     <- 0.5
  effect_exp_raw <- -0.20   # raw-scale effect for experimental (rw=0) studies
  effect_rw_raw  <- effect_exp_raw * gamma_true
  baseline_mean  <- 0.45
  sigma_effect   <- 0.02

  # Simulate two independent batches and bind them. Each batch has its own
  # true population effect; rw flags which is which.
  sim_exp <- simulate_meta_did(
    n_studies   = 12, n_control = 80, n_treatment = 80,
    true_effect = effect_exp_raw, sigma_effect = sigma_effect,
    true_trend  = 0, baseline_mean = baseline_mean,
    seed = 11L
  )
  sim_rw <- simulate_meta_did(
    n_studies   = 12, n_control = 80, n_treatment = 80,
    true_effect = effect_rw_raw,  sigma_effect = sigma_effect,
    true_trend  = 0, baseline_mean = baseline_mean,
    seed = 22L
  )
  # Disambiguate study IDs across batches
  sim_rw$study_id <- paste0(sim_rw$study_id, "_rw")

  sum_exp <- as_summary_did(sim_exp)
  sum_exp$real_world <- 0
  sum_rw  <- as_summary_did(sim_rw)
  sum_rw$real_world  <- 1
  studies <- rbind(sum_exp, sum_rw)

  model <- get_compiled_model()
  skip_if(is.null(model), "Stan model could not be compiled")
  local_mocked_bindings(
    stan_package_model = function(...) model,
    .package = "instantiate"
  )

  fit <- meta_did(
    summary_data              = studies,
    multiplicative_covariates = ~ real_world,
    priors                    = set_priors(
      multiplier = normal(1, 0.5, lower = 0, upper = 5)
    ),
    chains        = 2L,
    iter_warmup   = 800L,
    iter_sampling = 800L,
    seed          = 9931L,
    refresh       = 0
  )

  draws <- fit$fit$draws("gamma_mult", format = "matrix")
  lo <- unname(stats::quantile(draws, 0.05))
  hi <- unname(stats::quantile(draws, 0.95))
  expect_true(lo < gamma_true && hi > gamma_true,
    label = sprintf("gamma_mult 90%% CI [%.3f, %.3f] covers true value %.3f",
                    lo, hi, gamma_true))
})

# ---------------------------------------------------------------------------
# print.did_prior with bounds
# ---------------------------------------------------------------------------

test_that("print.did_prior omits bounds when unbounded", {
  out <- capture.output(print(normal(0, 1)))
  expect_match(out, "normal", all = FALSE)
  expect_false(any(grepl("lower", out)))
  expect_false(any(grepl("upper", out)))
})

test_that("print.did_prior shows lower bound when finite", {
  out <- capture.output(print(normal(1, 0.5, lower = 0)))
  expect_match(out, "lower = 0", all = FALSE)
  expect_false(any(grepl("upper", out)))
})

test_that("print.did_prior shows upper bound when finite", {
  out <- capture.output(print(normal(1, 0.5, upper = 5)))
  expect_match(out, "upper = 5", all = FALSE)
  expect_false(any(grepl("lower", out)))
})

test_that("print.did_prior shows both bounds when finite", {
  out <- capture.output(print(normal(1, 0.5, lower = 0, upper = 5)))
  expect_match(out, "lower = 0", all = FALSE)
  expect_match(out, "upper = 5", all = FALSE)
})

# ---------------------------------------------------------------------------
# validate_multiplicative_covariates() additional edge cases
# ---------------------------------------------------------------------------

test_that("validate_multiplicative_covariates() rejects character column", {
  df <- make_did_summary_with_mult()
  df$real_world <- as.character(df$real_world)
  expect_error(
    validate_multiplicative_covariates("real_world", df, NULL),
    "must be numeric"
  )
})

test_that("validate_multiplicative_covariates() rejects logical column", {
  # is.numeric(c(TRUE, FALSE)) is FALSE in base R, so logical columns must be
  # coerced explicitly to integer/numeric before being passed in. The validator
  # rejects them with the "must be numeric" message to make this explicit.
  df <- make_did_summary_with_mult()
  df$real_world <- as.logical(df$real_world)
  expect_error(
    validate_multiplicative_covariates("real_world", df, NULL),
    "must be numeric"
  )
})

test_that("validate_multiplicative_covariates() rejects missing column in individual_data", {
  ind <- data.frame(
    study_id   = c("s1", "s1"),
    subject_id = c(1, 1),
    design     = "did",
    group      = c("control", "treatment"),
    time       = c("post", "post"),
    value      = rnorm(2)
    # no real_world column
  )
  expect_error(
    validate_multiplicative_covariates("real_world", NULL, ind),
    "not found in individual_data"
  )
})

test_that("validate_multiplicative_covariates() validates both data sources when supplied", {
  # Same column name must be valid in both frames. Build a constant-within-study
  # individual frame whose study_id values do not overlap the summary frame.
  sum_df <- make_did_summary_with_mult()
  ind <- data.frame(
    study_id   = rep(c("ind_a", "ind_b"), each = 4),
    subject_id = rep(1:2, times = 4),
    design     = "did",
    group      = rep(c("control", "control", "treatment", "treatment"), 2),
    time       = rep(c("pre", "post"), 4),
    value      = rnorm(8),
    real_world = rep(c(0, 1), each = 4)  # constant within each study
  )
  expect_invisible(
    validate_multiplicative_covariates("real_world", sum_df, ind)
  )
})

# ---------------------------------------------------------------------------
# meta_did_general() plumbing
# ---------------------------------------------------------------------------

test_that("meta_did_general() exposes the multiplicative_covariates argument", {
  # Trigger the same "listed in both" error through meta_did_general() —
  # this exercises the argument's plumbing without needing to actually fit.
  df <- make_did_summary_with_mult()
  expect_error(
    meta_did_general(
      summary_data              = df,
      covariates                = ~ real_world,
      multiplicative_covariates = ~ real_world
    ),
    "listed in both"
  )
})

test_that("meta_did_general() rejects non-formula multiplicative_covariates", {
  df <- make_did_summary_with_mult()
  expect_error(
    meta_did_general(
      summary_data              = df,
      multiplicative_covariates = "real_world"  # string, not formula
    ),
    "one-sided formula"
  )
})

# ---------------------------------------------------------------------------
# new_meta_did_fit() constructor
# ---------------------------------------------------------------------------

test_that("new_meta_did_fit() stores multiplicative_covariate_names", {
  obj <- new_meta_did_fit(
    fit                            = NULL,
    summary_data                   = NULL,
    individual_data                = NULL,
    model_flags                    = list(),
    priors                         = set_priors(),
    normalisation_factors          = NULL,
    multiplicative_covariate_names = c("rw", "fs")
  )
  expect_s3_class(obj, "meta_did_fit")
  expect_equal(obj$multiplicative_covariate_names, c("rw", "fs"))
})

test_that("new_meta_did_fit() defaults multiplicative_covariate_names to NULL", {
  obj <- new_meta_did_fit(
    fit                   = NULL,
    summary_data          = NULL,
    individual_data       = NULL,
    model_flags           = list(),
    priors                = set_priors(),
    normalisation_factors = NULL
  )
  expect_null(obj$multiplicative_covariate_names)
})

# ---------------------------------------------------------------------------
# prepare_stan_data() with individual data
# ---------------------------------------------------------------------------

test_that("prepare_stan_data() populates X_mult_did from individual data", {
  # The summary path was tested earlier; this covers the individual-level
  # extraction in .extract_cov_matrix_individual.
  ind <- data.frame(
    study_id   = rep(c("ind_a", "ind_b"), each = 8),
    subject_id = rep(1:4, times = 4),
    design     = "did",
    group      = rep(c("control", "control", "treatment", "treatment"), 4),
    time       = rep(c("pre", "post"), 8),
    value      = rnorm(16, mean = 0.45, sd = 0.1),
    real_world = rep(c(0, 1), each = 8)
  )
  sd <- prepare_stan_data(
    summary_data    = NULL,
    individual_data = ind,
    model_flags     = list(),
    priors          = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate_names = "real_world"
  )
  expect_equal(sd$K_mult, 1L)
  expect_equal(nrow(sd$X_mult_did), 2L)  # one row per study
  expect_equal(ncol(sd$X_mult_did), 1L)
  expect_setequal(as.numeric(sd$X_mult_did[, 1]), c(0, 1))
})
