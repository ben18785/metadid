# Tests for the optional multiplicative-covariate meta-regression.
# Covers normal() bounds, validator semantics (binary, no NAs, constancy
# within study, no overlap with covariates), identifiability checks (hard
# error on constant column; warning on collinearity), prepare_stan_data()
# plumbing, and (when Stan is available) recovery of a known multiplier
# from two-regime simulated data.

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

test_that("print.did_prior shows lower / upper when finite", {
  out <- capture.output(print(normal(1, 0.5, lower = 0, upper = 5)))
  expect_match(out, "lower = 0", all = FALSE)
  expect_match(out, "upper = 5", all = FALSE)
})

test_that("print.did_prior omits bounds when unbounded", {
  out <- capture.output(print(normal(0, 1)))
  expect_false(any(grepl("lower|upper", out)))
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

test_that("set_priors() accepts an upper bound on the multiplier", {
  p <- set_priors(multiplier = normal(1, 0.5, lower = 0, upper = 5))
  expect_equal(p$multiplier$upper, 5)
})

# ---------------------------------------------------------------------------
# as_stan_data() — multiplier hyperparameters
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

test_that("as_stan_data() passes finite bounds through verbatim", {
  sd <- as_stan_data(set_priors(multiplier = normal(1, 0.5, lower = 0, upper = 5)))
  expect_equal(sd$gamma_mult_upper, 5)
})

# ---------------------------------------------------------------------------
# validate_multiplicative_covariate() — basic checks
# ---------------------------------------------------------------------------

test_that("validate_multiplicative_covariate() passes a valid binary column", {
  df <- make_did_summary_with_mult()
  expect_invisible(
    validate_multiplicative_covariate("real_world", NULL, df, NULL)
  )
})

test_that("validate_multiplicative_covariate() returns NULL when arg is NULL", {
  expect_invisible(
    validate_multiplicative_covariate(NULL, NULL, make_did_summary_with_mult(), NULL)
  )
})

test_that("validate_multiplicative_covariate() rejects non-string argument", {
  df <- make_did_summary_with_mult()
  expect_error(
    validate_multiplicative_covariate(c("a", "b"), NULL, df, NULL),
    "single column name"
  )
  expect_error(
    validate_multiplicative_covariate(~ real_world, NULL, df, NULL),
    "single column name"
  )
})

test_that("validate_multiplicative_covariate() errors on missing column", {
  df <- make_did_summary_with_mult()
  expect_error(
    validate_multiplicative_covariate("missing_col", NULL, df, NULL),
    "not found in summary_data"
  )
})

test_that("validate_multiplicative_covariate() errors on non-binary values", {
  df <- make_did_summary_with_mult()
  df$real_world[1] <- 2
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, df, NULL),
    "must be binary"
  )
})

test_that("validate_multiplicative_covariate() errors on continuous covariate", {
  df <- make_did_summary_with_mult()
  df$real_world <- runif(nrow(df))
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, df, NULL),
    "must be binary"
  )
})

test_that("validate_multiplicative_covariate() rejects character column", {
  df <- make_did_summary_with_mult()
  df$real_world <- as.character(df$real_world)
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, df, NULL),
    "must be numeric"
  )
})

test_that("validate_multiplicative_covariate() errors on NA values", {
  df <- make_did_summary_with_mult()
  df$real_world[2] <- NA
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, df, NULL),
    "contains NA values"
  )
})

test_that("validate_multiplicative_covariate() errors when it varies within study (individual data)", {
  ind <- data.frame(
    study_id   = c("s1", "s1", "s1", "s1"),
    subject_id = c(1, 1, 2, 2),
    design     = "did",
    group      = c("control", "control", "treatment", "treatment"),
    time       = c("pre", "post", "pre", "post"),
    value      = rnorm(4),
    real_world = c(0, 0, 1, 1)
  )
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, NULL, ind),
    "varies within study"
  )
})

test_that("validate_multiplicative_covariate() errors when the column appears in both covariates and multiplicative_covariate", {
  df <- make_did_summary_with_mult()
  df$real_world <- as.numeric(df$real_world)
  expect_error(
    validate_multiplicative_covariate("real_world", "real_world", df, NULL),
    "listed in both"
  )
})

# ---------------------------------------------------------------------------
# validate_multiplicative_covariate() — identifiability checks
# ---------------------------------------------------------------------------

test_that("validate_multiplicative_covariate() errors when x_mult is constant across studies (all 1)", {
  df <- make_did_summary_with_mult()
  df$real_world <- 1
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, df, NULL),
    "constant across all studies"
  )
})

test_that("validate_multiplicative_covariate() errors when x_mult is constant across studies (all 0)", {
  df <- make_did_summary_with_mult()
  df$real_world <- 0
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, df, NULL),
    "constant across all studies"
  )
})

test_that("validate_multiplicative_covariate() warns on near-perfect collinearity with an additive covariate", {
  set.seed(1)
  # Make real_world and dose nearly identical at the study level:
  # all rw=0 studies have dose ≈ 0, all rw=1 studies have dose ≈ 1.
  df <- make_did_summary_with_mult(n = 10)
  df$dose       <- df$real_world + rnorm(nrow(df), sd = 0.01)
  df$real_world <- as.numeric(df$real_world)
  expect_warning(
    validate_multiplicative_covariate("real_world", "dose", df, NULL),
    "nearly collinear"
  )
})

test_that("validate_multiplicative_covariate() does not warn when collinearity is mild", {
  set.seed(2)
  # Build a 10-study frame where rw and dose are weakly correlated.
  df <- make_did_summary_with_mult(n = 10)
  df$dose       <- rnorm(nrow(df))   # independent of real_world
  df$real_world <- as.numeric(df$real_world)
  # Guard against the rare seed where independent draws still hit |cor| > 0.95
  stopifnot(abs(cor(df$dose, df$real_world)) < 0.9)
  expect_no_warning(
    validate_multiplicative_covariate("real_world", "dose", df, NULL)
  )
})

# ---------------------------------------------------------------------------
# prepare_stan_data() — flag and x_mult vectors
# ---------------------------------------------------------------------------

test_that("prepare_stan_data() sets has_multiplicative_covariate = 0 when off", {
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL,
    model_flags = list(),
    priors = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate = NULL
  )
  expect_equal(sd$has_multiplicative_covariate, 0L)
  # x_mult_did_summary still has length n_studies (filled with zeros)
  expect_equal(length(sd$x_mult_did_summary), nrow(df))
  expect_true(all(sd$x_mult_did_summary == 0))
})

test_that("prepare_stan_data() sets has_multiplicative_covariate = 1 and copies values when on", {
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL,
    model_flags = list(),
    priors = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate = "real_world"
  )
  expect_equal(sd$has_multiplicative_covariate, 1L)
  expect_equal(as.numeric(sd$x_mult_did_summary), df$real_world)
})

test_that("prepare_stan_data() does not center the multiplicative covariate", {
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL,
    model_flags = list(),
    priors = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate = "real_world",
    center_covariates = TRUE
  )
  # Values stay as {0, 1}
  expect_setequal(as.numeric(sd$x_mult_did_summary), c(0, 1))
})

test_that("prepare_stan_data() populates x_mult_did from individual data", {
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
    multiplicative_covariate = "real_world"
  )
  expect_equal(sd$has_multiplicative_covariate, 1L)
  expect_equal(length(sd$x_mult_did), 2L)  # one entry per study
  expect_setequal(as.numeric(sd$x_mult_did), c(0, 1))
})

# ---------------------------------------------------------------------------
# meta_did() plumbing
# ---------------------------------------------------------------------------

test_that("meta_did() routes the overlap error through .meta_did_core", {
  df <- make_did_summary_with_mult()
  df$real_world <- as.numeric(df$real_world)
  expect_error(
    meta_did(
      summary_data             = df,
      covariates               = ~ real_world,
      multiplicative_covariate = "real_world"
    ),
    "listed in both"
  )
})

test_that("meta_did() errors when multiplicative_covariate is constant across studies", {
  df <- make_did_summary_with_mult()
  df$real_world <- 1
  expect_error(
    meta_did(
      summary_data             = df,
      multiplicative_covariate = "real_world"
    ),
    "constant across all studies"
  )
})

test_that("meta_did() rejects non-string multiplicative_covariate", {
  df <- make_did_summary_with_mult()
  expect_error(
    meta_did(summary_data = df, multiplicative_covariate = ~ real_world),
    "single column name"
  )
})

# ---------------------------------------------------------------------------
# new_meta_did_fit() — constructor stores the name
# ---------------------------------------------------------------------------

test_that("new_meta_did_fit() stores multiplicative_covariate", {
  obj <- new_meta_did_fit(
    fit                      = NULL,
    summary_data             = NULL,
    individual_data          = NULL,
    model_flags              = list(),
    priors                   = set_priors(),
    normalisation_factors    = NULL,
    multiplicative_covariate = "real_world"
  )
  expect_s3_class(obj, "meta_did_fit")
  expect_equal(obj$multiplicative_covariate, "real_world")
})

test_that("new_meta_did_fit() defaults multiplicative_covariate to NULL", {
  obj <- new_meta_did_fit(
    fit                   = NULL,
    summary_data          = NULL,
    individual_data       = NULL,
    model_flags           = list(),
    priors                = set_priors(),
    normalisation_factors = NULL
  )
  expect_null(obj$multiplicative_covariate)
})

# ---------------------------------------------------------------------------
# Stan-gated recovery test
# ---------------------------------------------------------------------------
# Two batches of studies are simulated with population effects mu and gamma*mu.
# A binary `real_world` flag identifies which batch each study belongs to.
# Fitting with multiplicative_covariate = "real_world" should recover gamma.

test_that("Stan fit recovers a known multiplier on two-regime simulated data", {
  skip_if_no_stan()

  GAMMA_TRUE      <- 0.5
  EFFECT_EXP_RAW  <- -0.20
  EFFECT_RW_RAW   <- EFFECT_EXP_RAW * GAMMA_TRUE
  BASELINE_MEAN   <- 0.45
  SIGMA_EFFECT    <- 0.02

  sim_exp <- simulate_meta_did(
    n_studies     = 15L,
    n_control     = 80L,
    n_treatment   = 80L,
    true_effect   = EFFECT_EXP_RAW,
    sigma_effect  = SIGMA_EFFECT,
    true_trend    = 0,
    baseline_mean = BASELINE_MEAN,
    seed          = 11L
  )
  sim_rw <- simulate_meta_did(
    n_studies     = 15L,
    n_control     = 80L,
    n_treatment   = 80L,
    true_effect   = EFFECT_RW_RAW,
    sigma_effect  = SIGMA_EFFECT,
    true_trend    = 0,
    baseline_mean = BASELINE_MEAN,
    seed          = 22L
  )
  # Disambiguate study IDs across batches
  sim_rw$study_id <- paste0(sim_rw$study_id, "_rw")

  sum_exp <- as_summary_did(sim_exp)
  sum_exp$real_world <- 0
  sum_rw <- as_summary_did(sim_rw)
  sum_rw$real_world  <- 1
  studies <- rbind(sum_exp, sum_rw)

  fit <- meta_did(
    summary_data             = studies,
    multiplicative_covariate = "real_world",
    priors                   = set_priors(
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
  expect_true(lo < GAMMA_TRUE && hi > GAMMA_TRUE,
    label = sprintf("gamma_mult 90%% CI [%.3f, %.3f] covers true %.3f",
                    lo, hi, GAMMA_TRUE))

  # Sanity: convergence on the new parameter
  rhat <- fit$fit$summary("gamma_mult")$rhat
  expect_true(all(rhat < 1.05), label = sprintf("R-hat = %.3f", rhat))
})
