# Tests for the optional multiplicative-covariate meta-regression.
# Covers the multiplier prior slot, validator semantics (categorical levels,
# no NAs, constancy within study, no overlap with covariates),
# identifiability checks (hard error on constant column; warning on
# collinearity), prepare_stan_data() global level coding, and (when Stan is
# available) recovery of known multipliers from two- and three-regime
# simulated data.

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
# set_priors() / as_stan_data() — multiplier slot
# ---------------------------------------------------------------------------

test_that("set_priors() has a multiplier slot with sensible default", {
  p <- set_priors()
  expect_true("multiplier" %in% names(p))
  expect_equal(p$multiplier$dist, "lognormal")
  expect_equal(p$multiplier$meanlog, 0)
  expect_equal(p$multiplier$sdlog, 0.7)
})

test_that("set_priors() accepts a custom multiplier prior", {
  p <- set_priors(multiplier = lognormal(0, 0.25))
  expect_equal(p$multiplier$sdlog, 0.25)
})

test_that("set_priors() rejects a non-lognormal multiplier prior", {
  expect_error(set_priors(multiplier = normal(1, 0.5)), "multiplier")
  expect_error(set_priors(multiplier = cauchy(1)), "multiplier")
})

test_that("as_stan_data() exports multiplier hyperparameters on the log scale", {
  sd <- as_stan_data(set_priors())
  expect_true(all(c("effect_multiplier_prior_meanlog",
                    "effect_multiplier_prior_sdlog") %in% names(sd)))
  expect_equal(sd$effect_multiplier_prior_meanlog, 0)
  expect_equal(sd$effect_multiplier_prior_sdlog, 0.7)
  # The positivity comes from the log-normal prior itself, not from data.
  expect_false(any(c("effect_multiplier_lower", "effect_multiplier_upper") %in% names(sd)))
})

# ---------------------------------------------------------------------------
# validate_multiplicative_covariate() — basic checks
# ---------------------------------------------------------------------------

test_that("validate_multiplicative_covariate() passes a valid two-level column", {
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

test_that("validate_multiplicative_covariate() rejects a multi-element character vector", {
  df <- make_did_summary_with_mult()
  expect_error(
    validate_multiplicative_covariate(c("a", "b"), NULL, df, NULL),
    "single column name"
  )
})

test_that("validate_multiplicative_covariate() accepts a one-sided formula", {
  df <- make_did_summary_with_mult()
  expect_invisible(
    validate_multiplicative_covariate(~ real_world, NULL, df, NULL)
  )
})

test_that("validate_multiplicative_covariate() errors on missing column", {
  df <- make_did_summary_with_mult()
  expect_error(
    validate_multiplicative_covariate("missing_col", NULL, df, NULL),
    "not found in summary_data"
  )
})

test_that("validate_multiplicative_covariate() accepts a multi-level numeric column", {
  df <- make_did_summary_with_mult()
  df$real_world[1] <- 2
  expect_silent(
    validate_multiplicative_covariate("real_world", NULL, df, NULL)
  )
})

test_that("validate_multiplicative_covariate() errors on a continuous covariate", {
  df <- make_did_summary_with_mult(n = 8)
  df$real_world <- seq_len(nrow(df)) * 0.137
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, df, NULL),
    "looks continuous"
  )
})

test_that("validate_multiplicative_covariate() accepts a character column", {
  df <- make_did_summary_with_mult()
  df$real_world <- ifelse(df$real_world == 1, "real_world", "experimental")
  expect_silent(
    validate_multiplicative_covariate("real_world", NULL, df, NULL)
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

# When both summary and individual data are supplied, the multiplicative
# covariate column must be present in BOTH frames (it identifies every study).

make_did_individual_with_mult <- function(mult_col = "real_world") {
  ind <- data.frame(
    study_id   = rep(c("s1", "s2"), each = 4),
    subject_id = rep(1:2, times = 4),
    design     = "did",
    group      = rep(c("control", "control", "treatment", "treatment"), 2),
    time       = rep(c("pre", "post"), 4),
    value      = rnorm(8)
  )
  if (!is.null(mult_col)) ind[[mult_col]] <- rep(c(0, 1), each = 4)
  ind
}

test_that("validate_multiplicative_covariate() errors when the column is missing from individual_data", {
  df  <- make_did_summary_with_mult()              # has real_world
  ind <- make_did_individual_with_mult(mult_col = NULL)  # lacks real_world
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, df, ind),
    "not found in individual_data"
  )
})

test_that("validate_multiplicative_covariate() errors when the column is missing from summary_data", {
  df  <- make_did_summary_with_mult()
  df$real_world <- NULL                            # present only in individual_data
  ind <- make_did_individual_with_mult()
  expect_error(
    validate_multiplicative_covariate("real_world", NULL, df, ind),
    "not found in summary_data"
  )
})

test_that("validate_multiplicative_covariate() accepts the covariate present in both frames", {
  df  <- make_did_summary_with_mult()
  ind <- make_did_individual_with_mult()
  expect_invisible(
    validate_multiplicative_covariate("real_world", NULL, df, ind)
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

test_that("prepare_stan_data() zero-fills x_mult vectors when the feature is off", {
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL,
    model_flags = list(),
    priors = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate = NULL
  )
  expect_equal(sd$n_effect_multipliers, 0L)
  # x_mult_did_summary still has length n_studies (filled with zeros)
  expect_equal(length(sd$x_mult_did_summary), nrow(df))
  expect_true(all(sd$x_mult_did_summary == 0))
})

test_that("prepare_stan_data() codes a two-level {0, 1} covariate as a single multiplier", {
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL,
    model_flags = list(),
    priors = set_priors(),
    covariate_names = NULL,
    multiplicative_covariate = "real_world"
  )
  expect_equal(sd$n_effect_multipliers, 1L)
  expect_equal(as.numeric(sd$x_mult_did_summary), df$real_world)
  # array[] int declaration on the Stan side: storage must be integer
  expect_true(is.integer(sd$x_mult_did_summary))
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
  expect_equal(sd$n_effect_multipliers, 1L)
  expect_equal(length(sd$x_mult_did), 2L)  # one entry per study
  expect_setequal(as.numeric(sd$x_mult_did), c(0, 1))
  expect_true(is.integer(sd$x_mult_did))
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

test_that("meta_did() rejects more than two multiplicative covariates", {
  df <- make_did_summary_with_mult()
  expect_error(
    meta_did(summary_data = df,
             multiplicative_covariate = ~ delivery + intensity + duration),
    "one or two columns"
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
    multiplicative_covariate = list(name   = "real_world",
                                    levels = c("0", "1"))
  )
  expect_s3_class(obj, "meta_did_fit")
  expect_equal(obj$multiplicative_covariate$name, "real_world")
  expect_equal(obj$multiplicative_covariate$levels, c("0", "1"))
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
# Two batches of studies are simulated with population effects mu and multiplier*mu.
# A two-level `real_world` flag identifies which batch each study belongs to.
# Fitting with multiplicative_covariate = "real_world" should recover the multiplier.

test_that("Stan fit recovers a known multiplier on two-regime simulated data", {
  skip_if_no_stan()

  MULT_TRUE      <- 0.5
  EFFECT_EXP_RAW  <- -0.20
  EFFECT_RW_RAW   <- EFFECT_EXP_RAW * MULT_TRUE
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
      multiplier = lognormal(0, 0.7)
    ),
    chains        = 2L,
    iter_warmup   = 800L,
    iter_sampling = 800L,
    seed          = 9931L,
    refresh       = 0
  )

  draws <- fit$fit$draws("effect_multiplier", format = "matrix")
  lo <- unname(stats::quantile(draws, 0.05))
  hi <- unname(stats::quantile(draws, 0.95))
  expect_true(lo < MULT_TRUE && hi > MULT_TRUE,
    label = sprintf("multiplier 90%% CI [%.3f, %.3f] covers true %.3f",
                    lo, hi, MULT_TRUE))

  # Sanity: convergence on the new parameter
  rhat <- fit$fit$summary("effect_multiplier")$rhat
  expect_true(all(rhat < 1.05), label = sprintf("R-hat = %.3f", rhat))
})


# ---------------------------------------------------------------------------
# Categorical levels — global coding, reference control, plumbing
# ---------------------------------------------------------------------------

test_that("prepare_stan_data() codes a three-level character covariate with a reference", {
  df <- make_did_summary_with_mult(n = 6)
  df$setting <- rep(c("experimental", "rw_field", "rw_lab"), each = 2)
  sd <- prepare_stan_data(df, NULL, model_flags = list(), priors = set_priors(),
    covariate_names = NULL, multiplicative_covariate = "setting")
  expect_equal(sd$n_effect_multipliers, 2L)
  # alphabetical levels: experimental (reference = 0), rw_field (1), rw_lab (2)
  expect_equal(as.integer(sd$x_mult_did_summary), rep(c(0L, 1L, 2L), each = 2))
  expect_true(is.integer(sd$x_mult_did_summary))
  expect_equal(attr(sd, "multiplier_levels"),
               c("experimental", "rw_field", "rw_lab"))
})

test_that("prepare_stan_data() respects declared factor order (reference control)", {
  df <- make_did_summary_with_mult(n = 6)
  df$setting <- factor(rep(c("experimental", "rw_field", "rw_lab"), each = 2),
                       levels = c("rw_lab", "experimental", "rw_field"))
  sd <- prepare_stan_data(df, NULL, model_flags = list(), priors = set_priors(),
    covariate_names = NULL, multiplicative_covariate = "setting")
  expect_equal(attr(sd, "multiplier_levels"),
               c("rw_lab", "experimental", "rw_field"))
  expect_equal(as.integer(sd$x_mult_did_summary), rep(c(1L, 2L, 0L), each = 2))
})

test_that("prepare_stan_data() drops unused declared factor levels", {
  df <- make_did_summary_with_mult(n = 4)
  df$setting <- factor(rep(c("a", "b"), 2), levels = c("a", "b", "c"))
  sd <- prepare_stan_data(df, NULL, model_flags = list(), priors = set_priors(),
    covariate_names = NULL, multiplicative_covariate = "setting")
  expect_equal(attr(sd, "multiplier_levels"), c("a", "b"))
  expect_equal(sd$n_effect_multipliers, 1L)
})

test_that("prepare_stan_data() sorts numeric levels numerically", {
  df <- make_did_summary_with_mult(n = 6)
  df$dose_group <- rep(c(10, 2, 7), each = 2)
  sd <- prepare_stan_data(df, NULL, model_flags = list(), priors = set_priors(),
    covariate_names = NULL, multiplicative_covariate = "dose_group")
  expect_equal(attr(sd, "multiplier_levels"), c("2", "7", "10"))
  expect_equal(as.integer(sd$x_mult_did_summary), rep(c(2L, 0L, 1L), each = 2))
})

test_that("prepare_stan_data() codes levels jointly across summary and individual data", {
  df <- make_did_summary_with_mult(n = 4)
  df$setting <- rep(c("a", "c"), 2)
  ind <- data.frame(
    study_id   = rep(c("ind_a", "ind_b"), each = 8),
    subject_id = rep(1:4, times = 4),
    design     = "did",
    group      = rep(c("control", "control", "treatment", "treatment"), 4),
    time       = rep(c("pre", "post"), 8),
    value      = rnorm(16, mean = 0.45, sd = 0.1),
    setting    = rep(c("a", "b"), each = 8)
  )
  sd <- prepare_stan_data(df, ind, model_flags = list(), priors = set_priors(),
    covariate_names = NULL, multiplicative_covariate = "setting")
  expect_equal(attr(sd, "multiplier_levels"), c("a", "b", "c"))
  expect_equal(sd$n_effect_multipliers, 2L)
  expect_equal(as.integer(sd$x_mult_did_summary), rep(c(0L, 2L), 2))
  expect_equal(as.integer(sd$x_mult_did), c(0L, 1L))
})

test_that("prepare_stan_data() errors on conflicting declared factor orders", {
  df <- make_did_summary_with_mult(n = 4)
  df$setting <- factor(rep(c("a", "b"), 2), levels = c("a", "b"))
  ind <- data.frame(
    study_id   = rep(c("ind_a", "ind_b"), each = 8),
    subject_id = rep(1:4, times = 4),
    design     = "did",
    group      = rep(c("control", "control", "treatment", "treatment"), 4),
    time       = rep(c("pre", "post"), 8),
    value      = rnorm(16, mean = 0.45, sd = 0.1),
    setting    = factor(rep(c("a", "b"), each = 8), levels = c("b", "a"))
  )
  expect_error(
    prepare_stan_data(df, ind, model_flags = list(), priors = set_priors(),
      covariate_names = NULL, multiplicative_covariate = "setting"),
    "different level sets/orders"
  )
})

test_that("prepare_stan_data() sets n_effect_multipliers = 0 when the feature is off", {
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL, model_flags = list(), priors = set_priors(),
    covariate_names = NULL, multiplicative_covariate = NULL)
  expect_equal(sd$n_effect_multipliers, 0L)
  expect_equal(sd$n_effect_multipliers2, 0L)
  expect_null(attr(sd, "multiplier_levels"))
  expect_null(attr(sd, "mult_covariates"))
})

# ---------------------------------------------------------------------------
# Two multiplicative covariates (product of factors)
# ---------------------------------------------------------------------------

test_that("prepare_stan_data() zero-fills the second covariate when only one is given", {
  df <- make_did_summary_with_mult()
  sd <- prepare_stan_data(df, NULL, model_flags = list(), priors = set_priors(),
    covariate_names = NULL, multiplicative_covariate = "real_world")
  expect_equal(sd$n_effect_multipliers, 1L)
  expect_equal(sd$n_effect_multipliers2, 0L)
  expect_equal(length(sd$x_mult2_did_summary), nrow(df))
  expect_true(all(sd$x_mult2_did_summary == 0))
  expect_true(is.integer(sd$x_mult2_did_summary))
  mc <- attr(sd, "mult_covariates")
  expect_equal(length(mc), 1L)
  expect_equal(mc[[1]]$name, "real_world")
})

test_that("prepare_stan_data() codes two covariates as independent factors (~ a + b)", {
  df <- make_did_summary_with_mult(n = 8)
  df$delivery  <- rep(c("in_person", "remote"), length.out = nrow(df))    # 2 levels
  df$intensity <- rep(c("low", "high", "medium"), length.out = nrow(df))  # 3 levels
  sd <- prepare_stan_data(df, NULL, model_flags = list(), priors = set_priors(),
    covariate_names = NULL, multiplicative_covariate = ~ delivery + intensity)

  expect_equal(sd$n_effect_multipliers, 1L)
  expect_equal(sd$n_effect_multipliers2, 2L)
  expect_equal(as.integer(sd$x_mult_did_summary),
               rep(c(0L, 1L), length.out = 8))
  # intensity = low,high,medium,... -> alphabetical high<low<medium -> codes 1,0,2,...
  expect_equal(as.integer(sd$x_mult2_did_summary),
               rep(c(1L, 0L, 2L), length.out = 8))
  expect_true(is.integer(sd$x_mult2_did_summary))

  mc <- attr(sd, "mult_covariates")
  expect_equal(length(mc), 2L)
  expect_equal(mc[[1]]$name, "delivery")
  expect_equal(mc[[2]]$name, "intensity")
  expect_equal(mc[[2]]$levels, c("high", "low", "medium"))
})

test_that("validate_multiplicative_covariate() accepts a three-level factor", {
  df <- make_did_summary_with_mult(n = 6)
  df$real_world <- factor(rep(c("exp", "lab", "field"), each = 2))
  expect_silent(validate_multiplicative_covariate("real_world", NULL, df, NULL))
})

test_that("meta_did fit objects carry the covariate name and levels together", {
  obj <- new_meta_did_fit(
    fit = NULL, summary_data = NULL, individual_data = NULL,
    model_flags = list(), priors = set_priors(), normalisation_factors = NULL,
    multiplicative_covariate = list(name   = "setting",
                                    levels = c("a", "b", "c"))
  )
  expect_equal(obj$multiplicative_covariate$levels, c("a", "b", "c"))
})

test_that("Stan fit recovers level-specific multipliers on three-regime simulated data", {
  skip_if_no_stan()

  EFFECT_EXP_RAW <- -0.20
  MULT_LAB       <- 0.6
  MULT_FIELD     <- 0.3
  BASELINE_MEAN  <- 0.45
  SIGMA_EFFECT   <- 0.02

  sims <- list(
    experimental = simulate_meta_did(
      n_studies = 12L, n_control = 70L, n_treatment = 70L,
      true_effect = EFFECT_EXP_RAW, sigma_effect = SIGMA_EFFECT,
      true_trend = 0, baseline_mean = BASELINE_MEAN, seed = 31L),
    rw_lab = simulate_meta_did(
      n_studies = 12L, n_control = 70L, n_treatment = 70L,
      true_effect = EFFECT_EXP_RAW * MULT_LAB, sigma_effect = SIGMA_EFFECT,
      true_trend = 0, baseline_mean = BASELINE_MEAN, seed = 32L),
    rw_field = simulate_meta_did(
      n_studies = 12L, n_control = 70L, n_treatment = 70L,
      true_effect = EFFECT_EXP_RAW * MULT_FIELD, sigma_effect = SIGMA_EFFECT,
      true_trend = 0, baseline_mean = BASELINE_MEAN, seed = 33L)
  )
  studies <- do.call(rbind, lapply(names(sims), function(nm) {
    s <- sims[[nm]]
    s$study_id <- paste0(s$study_id, "_", nm)
    out <- as_summary_did(s)
    out$setting <- nm
    out
  }))
  studies$setting <- factor(studies$setting,
                            levels = c("experimental", "rw_lab", "rw_field"))

  fit <- meta_did(
    summary_data             = studies,
    multiplicative_covariate = "setting",
    priors                   = set_priors(multiplier = lognormal(0, 0.7)),
    chains        = 2L,
    iter_warmup   = 800L,
    iter_sampling = 800L,
    seed          = 9932L,
    refresh       = 0
  )

  expect_equal(fit$multiplicative_covariate$name, "setting")
  expect_equal(fit$multiplicative_covariate$levels,
               c("experimental", "rw_lab", "rw_field"))

  draws <- fit$fit$draws("effect_multiplier", format = "matrix")
  expect_equal(ncol(draws), 2L)
  true_m <- c(MULT_LAB, MULT_FIELD)
  for (j in 1:2) {
    lo <- unname(stats::quantile(draws[, j], 0.05))
    hi <- unname(stats::quantile(draws[, j], 0.95))
    expect_true(lo < true_m[j] && hi > true_m[j],
      label = sprintf("multiplier %d 90%% CI [%.3f, %.3f] covers true %.3f",
                      j, lo, hi, true_m[j]))
  }

  rhat <- fit$fit$summary("effect_multiplier")$rhat
  expect_true(all(rhat < 1.05),
              label = paste("R-hat =", paste(round(rhat, 3), collapse = ", ")))

  # summary() rows are labelled with level names
  sm <- summary(fit)
  expect_true(all(c("effect_multiplier[rw_lab]",
                    "effect_multiplier[rw_field]") %in% sm$parameter))
})

test_that("Stan fit recovers two multiplicative covariates (product of factors)", {
  skip_if_no_stan()

  MU    <- -0.30
  ALPHA <- 0.6   # remote-delivery multiplier  (covariate 1)
  BETA  <- 0.5   # low-intensity multiplier    (covariate 2)
  BASE  <- 0.45
  SIG   <- 0.02

  make_group <- function(delivery, intensity, mult, seed, tag) {
    sim <- simulate_meta_did(
      n_studies = 10L, n_control = 80L, n_treatment = 80L,
      true_effect = MU * mult, sigma_effect = SIG,
      true_trend = 0, baseline_mean = BASE, seed = seed)
    sim$study_id  <- paste0(sim$study_id, "_", tag)
    out           <- as_summary_did(sim)
    out$delivery  <- delivery
    out$intensity <- intensity
    out
  }
  studies <- rbind(
    make_group("in_person", "high", 1,            41L, "ih"),
    make_group("remote",    "high", ALPHA,        42L, "rh"),
    make_group("in_person", "low",  BETA,         43L, "il"),
    make_group("remote",    "low",  ALPHA * BETA, 44L, "rl")
  )

  fit <- meta_did(
    summary_data             = studies,
    multiplicative_covariate = ~ delivery + intensity,
    priors                   = set_priors(multiplier = lognormal(0, 0.7)),
    chains        = 2L,
    iter_warmup   = 800L,
    iter_sampling = 800L,
    seed          = 9933L,
    refresh       = 0
  )

  a <- fit$fit$draws("effect_multiplier",  format = "matrix")
  b <- fit$fit$draws("effect_multiplier2", format = "matrix")
  a_lo <- unname(stats::quantile(a, 0.05)); a_hi <- unname(stats::quantile(a, 0.95))
  b_lo <- unname(stats::quantile(b, 0.05)); b_hi <- unname(stats::quantile(b, 0.95))
  expect_true(a_lo < ALPHA && a_hi > ALPHA,
    label = sprintf("alpha 90%% CI [%.3f, %.3f] covers %.3f", a_lo, a_hi, ALPHA))
  expect_true(b_lo < BETA && b_hi > BETA,
    label = sprintf("beta 90%% CI [%.3f, %.3f] covers %.3f", b_lo, b_hi, BETA))

  expect_equal(length(fit$multiplicative_covariate), 2L)
  expect_equal(fit$multiplicative_covariate[[1]]$name, "delivery")
  expect_equal(fit$multiplicative_covariate[[2]]$name, "intensity")

  sm <- summary(fit)
  expect_true(any(grepl("effect_multiplier\\[delivery:", sm$parameter)))
  expect_true(any(grepl("effect_multiplier\\[intensity:", sm$parameter)))

  rhat1 <- fit$fit$summary("effect_multiplier")$rhat
  rhat2 <- fit$fit$summary("effect_multiplier2")$rhat
  expect_true(all(c(rhat1, rhat2) < 1.05))
})


# ---------------------------------------------------------------------------
# Level-coding helpers — direct unit tests
# ---------------------------------------------------------------------------
# These exercise the extracted internal helpers in isolation (rather than only
# end-to-end through prepare_stan_data()).

test_that(".normalise_mult_covariate() handles NULL, a string, and formulas", {
  expect_equal(.normalise_mult_covariate(NULL), character(0))
  expect_equal(.normalise_mult_covariate("a"), "a")
  expect_equal(.normalise_mult_covariate(~ a), "a")
  expect_equal(.normalise_mult_covariate(~ a + b), c("a", "b"))
})

test_that(".normalise_mult_covariate() rejects invalid specifications", {
  expect_error(.normalise_mult_covariate(c("a", "b")), "single column name")
  expect_error(.normalise_mult_covariate(~ a + b + c), "one or two columns")
  expect_error(.normalise_mult_covariate(~ a:b), "interactions")
  expect_error(.normalise_mult_covariate(42), "single column name or a")
})

test_that(".study_first_value() returns one value per study in first-appearance order", {
  ind <- data.frame(
    study_id = c("a", "a", "b", "b", "b"),
    setting  = c("p", "p", "q", "q", "q"),
    stringsAsFactors = FALSE
  )
  expect_equal(.study_first_value(ind, "setting"), c("p", "q"))
})

test_that(".study_first_value() returns character(0) for NULL or empty input", {
  expect_equal(.study_first_value(NULL, "setting"), character(0))
  empty <- data.frame(study_id = character(0), setting = character(0))
  expect_equal(.study_first_value(empty, "setting"), character(0))
})

test_that(".mult_levels() sorts numeric levels ascending and returns character", {
  expect_equal(.mult_levels(list(c(10, 2, 7))), c("2", "7", "10"))
})

test_that(".mult_levels() sorts character and logical levels alphabetically", {
  expect_equal(.mult_levels(list(c("b", "a", "c"))), c("a", "b", "c"))
  expect_equal(.mult_levels(list(c(TRUE, FALSE, TRUE))), c("FALSE", "TRUE"))
})

test_that(".mult_levels() respects declared factor order and drops unused levels", {
  f <- factor(c("a", "b"), levels = c("b", "a", "c"))
  expect_equal(.mult_levels(list(f)), c("b", "a"))
})

test_that(".mult_levels() errors on conflicting factor orders across frames", {
  expect_error(
    .mult_levels(list(factor("a", levels = c("a", "b")),
                      factor("b", levels = c("b", "a")))),
    "different level sets/orders"
  )
})

test_that(".mult_levels() errors when values are not among the declared factor levels", {
  expect_error(
    .mult_levels(list(factor(c("a", "z"), levels = c("a", "b")))),
    "not among the declared"
  )
})

test_that(".mult_levels() ignores empty frames and returns character(0) for none", {
  expect_equal(.mult_levels(list(character(0), c("x", "y"))), c("x", "y"))
  expect_equal(.mult_levels(list()), character(0))
})

test_that(".compute_mult_levels() returns character(0) when the feature is off", {
  expect_equal(
    .compute_mult_levels(NULL, summary_frames = list(), individual_frames = list()),
    character(0)
  )
})

test_that(".compute_mult_levels() errors when fewer than two levels are observed", {
  expect_error(
    .compute_mult_levels(
      "setting",
      summary_frames    = list(data.frame(setting = c("a", "a"))),
      individual_frames = list()
    ),
    "at least two distinct values"
  )
})

test_that(".compute_mult_levels() pools levels across summary and individual frames", {
  ind <- data.frame(study_id = c("s1", "s1", "s2"), setting = c("b", "b", "c"))
  levs <- .compute_mult_levels(
    "setting",
    summary_frames    = list(data.frame(setting = c("a", "c"))),
    individual_frames = list(ind)
  )
  expect_equal(levs, c("a", "b", "c"))
})

test_that(".extract_mult_vec_summary() returns 0-based integer codes (reference = 0)", {
  d <- data.frame(setting = c("x", "y", "x"), stringsAsFactors = FALSE)
  v <- .extract_mult_vec_summary(d, "setting", c("x", "y"))
  expect_equal(as.integer(v), c(0L, 1L, 0L))
  expect_true(is.integer(v))
})

test_that(".extract_mult_vec_summary() zero-fills when the feature is off", {
  d <- data.frame(setting = c("x", "y", "x"))
  expect_equal(as.integer(.extract_mult_vec_summary(d, NULL, character(0))),
               c(0L, 0L, 0L))
})

test_that(".extract_mult_vec_individual() codes one entry per study against shared levels", {
  ind <- data.frame(study_id = c("a", "a", "b"), setting = c("p", "p", "q"))
  expect_equal(as.integer(.extract_mult_vec_individual(ind, "setting", c("p", "q"))),
               c(0L, 1L))
})

test_that(".extract_mult_vec_individual() returns integer(0) for an empty frame", {
  empty <- data.frame(study_id = character(0), setting = character(0))
  expect_equal(as.integer(.extract_mult_vec_individual(empty, "setting", c("p", "q"))),
               integer(0))
})
