# Tests for R/priors.R

# ---------------------------------------------------------------------------
# Constructors
# ---------------------------------------------------------------------------

test_that("normal() creates a valid did_prior", {
  p <- normal(0, 10)
  expect_s3_class(p, "did_prior")
  expect_equal(p$dist, "normal")
  expect_equal(p$mean, 0)
  expect_equal(p$sd, 10)
})

test_that("cauchy() creates a valid did_prior", {
  p <- cauchy(5)
  expect_s3_class(p, "did_prior")
  expect_equal(p$dist, "cauchy")
  expect_equal(p$scale, 5)
})

test_that("gamma() creates a valid did_prior", {
  p <- gamma(2, 0.1)
  expect_s3_class(p, "did_prior")
  expect_equal(p$dist, "gamma")
  expect_equal(p$shape, 2)
  expect_equal(p$rate, 0.1)
})

test_that("constructors reject invalid inputs", {
  expect_error(normal("a", 1))
  expect_error(normal(0, -1))
  expect_error(normal(0, 0))
  expect_error(normal(c(0, 1), 1))
  expect_error(cauchy(-1))
  expect_error(cauchy(0))
  expect_error(gamma(0, 0.1))
  expect_error(gamma(2, -1))
})

# ---------------------------------------------------------------------------
# set_priors()
# ---------------------------------------------------------------------------

test_that("set_priors() returns a did_priors with all parameters", {
  p <- set_priors()
  expect_s3_class(p, "did_priors")
  expected_names <- c(
    "treatment_effect_mean", "treatment_effect_sd",
    "time_trend_mean", "time_trend_sd",
    "rho_mean", "rho_sd", "nu",
    "delta_rct", "delta_pp", "sigma"
  )
  expect_equal(names(p), expected_names)
})

test_that("set_priors() uses correct defaults", {
  p <- set_priors()
  expect_equal(p$treatment_effect_mean$mean, 0)
  expect_equal(p$treatment_effect_mean$sd, 10)
  expect_equal(p$treatment_effect_sd$scale, 5)
  expect_equal(p$nu$shape, 2)
  expect_equal(p$nu$rate, 0.1)
})

test_that("set_priors() accepts overrides", {
  p <- set_priors(treatment_effect_sd = cauchy(2))
  expect_equal(p$treatment_effect_sd$scale, 2)
  # other defaults unchanged
  expect_equal(p$treatment_effect_mean$mean, 0)
})

test_that("set_priors() rejects wrong family for parameter", {
  expect_error(
    set_priors(treatment_effect_mean = cauchy(5)),
    "must be one of: normal"
  )
  expect_error(
    set_priors(nu = normal(0, 10)),
    "must be one of: gamma"
  )
})

test_that("set_priors() rejects non-did_prior objects", {
  expect_error(
    set_priors(treatment_effect_mean = list(dist = "normal", mean = 0, sd = 10)),
    "must be created with"
  )
})

# ---------------------------------------------------------------------------
# as_stan_data()
# ---------------------------------------------------------------------------

test_that("as_stan_data() returns all expected hyperparameters", {
  sd <- as_stan_data(set_priors())
  expected <- c(
    "treatment_effect_mean_prior_mean", "treatment_effect_mean_prior_sd",
    "treatment_effect_sd_prior_scale",
    "time_trend_mean_prior_mean", "time_trend_mean_prior_sd",
    "time_trend_sd_prior_scale",
    "mu_z_prior_mean", "mu_z_prior_sd",
    "tau_z_prior_mean", "tau_z_prior_sd",
    "nu_prior_shape", "nu_prior_rate",
    "delta_rct_prior_sd", "delta_pp_prior_sd"
  )
  expect_true(all(expected %in% names(sd)))
})

test_that("as_stan_data() returns correct values for defaults", {
  sd <- as_stan_data(set_priors())
  expect_equal(sd$treatment_effect_mean_prior_mean, 0)
  expect_equal(sd$treatment_effect_mean_prior_sd, 10)
  expect_equal(sd$treatment_effect_sd_prior_scale, 5)
  expect_equal(sd$mu_z_prior_mean, 0)
  expect_equal(sd$mu_z_prior_sd, 1)
  expect_equal(sd$nu_prior_shape, 2)
  expect_equal(sd$nu_prior_rate, 0.1)
})

test_that("as_stan_data() handles normal prior on treatment_effect_sd", {
  # treatment_effect_sd allows both cauchy and normal
  p  <- set_priors(treatment_effect_sd = normal(0, 2))
  sd <- as_stan_data(p)
  expect_equal(sd$treatment_effect_sd_prior_scale, 2)
})
