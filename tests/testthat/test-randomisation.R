# Tests for the randomisation-aware baseline-imbalance model.
#
# gamma_mode per study: 0 = fixed zero, 1 = non-randomised, 2 = randomised.
# gamma_scale (s_i) is the sampling SD of the baseline contrast: data for
# summary studies, built inside Stan for individual-level ones.

mk_summary <- function(randomisation = NULL, ...) {
  d <- data.frame(
    study_id            = c("d1", "d2", "r1", "r2"),
    design              = c("did", "did", "rct", "rct"),
    n_control           = c(100, 100, 100, 100),
    n_treatment         = c(100, 100, 100, 100),
    mean_pre_control    = c(0.5, 0.5, NA, NA),
    mean_post_control   = c(0.48, 0.48, 0.5, 0.5),
    sd_pre_control      = c(0.1, 0.1, NA, NA),
    sd_post_control     = c(0.1, 0.1, 0.1, 0.1),
    mean_pre_treatment  = c(0.5, 0.5, NA, NA),
    mean_post_treatment = c(0.4, 0.4, 0.42, 0.42),
    sd_pre_treatment    = c(0.2, 0.2, NA, NA),
    sd_post_treatment   = c(0.1, 0.1, 0.2, 0.2),
    rho                 = c(0.5, 0.5, NA, NA),
    stringsAsFactors    = FALSE
  )
  if (!is.null(randomisation)) d$randomisation <- randomisation
  extra <- list(...)
  for (nm in names(extra)) d[[nm]] <- extra[[nm]]
  d
}

prep <- function(d, baseline_imbalance = "by_randomisation", cluster_deff_default = 2) {
  prepare_stan_data(
    d, NULL,
    model_flags = list(is_baseline_normalised = 0L),
    priors = set_priors(),
    baseline_imbalance = baseline_imbalance,
    cluster_deff_default = cluster_deff_default
  )
}

# ---------------------------------------------------------------------------
# gamma_mode assignment
# ---------------------------------------------------------------------------

test_that("absent randomisation column places every study in the non-randomised branch", {
  sd_ <- prep(mk_summary())
  expect_equal(sd_$gamma_mode_did_summary, c(1L, 1L))
  expect_equal(sd_$gamma_mode_rct_summary, c(1L, 1L))
  expect_false(attr(sd_, "has_kappa_anchor"))
  expect_equal(attr(sd_, "n_randomised"), 0L)
})

test_that("NA randomisation reads as 'none', never as randomised", {
  sd_ <- prep(mk_summary(randomisation = c(NA, "none", NA, "individual")))
  expect_equal(sd_$gamma_mode_did_summary, c(1L, 1L))
  expect_equal(sd_$gamma_mode_rct_summary, c(1L, 2L))
})

test_that("randomisation is honoured independently of design", {
  # A randomised DiD and an unrandomised post-only study: the combination the
  # design label alone cannot express.
  sd_ <- prep(mk_summary(randomisation = c("individual", "none", "none", "cluster")))
  expect_equal(sd_$gamma_mode_did_summary, c(2L, 1L))
  expect_equal(sd_$gamma_mode_rct_summary, c(1L, 2L))
  expect_true(attr(sd_, "has_kappa_anchor"))   # anchored by the randomised DiD
  expect_equal(attr(sd_, "n_randomised"), 2L)
})

test_that("baseline_imbalance='estimated' ignores the randomisation column", {
  sd_ <- prep(mk_summary(randomisation = "individual"), baseline_imbalance = "estimated")
  expect_equal(sd_$gamma_mode_did_summary, c(1L, 1L))
  expect_equal(sd_$gamma_mode_rct_summary, c(1L, 1L))
  expect_equal(attr(sd_, "n_randomised"), 0L)
})

test_that("baseline_imbalance='fixed_zero' zeroes RCTs only, as before", {
  sd_ <- prep(mk_summary(), baseline_imbalance = "fixed_zero")
  expect_equal(sd_$gamma_mode_did_summary, c(1L, 1L))
  expect_equal(sd_$gamma_mode_rct_summary, c(0L, 0L))
})

test_that("post-only randomised studies alone do not anchor kappa", {
  sd_ <- prep(mk_summary(randomisation = c("none", "none", "individual", "individual")))
  expect_equal(attr(sd_, "n_randomised"), 2L)
  expect_false(attr(sd_, "has_kappa_anchor"))
})

# ---------------------------------------------------------------------------
# gamma_scale
# ---------------------------------------------------------------------------

test_that("gamma_scale is the sampling SD of the baseline contrast", {
  sd_ <- prep(mk_summary(randomisation = "individual"))
  # DiD uses PRE-treatment SDs: sqrt(0.2^2/100 + 0.1^2/100)
  expect_equal(sd_$gamma_scale_did_summary,
               rep(sqrt(0.2^2 / 100 + 0.1^2 / 100), 2))
  # Post-only RCTs have no pre SDs, so post SDs stand in.
  expect_equal(sd_$gamma_scale_rct_summary,
               rep(sqrt(0.2^2 / 100 + 0.1^2 / 100), 2))
})

test_that("cluster randomisation inflates gamma_scale by sqrt(DEFF)", {
  d <- mk_summary(randomisation = c("none", "none", "cluster", "cluster"),
                  cluster_size = c(NA, NA, 51, 51),
                  icc          = c(NA, NA, 0.02, 0.02))
  sd_ <- prep(d)
  base <- sqrt(0.2^2 / 100 + 0.1^2 / 100)
  deff <- 1 + (51 - 1) * 0.02   # = 2
  expect_equal(sd_$gamma_scale_rct_summary, rep(sqrt(deff) * base, 2))
  # Non-randomised studies are untouched (their mode never consults the scale).
  expect_equal(sd_$gamma_scale_did_summary, rep(base, 2))
})

test_that("cluster studies without m/ICC fall back to cluster_deff_default", {
  d <- mk_summary(randomisation = c("none", "none", "cluster", "cluster"))
  sd_ <- prep(d, cluster_deff_default = 4)
  base <- sqrt(0.2^2 / 100 + 0.1^2 / 100)
  expect_equal(sd_$gamma_scale_rct_summary, rep(2 * base, 2))
})

test_that("gamma_scale is computed on the normalised scale", {
  # normalise_by_baseline divides means and SDs by the study's own baseline, so
  # s_i must be computed after that or it would be on the wrong scale entirely.
  d  <- mk_summary(randomisation = "individual")
  nd <- normalise_summary(d, NULL)$summary_data
  raw  <- prep(d)$gamma_scale_did_summary
  norm <- prep(nd)$gamma_scale_did_summary
  expect_equal(norm, raw / 0.5)   # each DiD study's mean_pre_control is 0.5
})

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

test_that("validate_randomisation rejects unrecognised values", {
  expect_error(
    validate_randomisation(mk_summary(randomisation = "randomized"), NULL),
    "unrecognised values"
  )
})

test_that("validate_randomisation accepts an absent column and NA entries", {
  expect_silent(validate_randomisation(mk_summary(), NULL))
  expect_silent(validate_randomisation(mk_summary(randomisation = NA_character_), NULL))
})

test_that("validate_randomisation bounds the cluster companions", {
  expect_error(
    validate_randomisation(mk_summary(randomisation = "cluster", icc = 1.2), NULL),
    "must lie in \\[0, 1\\)"
  )
  expect_error(
    validate_randomisation(mk_summary(randomisation = "cluster", cluster_size = 0), NULL),
    "must be >= 1"
  )
})

test_that("randomisation must be constant within study for individual data", {
  ind <- data.frame(
    study_id      = rep("s1", 4),
    design        = "did",
    group         = rep(c("control", "treatment"), each = 2),
    time          = rep(c("pre", "post"), 2),
    value         = c(1, 2, 3, 4),
    subject_id    = rep(c("a", "b"), each = 2),
    randomisation = c("individual", "individual", "none", "none"),
    stringsAsFactors = FALSE
  )
  expect_error(validate_randomisation(NULL, ind), "must be constant within study")
})

test_that("meta_did rejects malformed kappa before doing any work", {
  expect_error(meta_did(summary_data = mk_summary(), kappa = -1),
               "non-negative number")
  expect_error(meta_did(summary_data = mk_summary(), kappa = c(1, 2)),
               "non-negative number")
  expect_error(meta_did(summary_data = mk_summary(), cluster_deff_default = 0.5),
               "must be a single number >= 1")
})

# ---------------------------------------------------------------------------
# Individual-level data: mode only, scale built in Stan
# ---------------------------------------------------------------------------

test_that("individual-level studies carry a mode but no precomputed scale", {
  ind <- do.call(rbind, lapply(c("s1", "s2"), function(sid) {
    data.frame(
      study_id   = sid,
      design     = "did",
      group      = rep(c("control", "treatment"), each = 20),
      time       = rep(rep(c("pre", "post"), each = 10), 2),
      value      = rnorm(40, 0.5, 0.1),
      subject_id = paste0(sid, "_", rep(rep(1:10, 2), 2)),
      randomisation = if (sid == "s1") "individual" else "none",
      stringsAsFactors = FALSE
    )
  }))
  sd_ <- prepare_stan_data(NULL, ind,
                           model_flags = list(is_baseline_normalised = 0L),
                           priors = set_priors())
  expect_equal(sd_$gamma_mode_did, c(2L, 1L))       # ordered by sorted study_id
  # No precomputed scale for individual data -- Stan builds s_i from the sampled
  # observation SDs. Exact lookup: `$` would partial-match gamma_scale_did_summary.
  expect_false("gamma_scale_did" %in% names(sd_))
  expect_true(attr(sd_, "has_kappa_anchor"))
})

# ---------------------------------------------------------------------------
# allow_unidentified_kappa: the argument must actually reach the core
# ---------------------------------------------------------------------------
# Every public entry point declares this argument, but they reach
# .meta_did_core() through different call sites. An argument that is accepted
# and then silently dropped would leave the guard permanently armed, so assert
# the plumbing rather than trusting it.

test_that("every entry point declares and forwards allow_unidentified_kappa", {
  for (fn in list(meta_did, meta_did_general, meta_did_naive,
                  metadid:::.meta_did_core)) {
    expect_true("allow_unidentified_kappa" %in% names(formals(fn)))
  }
  # Each wrapper must pass it on, not just accept it.
  for (fn in list(meta_did, meta_did_general, meta_did_naive)) {
    body_src <- paste(deparse(body(fn)), collapse = " ")
    expect_match(body_src, "allow_unidentified_kappa\\s*=\\s*allow_unidentified_kappa")
  }
})

test_that("allow_unidentified_kappa gates the unanchored-kappa refusal", {
  d <- mk_summary(randomisation = c("none", "none", "individual", "individual"))
  # No randomised study carries pre-treatment data, so kappa has no anchor.
  expect_error(meta_did(summary_data = d, kappa = "estimate"),
               "no randomised study carries pre-treatment data")
  # The error should point at both escape routes.
  err <- tryCatch(meta_did(summary_data = d, kappa = "estimate"),
                  error = conditionMessage)
  expect_match(err, "allow_unidentified_kappa = TRUE")

  # With the override the refusal is lifted; it must announce that kappa is
  # prior-driven rather than estimated.
  expect_message(
    try(meta_did(summary_data = d, kappa = "estimate",
                 allow_unidentified_kappa = TRUE,
                 chains = 1, iter_warmup = 50, iter_sampling = 50,
                 refresh = 0, show_messages = FALSE), silent = TRUE),
    "reproduce the prior"
  )
})

test_that("allow_unidentified_kappa is irrelevant when kappa is anchored", {
  # A randomised DiD identifies kappa, so the guard never fires and the
  # override changes nothing.
  d <- mk_summary(randomisation = c("individual", "none", "none", "none"))
  sd_ <- prep(d)
  expect_true(attr(sd_, "has_kappa_anchor"))
})
