#' Fit a Bayesian meta-analysis model across study designs
#'
#' Fits a hierarchical Bayesian model synthesising treatment effects from
#' studies with different designs: difference-in-differences (DiD), randomised
#' controlled trials (RCT), and pre-post studies. All designs contribute to a
#' shared population treatment effect.
#'
#' DiD studies are required for identification of the treatment effect. Without
#' them, the treatment effect is confounded with time trends (pre-post) or
#' baseline group differences (RCT). See `vignette("model-details")` for a
#' full discussion of the model, normalisation, and identification.
#'
#' @param summary_data A data frame with one row per study containing
#'   summary statistics. Must include columns `study_id` and `design`. See
#'   [validate_summary_data()] for the full column specification per design.
#'   Valid designs: `"did"`, `"did_change"`, `"rct"`, `"pp"`.
#' @param individual_data A data frame in long format with one row per
#'   observation. Must include columns `study_id`, `design`, `group`,
#'   `time`, and `value`. Valid designs: `"did"`, `"rct"`, `"pp"`.
#'   No `study_id` may appear in both `summary_data` and `individual_data`.
#' @param normalise_by_baseline Logical. If `TRUE` (default), all means and
#'   SDs are divided by each study's pre-treatment control mean (or the
#'   grand mean for change-only studies), placing outcomes on a common
#'   fractional scale. The reported `treatment_effect_mean` is then the
#'   population mean of the per-study proportional effects,
#'   \eqn{E[\theta_i / b_i]} (a percentage-scale effect, each study expressed
#'   as a fraction of its own baseline), which is the appropriate estimand
#'   when studies are on heterogeneous scales. Note this is **not**
#'   \eqn{E[\theta] / E[b]}: when baselines vary across studies the two differ
#'   by the between-study baseline coefficient of variation squared (Jensen's
#'   inequality).
#' @param robust_heterogeneity Logical. If `TRUE`, study-level treatment
#'   effects are drawn from a Student-t distribution rather than a normal,
#'   providing robustness to outlier studies. The degrees-of-freedom
#'   parameter is estimated with the prior specified in `priors$nu`.
#' @param design_effects Logical. If `TRUE`, additive offsets on the
#'   population treatment effect mean are estimated for RCT and Pre-Post
#'   studies relative to DiD (the reference). Useful for testing whether
#'   designs yield systematically different effect estimates.
#' @param hierarchical_rho Logical. If `TRUE` (default), the pre-post
#'   correlation is modelled hierarchically across studies. Studies with a
#'   reported correlation inform the population distribution; studies without
#'   one have their correlation imputed.
#' @param correlated_effects Logical. If `TRUE`, study-level treatment
#'   effects and time trends are drawn jointly from a bivariate normal
#'   with a shared correlation parameter, rather than independently. The
#'   correlation is parameterised via a Cholesky factor of a 2×2 correlation
#'   matrix with an LKJ prior (see [set_priors()]). Cannot be combined with
#'   `robust_heterogeneity = TRUE`. Default `FALSE`.
#' @param baseline_imbalance How the per-study baseline difference
#'   \eqn{\gamma_i} is modelled. One of:
#'   \describe{
#'     \item{`"by_randomisation"`}{(Default) Each study's \eqn{\gamma_i}
#'       follows from its `randomisation` column. **Non-randomised** studies
#'       draw \eqn{\gamma_i \sim N(\mu_\gamma, \tau_\gamma)};
#'       **randomised** studies draw
#'       \eqn{\gamma_i \sim N(0, \kappa^2 s_i^2)}, where \eqn{s_i} is the
#'       sampling SD of that study's baseline contrast. The two populations do
#'       not share a mean: randomisation implies a zero population imbalance
#'       structurally, so it is imposed rather than estimated.}
#'     \item{`"estimated"`}{Every DiD and RCT study draws
#'       \eqn{\gamma_i \sim N(\mu_\gamma, \tau_\gamma)}, ignoring the
#'       `randomisation` column entirely. This was the previous default.}
#'     \item{`"fixed_zero"`}{Fix \eqn{\gamma_i = 0} for RCT studies; DiD
#'       studies are unaffected and still estimate \eqn{\gamma_i} from their
#'       own pre-treatment means. Note this is a *hard* constraint, which costs
#'       a DiD study its robustness to baseline imbalance: with
#'       \eqn{\gamma_i} pinned at zero the model averages the pre- and
#'       post-period information instead of differencing it, so a real
#'       imbalance leaks into \eqn{\theta_i}. Prefer `"by_randomisation"`
#'       with a small `kappa`, which shrinks toward zero without imposing it.}
#'   }
#'
#'   Baseline imbalance is **unidentified** for post-only studies: it subtracts
#'   directly from their estimated effect, so whatever the model assumes about
#'   \eqn{\gamma} propagates straight into the pooled treatment effect for
#'   those studies. The default exists to keep that propagation honest.
#' @param mu_gamma Whether the population mean baseline imbalance among
#'   **non-randomised** studies is estimated. One of:
#'   \describe{
#'     \item{`"zero"`}{(Default) \eqn{\mu_\gamma = 0}. The *magnitude* of
#'       selection-driven imbalance is pooled across non-randomised studies,
#'       but its *direction* is not transported between them.}
#'     \item{`"estimated"`}{\eqn{\mu_\gamma} is estimated, with the prior
#'       from `baseline_difference_mean` in [set_priors()].}
#'   }
#'
#'   The default is deliberate. Whether a treated group starts above or below
#'   its control is a property of each programme's targeting rule -- some
#'   interventions go to high-need (high-baseline) populations, others to
#'   easy-to-reach (low-baseline) ones -- not of the outcome or the
#'   intervention. Estimating a single \eqn{\mu_\gamma} asserts that a whole
#'   literature shares a direction of selection, and then applies that
#'   direction to every post-only study, whose own data cannot contradict it.
#'   Worse, the resulting bias does not shrink with more data: it *sharpens*,
#'   because \eqn{\mu_\gamma} is estimated more precisely. Set to
#'   `"estimated"` only when the studies plausibly share a targeting
#'   mechanism -- for example, several evaluations of the same programme.
#' @param kappa Excess-imbalance factor for **randomised** studies. Either a
#'   single non-negative number (default `0.5`) or the string `"estimate"`.
#'
#'   \eqn{\kappa^2} is the variance inflation of a randomised study's baseline
#'   contrast beyond simple random sampling, so \eqn{\kappa^2 = }`DEFF`\eqn{ - 1}
#'   and \eqn{\kappa = 0} is perfect randomisation. Note \eqn{\kappa = 0} is
#'   *not* the same as ignoring finite-sample imbalance: the realised imbalance
#'   of a randomised allocation is already carried by the likelihood's
#'   \eqn{\sigma^2/n} terms (and, for DiD, propagated into the post period by
#'   the pre-post correlation). \eqn{\kappa} governs only the *extra*
#'   allowance for randomisation not having held exactly -- imperfect
#'   allocation, attrition, post-randomisation selection.
#'
#'   Because \eqn{s_i} scales as \eqn{1/\sqrt{n_i}}, this automatically
#'   down-weights small randomised studies more than large ones. For a
#'   post-only randomised study, where \eqn{\gamma_i} is unidentified, the
#'   mechanism is equivalent to inflating that study's standard error by
#'   \eqn{\sqrt{1 + \kappa^2}}.
#'
#'   `"estimate"` samples \eqn{\kappa} with the prior from `kappa` in
#'   [set_priors()], but by default is only permitted when at least one
#'   randomised study carries pre-treatment data (a randomised DiD). Post-only
#'   randomised studies constrain \eqn{\kappa} not at all. With no such anchor,
#'   either fix \eqn{\kappa} or set `allow_unidentified_kappa = TRUE`. The
#'   default of `0.5` is a reasonable central choice rather than a value to
#'   trust on its own, so it is worth refitting at a few values to see whether
#'   any conclusion turns on it.
#' @param cluster_deff_default Design effect assumed for studies with
#'   `randomisation = "cluster"` that do not supply `cluster_size` and `icc`
#'   columns. Default `2`, which corresponds to roughly 50 units per cluster at
#'   an ICC of 0.02. When both columns are supplied, the study's own
#'   \eqn{1 + (m - 1)\rho_{ICC}} is used instead. The design effect inflates
#'   \eqn{s_i} so that \eqn{\kappa} keeps one meaning across designs.
#'
#'   This corrects the *baseline contrast* only. A cluster-randomised study's
#'   likelihood still uses \eqn{\sigma^2/n} for the post-treatment period, so
#'   it remains over-precise about its own treatment effect. Correcting that
#'   needs cluster identifiers and a random effect, which this model does not
#'   carry.
#' @param priors A `did_priors` object from [set_priors()]. Controls the
#'   prior distributions on all population-level parameters.
#' @param covariates An optional one-sided formula specifying study-level
#'   covariates for meta-regression on the treatment effect (e.g.,
#'   `~ dose + year`). The named columns must be numeric and present in
#'   both `summary_data` and `individual_data` (whichever are provided).
#'   For individual-level data, covariate values must be constant within
#'   each study. Default `NULL` (no meta-regression).
#' @param multiplicative_covariate Optional specification of one or two
#'   \emph{categorical} study-level covariates that modify the population
#'   treatment effect *multiplicatively* rather than additively. Either a
#'   single column name (character of length 1) for one covariate, or a
#'   one-sided formula naming one or two columns (`~ a` or `~ a + b`). At
#'   most two are allowed. One factor is estimated per non-reference level
#'   of each covariate: studies at a covariate's reference level keep their
#'   population-mean linear predictor
#'   \eqn{\mu_\theta + X_{\mathrm{cov},i}^{\top}\beta_{\mathrm{cov}}}
#'   unchanged (factor fixed at 1), while studies at level \eqn{k} have it
#'   multiplied by the estimated `effect_multiplier[k]`. With \strong{two}
#'   covariates the study's overall factor is the \emph{product} of the two
#'   per-covariate factors,
#'   \eqn{\alpha_{a(i)} \cdot \beta_{b(i)}} — i.e. each covariate scales the
#'   effect independently (a log-additive structure). The reference level is
#'   the first factor level (declare the column as a factor to control it,
#'   with identical levels declared in every data frame), the lowest value
#'   for numeric input, or the alphabetically first value for character
#'   input. A numeric `{0, 1}` indicator is the simplest case: 0 is the
#'   reference (factor fixed at 1) and 1 selects the single estimated
#'   multiplier. Useful when a study attribute attenuates or amplifies the
#'   underlying effect by a shared factor — e.g. how an intervention was
#'   delivered, optionally crossed with a second attribute such as how long it
#'   ran. Each column must contain no `NA`s, be constant within study for
#'   individual-level data, must not also appear in `covariates`, and must
#'   take at least two distinct values across studies for its multipliers to
#'   be identified; the two columns must be distinct. Numeric columns with
#'   more than 5 distinct values are rejected as likely continuous (convert
#'   genuinely categorical numeric codes to a factor). The same `multiplier`
#'   prior from [set_priors()] is applied independently to every estimated
#'   factor. On the returned object, `fit$multiplicative_covariate` is a list
#'   with elements `name` and `levels` (reference first) for one covariate,
#'   or a list of two such descriptors for two covariates. Default `NULL`
#'   (no multiplicative structure).
#' @param center_covariates Logical. If `TRUE` (default), covariates are
#'   mean-centered across all studies before fitting. This ensures that
#'   `treatment_effect_mean` is the population treatment effect at the
#'   average covariate values. Set to `FALSE` to use raw covariate values,
#'   in which case `treatment_effect_mean` is the effect when all covariates
#'   equal zero. The covariate coefficients (`beta_cov`) have the same
#'   interpretation regardless of centering: the change in expected
#'   treatment effect per unit increase in the covariate.
#' @param method Inference method. `"sample"` (default) runs full MCMC via
#'   Stan's HMC-NUTS sampler and returns a posterior distribution. `"optimize"`
#'   finds the maximum a posteriori (MAP) estimate via L-BFGS and is
#'   substantially faster, but returns only a point estimate with no
#'   uncertainty quantification.
#' @param chains Number of MCMC chains. Ignored when `method = "optimize"`.
#'   Default `4`.
#' @param iter_warmup Number of warmup iterations per chain. Ignored when
#'   `method = "optimize"`. Default `1000`.
#' @param iter_sampling Number of sampling iterations per chain. Ignored when
#'   `method = "optimize"`. Default `1000`.
#' @param seed Integer random seed for reproducibility. Default `NULL`.
#' @param allow_unidentified_kappa Logical. If `FALSE` (default),
#'   `kappa = "estimate"` errors when no randomised study carries pre-treatment
#'   data, because \eqn{\kappa} is then not identified by anything. Set to
#'   `TRUE` to sample it anyway (the posterior for \eqn{\kappa} will reproduce
#'   its prior).
#'
#'   Doing so is a modelling choice rather than an estimate. Sampling
#'   \eqn{\kappa} instead of fixing it makes the marginal prior on
#'   \eqn{\gamma_i} a scale mixture of normals rather than a normal --
#'   simultaneously more peaked at zero and much heavier-tailed than any fixed
#'   \eqn{\kappa}. That is a better description of "most randomised trials
#'   achieved balance, occasionally one badly did not" than a single scale can
#'   give, and it propagates the uncertainty in \eqn{\kappa} into the pooled
#'   effect rather than conditioning on one value. What it does *not* do is
#'   learn \eqn{\kappa} from the data, which is why it must be asked for
#'   explicitly.
#' @param allow_no_did Logical. If `FALSE` (default), `meta_did()` will
#'   stop with an error when no DiD studies are present, because the
#'   treatment effect is not identified from the data without the
#'   double-difference structure. Set to `TRUE` to override this check
#'   if you understand the limitation (the posterior will be prior-driven).
#' @param ... Additional arguments passed to the underlying CmdStanModel
#'   method: `$sample()` when `method = "sample"` (e.g., `parallel_chains`,
#'   `adapt_delta`) or `$optimize()` when `method = "optimize"` (e.g.,
#'   `algorithm`, `iter`).
#'
#' @return A `meta_did_fit` object. See [print.meta_did_fit()] and
#'   [summary.meta_did_fit()] for extracting results. When
#'   `method = "optimize"`, the summary contains MAP point estimates only;
#'   `sd`, `lo`, and `hi` columns will be `NA`.
#'
#' @export
#'
#' @examples
#' if (instantiate::stan_cmdstan_exists()) {
#'   studies <- data.frame(
#'     study_id            = c("Smith 2020", "Jones 2021"),
#'     design              = c("did", "rct"),
#'     n_control           = c(50, 60),
#'     mean_pre_control    = c(0.45, NA),
#'     mean_post_control   = c(0.42, 0.48),
#'     sd_pre_control      = c(0.12, NA),
#'     sd_post_control     = c(0.11, 0.12),
#'     n_treatment         = c(55, 65),
#'     mean_pre_treatment  = c(0.46, NA),
#'     mean_post_treatment = c(0.30, 0.35),
#'     sd_pre_treatment    = c(0.13, NA),
#'     sd_post_treatment   = c(0.10, 0.11),
#'     rho                 = c(0.75, NA)
#'   )
#'   fit <- meta_did(summary_data = studies)
#' }
meta_did <- function(
    summary_data             = NULL,
    individual_data          = NULL,
    normalise_by_baseline    = TRUE,
    robust_heterogeneity     = FALSE,
    design_effects           = FALSE,
    hierarchical_rho         = TRUE,
    correlated_effects       = FALSE,
    baseline_imbalance       = c("by_randomisation", "estimated", "fixed_zero"),
    mu_gamma                 = c("zero", "estimated"),
    kappa                    = 0.5,
    cluster_deff_default     = 2,
    covariates               = NULL,
    multiplicative_covariate = NULL,
    center_covariates        = TRUE,
    priors                   = set_priors(),
    method                   = c("sample", "optimize"),
    chains                   = 4L,
    iter_warmup              = 1000L,
    iter_sampling            = 1000L,
    seed                     = NULL,
    allow_no_did             = FALSE,
    allow_unidentified_kappa = FALSE,
    ...
) {
  .meta_did_core(
    summary_data             = summary_data,
    individual_data          = individual_data,
    normalise_by_baseline    = normalise_by_baseline,
    robust_heterogeneity     = robust_heterogeneity,
    design_effects           = design_effects,
    hierarchical_rho         = hierarchical_rho,
    correlated_effects       = correlated_effects,
    baseline_imbalance       = baseline_imbalance,
    mu_gamma                 = mu_gamma,
    kappa                    = kappa,
    cluster_deff_default     = cluster_deff_default,
    covariates               = covariates,
    multiplicative_covariate = multiplicative_covariate,
    center_covariates        = center_covariates,
    priors                   = priors,
    method                   = method,
    chains                   = chains,
    iter_warmup              = iter_warmup,
    iter_sampling            = iter_sampling,
    seed                     = seed,
    allow_no_did             = allow_no_did,
    allow_unidentified_kappa = allow_unidentified_kappa,
    ...
  )
}


#' Fit a meta-analysis model with explicit control over design assumptions
#'
#' A flexible interface for controlling how nuisance parameters are handled
#' for non-DiD study designs. By default, behaves identically to
#' [meta_did()]. Users can independently control whether time trends and
#' baseline imbalances are estimated or fixed to zero for RCT and pre-post
#' studies.
#'
#' DiD studies always estimate both time trends and baseline differences
#' regardless of these settings, since they provide the identifying
#' information for these parameters.
#'
#' @inheritParams meta_did
#' @param time_trend How to handle the time trend \eqn{\beta_i} for non-DiD
#'   studies. One of:
#'   \describe{
#'     \item{`"pooled"`}{(Default) Estimate study-level time trends with a
#'       hierarchical prior shared across designs. Information from DiD studies
#'       informs the RCT and pre-post time trends. This is the same behaviour
#'       as [meta_did()].}
#'     \item{`"fixed_zero"`}{Fix \eqn{\beta_i = 0} for RCT and pre-post
#'       studies. For pre-post studies, this means the pre-post change is
#'       attributed entirely to treatment. For RCT studies, the
#'       reparameterised time trend correction is bypassed.}
#'   }
#' @inheritParams meta_did
#' @param baseline_imbalance How the per-study baseline difference
#'   \eqn{\gamma_i} is modelled: `"by_randomisation"` (default),
#'   `"estimated"`, or `"fixed_zero"`. Identical in meaning and default to the
#'   [meta_did()] argument of the same name -- see there for the full
#'   description, and for `mu_gamma`, `kappa` and `cluster_deff_default`, which
#'   this function also accepts.
#' @param pp_likelihood Likelihood form for pre-post studies. One of:
#'   \describe{
#'     \item{`"differenced"`}{(Default) Use the differenced (post minus pre)
#'       likelihood. The pre/post correlation \eqn{\rho_i} is not separately
#'       estimable and the baseline cancels algebraically. This is the same
#'       behaviour as [meta_did()].}
#'     \item{`"bivariate"`}{Use the full bivariate normal likelihood for the
#'       (pre, post) pair. This retains the pre/post correlation \eqn{\rho_i}
#'       as an estimable parameter, contributing to the hierarchical
#'       \eqn{\rho} model, at the cost of estimating additional nuisance
#'       parameters.}
#'   }
#'
#' @return A `meta_did_fit` object, identical in structure to the return
#'   value of [meta_did()].
#'
#' @seealso [meta_did()] for the standard model, which is equivalent to
#'   `meta_did_general()` with default settings.
#' @export
#'
#' @examples
#' if (instantiate::stan_cmdstan_exists()) {
#'   studies <- data.frame(
#'     study_id            = c("Smith 2020", "Jones 2021"),
#'     design              = c("did", "rct"),
#'     n_control           = c(50, 60),
#'     mean_pre_control    = c(0.45, NA),
#'     mean_post_control   = c(0.42, 0.48),
#'     sd_pre_control      = c(0.12, NA),
#'     sd_post_control     = c(0.11, 0.12),
#'     n_treatment         = c(55, 65),
#'     mean_pre_treatment  = c(0.46, NA),
#'     mean_post_treatment = c(0.30, 0.35),
#'     sd_pre_treatment    = c(0.13, NA),
#'     sd_post_treatment   = c(0.10, 0.11),
#'     rho                 = c(0.75, NA)
#'   )
#'
#'   # Borrow time trends from DiD, but assume equal baselines for RCT
#'   fit <- meta_did_general(
#'     summary_data       = studies,
#'     baseline_imbalance = "fixed_zero"
#'   )
#' }
meta_did_general <- function(
    summary_data             = NULL,
    individual_data          = NULL,
    normalise_by_baseline    = TRUE,
    robust_heterogeneity     = FALSE,
    design_effects           = FALSE,
    hierarchical_rho         = TRUE,
    correlated_effects       = FALSE,
    baseline_imbalance       = c("by_randomisation", "estimated", "fixed_zero"),
    mu_gamma                 = c("zero", "estimated"),
    kappa                    = 0.5,
    cluster_deff_default     = 2,
    covariates               = NULL,
    multiplicative_covariate = NULL,
    center_covariates        = TRUE,
    priors                   = set_priors(),
    time_trend               = c("pooled", "fixed_zero"),
    pp_likelihood            = c("differenced", "bivariate"),
    method                   = c("sample", "optimize"),
    chains                   = 4L,
    iter_warmup              = 1000L,
    iter_sampling            = 1000L,
    seed                     = NULL,
    allow_no_did             = FALSE,
    allow_unidentified_kappa = FALSE,
    ...
) {
  time_trend         <- match.arg(time_trend)
  baseline_imbalance <- match.arg(baseline_imbalance)
  mu_gamma           <- match.arg(mu_gamma)
  pp_likelihood      <- match.arg(pp_likelihood)

  overrides <- list()

  if (time_trend == "fixed_zero") {
    overrides$is_time_trend_pp_zero             <- 1L
    overrides$is_time_trend_pp_summary_zero     <- 1L
    overrides$is_time_trend_rct_zero            <- 1L
    overrides$is_time_trend_rct_summary_zero    <- 1L
  }

  if (pp_likelihood == "bivariate") {
    overrides$is_differenced_likelihood_pp         <- 0L
    overrides$is_differenced_likelihood_pp_summary <- 0L
  }

  .meta_did_core(
    summary_data             = summary_data,
    individual_data          = individual_data,
    normalise_by_baseline    = normalise_by_baseline,
    robust_heterogeneity     = robust_heterogeneity,
    design_effects           = design_effects,
    hierarchical_rho         = hierarchical_rho,
    correlated_effects       = correlated_effects,
    baseline_imbalance       = baseline_imbalance,
    mu_gamma                 = mu_gamma,
    kappa                    = kappa,
    cluster_deff_default     = cluster_deff_default,
    covariates               = covariates,
    multiplicative_covariate = multiplicative_covariate,
    center_covariates        = center_covariates,
    priors                   = priors,
    method                   = method,
    chains                   = chains,
    iter_warmup              = iter_warmup,
    iter_sampling            = iter_sampling,
    seed                     = seed,
    allow_no_did             = allow_no_did,
    allow_unidentified_kappa = allow_unidentified_kappa,
    stan_data_overrides      = if (length(overrides) > 0) overrides else NULL,
    ...
  )
}


#' Fit a naive meta-analysis model (no borrowing across designs)
#'
#' @description
#' **Deprecated.** `meta_did_naive()` is deprecated in favour of [meta_did_general()] with
#' `time_trend = "fixed_zero"` and `baseline_imbalance = "fixed_zero"`,
#' which provides the same behaviour with more explicit control.
#'
#' This function imposes the "naive" assumptions typically made when
#' analysing each design in isolation:
#' \itemize{
#'   \item **RCT**: Baseline means are assumed equal across treatment and
#'     control groups (randomisation assumption), so the baseline difference
#'     \eqn{\gamma} is fixed to zero.
#'   \item **Pre-post**: The time trend \eqn{\beta} is fixed to zero, so the
#'     pre-post change is attributed entirely to the treatment effect.
#' }
#'
#' @inheritParams meta_did
#' @return A `meta_did_fit` object, identical in structure to the return
#'   value of [meta_did()].
#'
#' @seealso [meta_did_general()] for independent control over assumptions.
#' @export
#' @keywords internal
meta_did_naive <- function(
    summary_data             = NULL,
    individual_data          = NULL,
    normalise_by_baseline    = TRUE,
    robust_heterogeneity     = FALSE,
    design_effects           = FALSE,
    hierarchical_rho         = TRUE,
    covariates               = NULL,
    multiplicative_covariate = NULL,
    center_covariates        = TRUE,
    priors                   = set_priors(),
    method                   = c("sample", "optimize"),
    chains                   = 4L,
    iter_warmup              = 1000L,
    iter_sampling            = 1000L,
    seed                     = NULL,
    allow_no_did             = FALSE,
    allow_unidentified_kappa = FALSE,
    ...
) {
  .Deprecated("meta_did_general",
              msg = paste(
                "'meta_did_naive()' is deprecated.",
                "Use meta_did_general(time_trend = \"fixed_zero\",",
                "baseline_imbalance = \"fixed_zero\") instead."
              ))
  meta_did_general(
    summary_data             = summary_data,
    individual_data          = individual_data,
    normalise_by_baseline    = normalise_by_baseline,
    robust_heterogeneity     = robust_heterogeneity,
    design_effects           = design_effects,
    hierarchical_rho         = hierarchical_rho,
    covariates               = covariates,
    multiplicative_covariate = multiplicative_covariate,
    center_covariates        = center_covariates,
    priors                   = priors,
    time_trend               = "fixed_zero",
    baseline_imbalance       = "fixed_zero",
    mu_gamma                 = "estimated",
    method                   = method,
    chains                   = chains,
    iter_warmup              = iter_warmup,
    iter_sampling            = iter_sampling,
    seed                     = seed,
    allow_no_did             = allow_no_did,
    allow_unidentified_kappa = allow_unidentified_kappa,
    ...
  )
}


# ---------------------------------------------------------------------------
# Internal core implementation shared by meta_did() and meta_did_naive()
# ---------------------------------------------------------------------------

.meta_did_core <- function(
    summary_data             = NULL,
    individual_data          = NULL,
    normalise_by_baseline    = TRUE,
    robust_heterogeneity     = FALSE,
    design_effects           = FALSE,
    hierarchical_rho         = TRUE,
    correlated_effects       = FALSE,
    baseline_imbalance       = c("by_randomisation", "estimated", "fixed_zero"),
    mu_gamma                 = c("zero", "estimated"),
    kappa                    = 0.5,
    cluster_deff_default     = 2,
    covariates               = NULL,
    multiplicative_covariate = NULL,
    center_covariates        = TRUE,
    priors                   = set_priors(),
    method                   = c("sample", "optimize"),
    chains                   = 4L,
    iter_warmup              = 1000L,
    iter_sampling            = 1000L,
    seed                     = NULL,
    allow_no_did             = FALSE,
    allow_unidentified_kappa = FALSE,
    stan_data_overrides      = NULL,
    ...
) {
  method             <- match.arg(method)
  baseline_imbalance <- match.arg(baseline_imbalance)
  mu_gamma           <- match.arg(mu_gamma)

  # --- Input checks ---
  if (is.null(summary_data) && is.null(individual_data)) {
    stop("At least one of summary_data or individual_data must be provided.",
         call. = FALSE)
  }

  validate_summary_data(summary_data)
  validate_individual_data(individual_data)
  validate_randomisation(summary_data, individual_data)

  # kappa: a fixed non-negative scalar, or "estimate" to sample it.
  estimate_kappa <- identical(kappa, "estimate")
  if (!estimate_kappa) {
    if (!is.numeric(kappa) || length(kappa) != 1L || is.na(kappa) || kappa < 0) {
      stop("'kappa' must be a single non-negative number, or the string ",
           "\"estimate\".", call. = FALSE)
    }
  }
  if (!is.numeric(cluster_deff_default) || length(cluster_deff_default) != 1L ||
      cluster_deff_default < 1) {
    stop("'cluster_deff_default' must be a single number >= 1.", call. = FALSE)
  }

  # Check no study_id overlap between the two inputs
  if (!is.null(summary_data) && !is.null(individual_data)) {
    overlap <- intersect(summary_data$study_id, individual_data$study_id)
    if (length(overlap) > 0) {
      stop(
        "The same study_id appears in both summary_data and individual_data: ",
        paste(overlap, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  # --- DiD identification check ---
  n_did <- sum(c(
    if (!is.null(summary_data))    sum(summary_data$design %in% c("did", "did_change")),
    if (!is.null(individual_data)) sum(individual_data$design == "did")
  ))
  if (n_did == 0 && !allow_no_did) {
    stop(
      "No DiD studies found. Without DiD studies, the treatment effect is not ",
      "identified from the data \u2014 it is confounded with time trends (pre-post) ",
      "or baseline group differences (RCT), and the posterior will be driven by ",
      "the priors rather than the data.\n",
      "If you understand this limitation and wish to proceed, set allow_no_did = TRUE.",
      call. = FALSE
    )
  }

  # --- Parse covariates ---
  covariate_names <- NULL
  if (!is.null(covariates)) {
    if (!inherits(covariates, "formula")) {
      stop("'covariates' must be a one-sided formula (e.g., ~ dose + year).",
           call. = FALSE)
    }
    # Extract variable names from the formula (drop intercept)
    covariate_names <- all.vars(covariates)
    if (length(covariate_names) == 0) {
      stop("'covariates' formula contains no variables.", call. = FALSE)
    }
    validate_covariates(covariate_names, summary_data, individual_data)
  }

  # --- Validate multiplicative covariate(s): one or two categorical columns ---
  validate_multiplicative_covariate(multiplicative_covariate, covariate_names,
                                    summary_data, individual_data)

  # --- Normalisation ---
  normalisation_factors <- NULL
  if (normalise_by_baseline) {
    norm_result           <- normalise_summary(summary_data, individual_data)
    summary_data          <- norm_result$summary_data
    individual_data       <- norm_result$individual_data
    normalisation_factors <- norm_result$factors
  }

  # --- Validate rho when hierarchical modelling is off ---
  if (!hierarchical_rho && !is.null(summary_data) && nrow(summary_data) > 0) {
    rho_col <- if ("rho" %in% names(summary_data)) summary_data$rho else NULL
    designs_needing_rho <- summary_data$design %in% c("did", "pp")
    if (!is.null(rho_col) && any(is.na(rho_col[designs_needing_rho]))) {
      missing_ids <- summary_data$study_id[designs_needing_rho & is.na(rho_col)]
      stop(
        "hierarchical_rho = FALSE but the following summary-level studies have ",
        "missing rho: ", paste(missing_ids, collapse = ", "), ". ",
        "Without hierarchical modelling, missing correlations cannot be imputed. ",
        "Either provide rho for all studies or set hierarchical_rho = TRUE.",
        call. = FALSE
      )
    }
    if (is.null(rho_col) && any(designs_needing_rho)) {
      missing_ids <- summary_data$study_id[designs_needing_rho]
      stop(
        "hierarchical_rho = FALSE but no rho column is present in summary_data. ",
        "The following studies require rho: ",
        paste(missing_ids, collapse = ", "), ". ",
        "Either provide rho for all studies or set hierarchical_rho = TRUE.",
        call. = FALSE
      )
    }
  }

  # --- Validate rho when hierarchical modelling is on ---
  # The hierarchy imputes rho for summary-level studies that don't report it,
  # but only by borrowing strength from studies that DO carry correlation
  # information: summary studies with a reported rho, or individual-level
  # DiD/Pre-Post studies (whose rho is identified from the paired data). With
  # no such anchor the population rho parameters are informed only by the prior,
  # producing a funnel and non-convergence (see E6). Rho cannot be inferred from
  # nothing, so refuse rather than impute.
  if (hierarchical_rho && !is.null(summary_data) && nrow(summary_data) > 0) {
    designs_needing_rho <- summary_data$design %in% c("did", "pp")
    if (any(designs_needing_rho)) {
      rho_col <- if ("rho" %in% names(summary_data)) summary_data$rho
                 else rep(NA_real_, nrow(summary_data))
      n_summary_known   <- sum(designs_needing_rho & !is.na(rho_col))
      n_summary_missing <- sum(designs_needing_rho &  is.na(rho_col))
      has_individual_rho <- !is.null(individual_data) &&
        nrow(individual_data) > 0 &&
        any(individual_data$design %in% c("did", "pp"))
      if (n_summary_missing > 0 && n_summary_known == 0 && !has_individual_rho) {
        stop(
          "hierarchical_rho = TRUE but no study provides a pre-post correlation ",
          "to anchor the hierarchy: all ", n_summary_missing, " summary-level ",
          "DiD/Pre-Post studies have missing rho and there is no individual-level ",
          "DiD/Pre-Post data from which rho could be identified. Correlations ",
          "cannot be inferred from nothing. Provide rho for at least one study.",
          call. = FALSE
        )
      }
    }
  }

  # --- Validate correlated_effects ---
  if (correlated_effects && robust_heterogeneity) {
    stop(
      "correlated_effects = TRUE cannot be combined with robust_heterogeneity = TRUE. ",
      "The bivariate normal prior on (treatment_effect, time_trend) is not compatible ",
      "with Student-t heterogeneity. This restriction may be relaxed in a future version.",
      call. = FALSE
    )
  }

  # --- Model flags ---
  model_flags <- list(
    is_baseline_normalised                  = as.integer(normalise_by_baseline),
    is_correlation_coefficient_hierarchical = as.integer(hierarchical_rho),
    is_student_t_heterogeneity              = as.integer(robust_heterogeneity),
    is_design_effect                        = as.integer(design_effects),
    is_correlated_effects                   = as.integer(correlated_effects),
    is_mu_gamma_estimated                   = as.integer(mu_gamma == "estimated"),
    # Provisionally on when requested; downgraded below if nothing anchors it.
    is_kappa_estimated                      = as.integer(estimate_kappa),
    kappa_fixed                             = if (estimate_kappa) 0 else as.numeric(kappa)
  )

  # --- Stan data ---
  stan_data <- prepare_stan_data(summary_data, individual_data, model_flags, priors,
                                  covariate_names = covariate_names,
                                  multiplicative_covariate = multiplicative_covariate,
                                  center_covariates = center_covariates,
                                  baseline_imbalance = baseline_imbalance,
                                  cluster_deff_default = cluster_deff_default)

  # --- kappa identification check ---
  # kappa is the scale of the baseline imbalance permitted to randomised
  # studies. Only a randomised study with PRE-treatment data constrains it: a
  # post-only randomised study has an unidentified gamma_i, so it says nothing
  # about how large gamma tends to be. Sampling kappa without such an anchor
  # gives a prior-driven parameter that silently sets how much every randomised
  # study is down-weighted, so refuse -- mirroring the hierarchical-rho check
  # above, which declines to impute correlations from nothing.
  if (estimate_kappa && !isTRUE(attr(stan_data, "has_kappa_anchor"))) {
    if (!allow_unidentified_kappa) {
      stop(
        "kappa = \"estimate\" but no randomised study carries pre-treatment data ",
        "to identify it. kappa is anchored only by randomised DiD studies; ",
        "post-only randomised studies have an unidentified baseline imbalance and ",
        "constrain its scale not at all.\n",
        "Supply a fixed value instead (e.g. kappa = 0.5 for individual ",
        "randomisation, or kappa = 1 to allow more). Refitting at a few values ",
        "is usually cheap and shows whether any conclusion turns on the choice.\n",
        "Or set allow_unidentified_kappa = TRUE to sample kappa anyway, ",
        "integrating over its prior rather than conditioning on one value.",
        call. = FALSE
      )
    }
    if (!isTRUE(getOption("metadid.quiet", FALSE))) {
      message(
        "kappa is being sampled with nothing to identify it: no randomised ",
        "study carries pre-treatment data. Its posterior will reproduce the ",
        "prior from set_priors(kappa = ). This is a deliberate choice, not an ",
        "estimate -- the effect is to give each randomised study's baseline ",
        "imbalance a heavier-tailed prior than any fixed kappa can, rather ",
        "than to learn kappa from the data."
      )
    }
  }
  # Warn only where the assumption actually bites: a post-only study cannot
  # identify its own gamma, so whatever the model assumes about imbalance
  # subtracts straight from its estimated effect. A DiD-only analysis identifies
  # gamma per study from its own pre-treatment means, so the label changes
  # little and the message would be noise. Suppress with
  # options(metadid.quiet = TRUE) in batch pipelines.
  has_rand_col <- (!is.null(summary_data) && "randomisation" %in% names(summary_data)) ||
                  (!is.null(individual_data) && "randomisation" %in% names(individual_data))
  n_post_only <- sum(summary_data$design == "rct", na.rm = TRUE) +
                 sum(individual_data$design == "rct", na.rm = TRUE)
  if (!has_rand_col && n_post_only > 0 &&
      baseline_imbalance == "by_randomisation" &&
      !isTRUE(getOption("metadid.quiet", FALSE))) {
    message(
      "No 'randomisation' column supplied, so all ", n_post_only,
      " post-only (RCT) studies are treated as non-randomised and have their ",
      "baseline imbalance imputed from the other studies. That imputation ",
      "subtracts directly from their estimated treatment effect.\n",
      "Add a 'randomisation' column (\"individual\", \"cluster\" or \"none\") ",
      "so randomised studies use the zero-centred, sample-size-scaled model ",
      "instead. Silence with options(metadid.quiet = TRUE)."
    )
  }
  model_flags$is_kappa_estimated <- stan_data$is_kappa_estimated
  # Studies actually placed in the randomised gamma branch. Distinct from the
  # count of studies *labelled* randomised: baseline_imbalance = "estimated"
  # deliberately ignores the label, and printing the label there would overstate
  # what the fitted model assumed.
  model_flags$n_gamma_randomised <- attr(stan_data, "n_randomised")
  cov_centers <- attr(stan_data, "cov_centers")
  mult_covariates <- attr(stan_data, "mult_covariates")
  multiplicative_covariate_info <- if (is.null(mult_covariates)) {
    NULL
  } else if (length(mult_covariates) == 1L) {
    mult_covariates[[1]]
  } else {
    mult_covariates
  }

  # Apply any overrides (e.g. naive-mode flags)
  if (!is.null(stan_data_overrides)) {
    stan_data[names(stan_data_overrides)] <- stan_data_overrides
  }

  # --- Fit ---
  stan_dir <- system.file("bin/stan", package = "metadid")
  if (nzchar(stan_dir)) {
    # Installed package: load pre-compiled binary directly via cmdstanr.
    # We bypass instantiate::stan_package_model() because it does not
    # propagate include_paths to the CmdStanModel object, causing stanc
    # to fail when the model uses #include directives.
    model <- cmdstanr::cmdstan_model(
      stan_file     = file.path(stan_dir, "meta_analysis_master.stan"),
      exe_file      = file.path(stan_dir, "meta_analysis_master"),
      include_paths = stan_dir
    )
  } else {
    # Development (devtools::load_all()): compile directly from src/stan/
    pkg_root <- dirname(system.file(package = "metadid"))
    stan_dir <- file.path(pkg_root, "src", "stan")
    model <- cmdstanr::cmdstan_model(
      stan_file     = file.path(stan_dir, "meta_analysis_master.stan"),
      include_paths = stan_dir
    )
  }

  fit <- if (method == "sample") {
    model$sample(
      data          = stan_data,
      chains        = chains,
      iter_warmup   = iter_warmup,
      iter_sampling = iter_sampling,
      seed          = seed,
      ...
    )
  } else {
    model$optimize(
      data   = stan_data,
      seed   = seed,
      ...
    )
  }

  # --- Return ---
  new_meta_did_fit(
    fit                      = fit,
    summary_data             = summary_data,
    individual_data          = individual_data,
    model_flags              = model_flags,
    priors                   = priors,
    normalisation_factors    = normalisation_factors,
    method                   = method,
    covariate_names          = covariate_names,
    multiplicative_covariate = multiplicative_covariate_info,
    cov_centers              = cov_centers,
    center_covariates        = center_covariates
  )
}

# ---------------------------------------------------------------------------
# Normalisation helper
# ---------------------------------------------------------------------------

normalise_summary <- function(summary_data, individual_data) {
  factors <- list()

  if (!is.null(summary_data) && nrow(summary_data) > 0) {
    # DiD: normalise by each study's own mean_pre_control
    did_rows <- summary_data$design == "did"
    if (any(did_rows)) {
      norm <- summary_data$mean_pre_control[did_rows]
      mean_cols <- grep("^mean_", names(summary_data), value = TRUE)
      sd_cols   <- grep("^sd_",   names(summary_data), value = TRUE)
      summary_data[did_rows, mean_cols] <-
        sweep(summary_data[did_rows, mean_cols, drop = FALSE], 1, norm, "/")
      summary_data[did_rows, sd_cols] <-
        sweep(summary_data[did_rows, sd_cols,   drop = FALSE], 1, norm, "/")
      factors$did <- stats::setNames(norm, summary_data$study_id[did_rows])
    }

    # RCT: normalise by each study's mean_post_control (best available baseline)
    rct_rows <- summary_data$design == "rct"
    if (any(rct_rows)) {
      norm <- summary_data$mean_post_control[rct_rows]
      mean_cols <- grep("^mean_post", names(summary_data), value = TRUE)
      sd_cols   <- grep("^sd_post",  names(summary_data), value = TRUE)
      summary_data[rct_rows, mean_cols] <-
        sweep(summary_data[rct_rows, mean_cols, drop = FALSE], 1, norm, "/")
      summary_data[rct_rows, sd_cols] <-
        sweep(summary_data[rct_rows, sd_cols,   drop = FALSE], 1, norm, "/")
      factors$rct <- stats::setNames(norm, summary_data$study_id[rct_rows])
    }

    # PP: normalise by each study's mean_pre_treatment
    pp_rows <- summary_data$design == "pp"
    if (any(pp_rows)) {
      norm <- summary_data$mean_pre_treatment[pp_rows]
      mean_cols <- grep("^mean_", names(summary_data), value = TRUE)
      sd_cols   <- grep("^sd_",   names(summary_data), value = TRUE)
      summary_data[pp_rows, mean_cols] <-
        sweep(summary_data[pp_rows, mean_cols, drop = FALSE], 1, norm, "/")
      summary_data[pp_rows, sd_cols] <-
        sweep(summary_data[pp_rows, sd_cols,   drop = FALSE], 1, norm, "/")
      factors$pp <- stats::setNames(norm, summary_data$study_id[pp_rows])
    }

    # did_change: normalise by grand mean of mean_pre_control across did studies
    did_change_rows <- summary_data$design == "did_change"
    if (any(did_change_rows)) {
      grand_mean <- mean(factors$did, na.rm = TRUE)
      if (is.nan(grand_mean)) grand_mean <- 1
      mean_cols <- grep("^mean_change", names(summary_data), value = TRUE)
      sd_cols   <- grep("^sd_change",  names(summary_data), value = TRUE)
      summary_data[did_change_rows, mean_cols] <-
        summary_data[did_change_rows, mean_cols, drop = FALSE] / grand_mean
      summary_data[did_change_rows, sd_cols] <-
        summary_data[did_change_rows, sd_cols, drop = FALSE] / grand_mean
      factors$did_change <- grand_mean
    }
  }

  if (!is.null(individual_data) && nrow(individual_data) > 0) {
    individual_data <- individual_data |>
      dplyr::group_by(.data$study_id, .data$design) |>
      dplyr::mutate(
        value = .data$value / dplyr::case_when(
          # DiD: normalise by pre-treatment control mean
          .data$design == "did" ~ mean(
            .data$value[.data$group == "control" & .data$time == "pre"],
            na.rm = TRUE
          ),
          # RCT: normalise by post-treatment control mean (no pre data)
          .data$design == "rct" ~ mean(
            .data$value[.data$group == "control" & .data$time == "post"],
            na.rm = TRUE
          ),
          # Pre-post: normalise by pre-treatment treatment mean (no control)
          .data$design == "pp" ~ mean(
            .data$value[.data$group == "treatment" & .data$time == "pre"],
            na.rm = TRUE
          )
        )
      ) |>
      dplyr::ungroup()
  }

  list(
    summary_data    = summary_data,
    individual_data = individual_data,
    factors         = factors
  )
}
