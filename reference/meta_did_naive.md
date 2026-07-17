# Fit a naive meta-analysis model (no borrowing across designs)

**Deprecated.** `meta_did_naive()` is deprecated in favour of
[`meta_did_general()`](https://ben18785.github.io/metadid/reference/meta_did_general.md)
with `time_trend = "fixed_zero"` and
`baseline_imbalance = "fixed_zero"`, which provides the same behaviour
with more explicit control.

This function imposes the "naive" assumptions typically made when
analysing each design in isolation:

- **RCT**: Baseline means are assumed equal across treatment and control
  groups (randomisation assumption), so the baseline difference
  \\\gamma\\ is fixed to zero.

- **Pre-post**: The time trend \\\beta\\ is fixed to zero, so the
  pre-post change is attributed entirely to the treatment effect.

## Usage

``` r
meta_did_naive(
  summary_data = NULL,
  individual_data = NULL,
  normalise_by_baseline = TRUE,
  robust_heterogeneity = FALSE,
  design_effects = FALSE,
  hierarchical_rho = TRUE,
  covariates = NULL,
  multiplicative_covariate = NULL,
  center_covariates = TRUE,
  priors = set_priors(),
  method = c("sample", "optimize"),
  chains = 4L,
  iter_warmup = 1000L,
  iter_sampling = 1000L,
  seed = NULL,
  allow_no_did = FALSE,
  ...
)
```

## Arguments

- summary_data:

  A data frame with one row per study containing summary statistics.
  Must include columns `study_id` and `design`. See
  [`validate_summary_data()`](https://ben18785.github.io/metadid/reference/validate_summary_data.md)
  for the full column specification per design. Valid designs: `"did"`,
  `"did_change"`, `"rct"`, `"pp"`.

- individual_data:

  A data frame in long format with one row per observation. Must include
  columns `study_id`, `design`, `group`, `time`, and `value`. Valid
  designs: `"did"`, `"rct"`, `"pp"`. No `study_id` may appear in both
  `summary_data` and `individual_data`.

- normalise_by_baseline:

  Logical. If `TRUE` (default), all means and SDs are divided by each
  study's pre-treatment control mean (or the grand mean for change-only
  studies), placing outcomes on a common fractional scale. The reported
  `treatment_effect_mean` is then the population mean of the per-study
  proportional effects, \\E\[\theta_i / b_i\]\\ (a percentage-scale
  effect, each study expressed as a fraction of its own baseline), which
  is the appropriate estimand when studies are on heterogeneous scales.
  Note this is **not** \\E\[\theta\] / E\[b\]\\: when baselines vary
  across studies the two differ by the between-study baseline
  coefficient of variation squared (Jensen's inequality).

- robust_heterogeneity:

  Logical. If `TRUE`, study-level treatment effects are drawn from a
  Student-t distribution rather than a normal, providing robustness to
  outlier studies. The degrees-of-freedom parameter is estimated with
  the prior specified in `priors$nu`.

- design_effects:

  Logical. If `TRUE`, additive offsets on the population treatment
  effect mean are estimated for RCT and Pre-Post studies relative to DiD
  (the reference). Useful for testing whether designs yield
  systematically different effect estimates.

- hierarchical_rho:

  Logical. If `TRUE` (default), the pre-post correlation is modelled
  hierarchically across studies. Studies with a reported correlation
  inform the population distribution; studies without one have their
  correlation imputed.

- covariates:

  An optional one-sided formula specifying study-level covariates for
  meta-regression on the treatment effect (e.g., `~ dose + year`). The
  named columns must be numeric and present in both `summary_data` and
  `individual_data` (whichever are provided). For individual-level data,
  covariate values must be constant within each study. Default `NULL`
  (no meta-regression).

- multiplicative_covariate:

  Optional specification of one or two *categorical* study-level
  covariates that modify the population treatment effect
  *multiplicatively* rather than additively. Either a single column name
  (character of length 1) for one covariate, or a one-sided formula
  naming one or two columns (`~ a` or `~ a + b`). At most two are
  allowed. One factor is estimated per non-reference level of each
  covariate: studies at a covariate's reference level keep their
  population-mean linear predictor \\\mu\_\theta +
  X\_{\mathrm{cov},i}^{\top}\beta\_{\mathrm{cov}}\\ unchanged (factor
  fixed at 1), while studies at level \\k\\ have it multiplied by the
  estimated `effect_multiplier[k]`. With **two** covariates the study's
  overall factor is the *product* of the two per-covariate factors,
  \\\alpha\_{a(i)} \cdot \beta\_{b(i)}\\ — i.e. each covariate scales
  the effect independently (a log-additive structure). The reference
  level is the first factor level (declare the column as a factor to
  control it, with identical levels declared in every data frame), the
  lowest value for numeric input, or the alphabetically first value for
  character input. A numeric `{0, 1}` indicator is the simplest case: 0
  is the reference (factor fixed at 1) and 1 selects the single
  estimated multiplier. Useful when a study attribute attenuates or
  amplifies the underlying effect by a shared factor — e.g. how an
  intervention was delivered, optionally crossed with a second attribute
  such as how long it ran. Each column must contain no `NA`s, be
  constant within study for individual-level data, must not also appear
  in `covariates`, and must take at least two distinct values across
  studies for its multipliers to be identified; the two columns must be
  distinct. Numeric columns with more than 5 distinct values are
  rejected as likely continuous (convert genuinely categorical numeric
  codes to a factor). The same `multiplier` prior from
  [`set_priors()`](https://ben18785.github.io/metadid/reference/set_priors.md)
  is applied independently to every estimated factor. On the returned
  object, `fit$multiplicative_covariate` is a list with elements `name`
  and `levels` (reference first) for one covariate, or a list of two
  such descriptors for two covariates. Default `NULL` (no multiplicative
  structure).

- center_covariates:

  Logical. If `TRUE` (default), covariates are mean-centered across all
  studies before fitting. This ensures that `treatment_effect_mean` is
  the population treatment effect at the average covariate values. Set
  to `FALSE` to use raw covariate values, in which case
  `treatment_effect_mean` is the effect when all covariates equal zero.
  The covariate coefficients (`beta_cov`) have the same interpretation
  regardless of centering: the change in expected treatment effect per
  unit increase in the covariate.

- priors:

  A `did_priors` object from
  [`set_priors()`](https://ben18785.github.io/metadid/reference/set_priors.md).
  Controls the prior distributions on all population-level parameters.

- method:

  Inference method. `"sample"` (default) runs full MCMC via Stan's
  HMC-NUTS sampler and returns a posterior distribution. `"optimize"`
  finds the maximum a posteriori (MAP) estimate via L-BFGS and is
  substantially faster, but returns only a point estimate with no
  uncertainty quantification.

- chains:

  Number of MCMC chains. Ignored when `method = "optimize"`. Default
  `4`.

- iter_warmup:

  Number of warmup iterations per chain. Ignored when
  `method = "optimize"`. Default `1000`.

- iter_sampling:

  Number of sampling iterations per chain. Ignored when
  `method = "optimize"`. Default `1000`.

- seed:

  Integer random seed for reproducibility. Default `NULL`.

- allow_no_did:

  Logical. If `FALSE` (default),
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  will stop with an error when no DiD studies are present, because the
  treatment effect is not identified from the data without the
  double-difference structure. Set to `TRUE` to override this check if
  you understand the limitation (the posterior will be prior-driven).

- ...:

  Additional arguments passed to the underlying CmdStanModel method:
  `$sample()` when `method = "sample"` (e.g., `parallel_chains`,
  `adapt_delta`) or `$optimize()` when `method = "optimize"` (e.g.,
  `algorithm`, `iter`).

## Value

A `meta_did_fit` object, identical in structure to the return value of
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).

## See also

[`meta_did_general()`](https://ben18785.github.io/metadid/reference/meta_did_general.md)
for independent control over assumptions.
