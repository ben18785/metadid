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
  normalise = TRUE,
  baseline_latent_arm = c("treatment", "control"),
  robust_heterogeneity = FALSE,
  design_effects = FALSE,
  hierarchical_rho = TRUE,
  covariates = NULL,
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

- normalise:

  Logical. If `TRUE` (default), population-level effects
  (`treatment_effect_mean`, `time_trend_mean`,
  `baseline_difference_mean`) are expressed as fractions of the
  treatment-arm pre-treatment baseline. Stan receives the raw data
  unchanged and performs the normalisation internally via per-study
  latent baseline parameters. If `FALSE`, no per-study latent baseline
  is fit; population-level effects are pooled on the **absolute**
  (user-units) scale instead. Equivalent to the legacy
  `normalise_by_baseline = FALSE` behaviour.

- baseline_latent_arm:

  Character. Advanced. Only used when `normalise = TRUE`. Determines
  which arm's pre-treatment baseline is the per-study latent parameter
  (the one with the wide data-vague uniform prior); the other arm's
  baseline is derived via the hierarchical baseline-difference
  parameter. One of:

  - `"treatment"` (default): the treatment-arm pre-baseline is the
    latent, informed directly by `mean_pre_treatment` observations.

  - `"control"`: the control-arm pre-baseline is the latent, informed
    directly by `mean_pre_control` observations.

  The choice does **not** change the canonical scale on which effects
  are reported — both options pool population-level effects on the
  treatment-arm pre-baseline scale. It only controls which arm's data
  most directly informs the per-study baseline's posterior, which can
  matter when one arm has substantially more direct data than the other.
  The two options are statistically equivalent in well-identified
  problems.

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
