# Fit a meta-analysis model with explicit control over design assumptions

A flexible interface for controlling how nuisance parameters are handled
for non-DiD study designs. By default, behaves identically to
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).
Users can independently control whether time trends and baseline
imbalances are estimated or fixed to zero for RCT and pre-post studies.

## Usage

``` r
meta_did_general(
  summary_data = NULL,
  individual_data = NULL,
  normalise_by_baseline = TRUE,
  robust_heterogeneity = FALSE,
  design_effects = FALSE,
  hierarchical_rho = TRUE,
  correlated_effects = FALSE,
  baseline_imbalance = c("estimated", "fixed_zero"),
  covariates = NULL,
  multiplicative_covariate = NULL,
  center_covariates = TRUE,
  priors = set_priors(),
  time_trend = c("pooled", "fixed_zero"),
  pp_likelihood = c("differenced", "bivariate"),
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

- correlated_effects:

  Logical. If `TRUE`, study-level treatment effects and time trends are
  drawn jointly from a bivariate normal with a shared correlation
  parameter, rather than independently. The correlation is parameterised
  via a Cholesky factor of a 2×2 correlation matrix with an LKJ prior
  (see
  [`set_priors()`](https://ben18785.github.io/metadid/reference/set_priors.md)).
  Cannot be combined with `robust_heterogeneity = TRUE`. Default
  `FALSE`.

- baseline_imbalance:

  How to handle the baseline difference \\\gamma_i\\ between treatment
  and control groups for **RCT** studies. DiD studies always estimate
  \\\gamma_i\\ per-study (the pre-treatment means identify it from
  data). One of:

  `"estimated"`

  :   (Default) Estimate per-study \\\gamma_i\\ for RCT, drawing from a
      hierarchical prior shared with DiD's per-study estimates. This is
      the same behaviour as
      [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).

  `"fixed_zero"`

  :   Fix \\\gamma_i = 0\\ for RCT studies, assuming randomisation
      eliminates baseline imbalances.

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

- time_trend:

  How to handle the time trend \\\beta_i\\ for non-DiD studies. One of:

  `"pooled"`

  :   (Default) Estimate study-level time trends with a hierarchical
      prior shared across designs. Information from DiD studies informs
      the RCT and pre-post time trends. This is the same behaviour as
      [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).

  `"fixed_zero"`

  :   Fix \\\beta_i = 0\\ for RCT and pre-post studies. For pre-post
      studies, this means the pre-post change is attributed entirely to
      treatment. For RCT studies, the reparameterised time trend
      correction is bypassed.

- pp_likelihood:

  Likelihood form for pre-post studies. One of:

  `"differenced"`

  :   (Default) Use the differenced (post minus pre) likelihood. The
      pre/post correlation \\\rho_i\\ is not separately estimable and
      the baseline cancels algebraically. This is the same behaviour as
      [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).

  `"bivariate"`

  :   Use the full bivariate normal likelihood for the (pre, post) pair.
      This retains the pre/post correlation \\\rho_i\\ as an estimable
      parameter, contributing to the hierarchical \\\rho\\ model, at the
      cost of estimating additional nuisance parameters.

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

## Details

DiD studies always estimate both time trends and baseline differences
regardless of these settings, since they provide the identifying
information for these parameters.

## See also

[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
for the standard model, which is equivalent to `meta_did_general()` with
default settings.

## Examples

``` r
if (instantiate::stan_cmdstan_exists()) {
  studies <- data.frame(
    study_id            = c("Smith 2020", "Jones 2021"),
    design              = c("did", "rct"),
    n_control           = c(50, 60),
    mean_pre_control    = c(0.45, NA),
    mean_post_control   = c(0.42, 0.48),
    sd_pre_control      = c(0.12, NA),
    sd_post_control     = c(0.11, 0.12),
    n_treatment         = c(55, 65),
    mean_pre_treatment  = c(0.46, NA),
    mean_post_treatment = c(0.30, 0.35),
    sd_pre_treatment    = c(0.13, NA),
    sd_post_treatment   = c(0.10, 0.11),
    rho                 = c(0.75, NA)
  )

  # Borrow time trends from DiD, but assume equal baselines for RCT
  fit <- meta_did_general(
    summary_data       = studies,
    baseline_imbalance = "fixed_zero"
  )
}
#> Running MCMC with 4 sequential chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 1 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 8, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 1 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 51, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 1 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 8, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 1 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 51, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 1 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 1 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 10.0 seconds.
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 2 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 2 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 2 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 2 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 2 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 10.8 seconds.
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: Exception: multi_normal_lpdf: Location parameter[1] is inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 3 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 8, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 3 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 51, column 2)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3 
#> Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 finished in 8.2 seconds.
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 8, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 51, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 8, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 51, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 62, column 8, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 53, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 8, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 51, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 8, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpGHA0kd/model-1b95527acd59.stan', line 51, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 11.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 10.0 seconds.
#> Total execution time: 40.2 seconds.
#> 
#> Warning: 662 of 4000 (17.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2971 of 4000 (74.0%) transitions hit the maximum treedepth limit of 10.
#> See https://mc-stan.org/misc/warnings for details.
```
