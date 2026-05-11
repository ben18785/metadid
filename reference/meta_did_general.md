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
  covariates = NULL,
  center_covariates = TRUE,
  priors = set_priors(),
  time_trend = c("pooled", "fixed_zero"),
  baseline_imbalance = c("estimated", "fixed_zero"),
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
  studies), placing outcomes on a common fractional scale.

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

- baseline_imbalance:

  How to handle the baseline difference \\\gamma_i\\ between treatment
  and control groups for RCT studies. One of:

  `"estimated"`

  :   (Default) Estimate \\\gamma_i\\, borrowing information from DiD
      studies when baseline-normalised. This is the same behaviour as
      [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).

  `"fixed_zero"`

  :   Fix \\\gamma_i = 0\\, assuming randomisation eliminates baseline
      imbalances. This is the standard RCT assumption.

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
#> Chain 1 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 26, column 6, included from
#> Chain 1 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 50, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 1 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 26, column 6, included from
#> Chain 1 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 50, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 1 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 26, column 6, included from
#> Chain 1 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 50, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 53, column 8, included from
#> Chain 1 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 52, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 53, column 8, included from
#> Chain 1 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 52, column 2)
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
#> Chain 1 finished in 5.8 seconds.
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 53, column 8, included from
#> Chain 2 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 52, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 53, column 8, included from
#> Chain 2 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 52, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 53, column 8, included from
#> Chain 2 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 52, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 2 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 26, column 6, included from
#> Chain 2 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 50, column 2)
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
#> Chain 2 finished in 7.1 seconds.
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 3 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 26, column 6, included from
#> Chain 3 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 50, column 2)
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
#> Chain 3 finished in 6.5 seconds.
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 26, column 6, included from
#> Chain 4 '/tmp/RtmpSwKEab/model-1a82616f6e06.stan', line 50, column 2)
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
#> Chain 4 finished in 7.6 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 6.8 seconds.
#> Total execution time: 27.3 seconds.
#> 
#> Warning: 366 of 4000 (9.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 3004 of 4000 (75.0%) transitions hit the maximum treedepth limit of 10.
#> See https://mc-stan.org/misc/warnings for details.
```
