# Fit a Bayesian meta-analysis model across study designs

Fits a hierarchical Bayesian model synthesising treatment effects from
studies with different designs: difference-in-differences (DiD),
randomised controlled trials (RCT), and pre-post studies. All designs
contribute to a shared population treatment effect.

## Usage

``` r
meta_did(
  summary_data = NULL,
  individual_data = NULL,
  normalise = TRUE,
  baseline_latent_arm = c("treatment", "control"),
  robust_heterogeneity = FALSE,
  design_effects = FALSE,
  hierarchical_rho = TRUE,
  correlated_effects = FALSE,
  baseline_imbalance = c("estimated", "fixed_zero"),
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

  How to handle the per-study baseline difference \\\gamma_i\\ for
  **non-DiD** designs. DiD studies always estimate \\\gamma_i\\
  per-study because the pre-treatment means on both arms identify it
  directly from the data; this argument controls only the RCT branch.
  One of:

  `"estimated"`

  :   (Default) RCT studies also have per-study `baseline_difference_i`
      parameters, drawn from a hierarchical prior shared with DiD
      studies. Because RCT data cannot identify per-study \\\gamma_i\\
      alone (only one post-treatment observation per arm), the
      hierarchical prior \\\\ informed by DiD's per-study estimates \\\\
      is what pins down the decomposition into baseline imbalance and
      treatment effect. Priors on the population hyperparameters are set
      via `baseline_difference_mean` and `baseline_difference_sd` in
      [`set_priors()`](https://ben18785.github.io/metadid/reference/set_priors.md).

  `"fixed_zero"`

  :   Fix \\\gamma_i = 0\\ for RCT studies. The treatment-arm baseline
      is constrained equal to the control-arm baseline. Use when
      randomisation can be assumed to eliminate baseline imbalances. DiD
      studies are unaffected.

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

  Logical. If `FALSE` (default), `meta_did()` will stop with an error
  when no DiD studies are present, because the treatment effect is not
  identified from the data without the double-difference structure. Set
  to `TRUE` to override this check if you understand the limitation (the
  posterior will be prior-driven).

- ...:

  Additional arguments passed to the underlying CmdStanModel method:
  `$sample()` when `method = "sample"` (e.g., `parallel_chains`,
  `adapt_delta`) or `$optimize()` when `method = "optimize"` (e.g.,
  `algorithm`, `iter`).

## Value

A `meta_did_fit` object. See
[`print.meta_did_fit()`](https://ben18785.github.io/metadid/reference/print.meta_did_fit.md)
and
[`summary.meta_did_fit()`](https://ben18785.github.io/metadid/reference/summary.meta_did_fit.md)
for extracting results. When `method = "optimize"`, the summary contains
MAP point estimates only; `sd`, `lo`, and `hi` columns will be `NA`.

## Details

DiD studies are required for identification of the treatment effect.
Without them, the treatment effect is confounded with time trends
(pre-post) or baseline group differences (RCT). See
[`vignette("model-details")`](https://ben18785.github.io/metadid/articles/model-details.md)
for a full discussion of the model, normalisation, and identification.

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
  fit <- meta_did(summary_data = studies)
}
#> Running MCMC with 4 sequential chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 1 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 1 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 1 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 1 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 1 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 1 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1 
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: Exception: normal_lpdf: Location parameter is inf, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model_functions.stan', line 19, column 2, included from
#> Chain 1 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 9, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/rct_summary_model.stan', line 36, column 4, included from
#> Chain 1 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 56, column 2)
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
#> Chain 1 finished in 11.8 seconds.
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2 
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 2 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
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
#> Chain 2 finished in 11.6 seconds.
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3 
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3 
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3 
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3 
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3 
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: Exception: multi_normal_lpdf: Location parameter[1] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 3 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
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
#> Chain 3 finished in 10.1 seconds.
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: Exception: multi_normal_lpdf: Location parameter[2] is -nan, but must be finite! (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model_functions.stan', line 49, column 2, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 7, column 2) (in '/home/runner/work/_temp/Library/00LOCK-metadid/00new/metadid/bin/stan/did_summary_model.stan', line 32, column 6, included from
#> Chain 4 '/tmp/RtmpMKSaj1/model-1b06127c804.stan', line 54, column 2)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4 
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
#> Chain 4 finished in 11.3 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 11.2 seconds.
#> Total execution time: 45.1 seconds.
#> 
#> Warning: 142 of 4000 (4.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 3656 of 4000 (91.0%) transitions hit the maximum treedepth limit of 10.
#> See https://mc-stan.org/misc/warnings for details.
```
