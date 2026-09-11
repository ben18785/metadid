# Set prior distributions for the meta-analysis model

Construct a `did_priors` object specifying the prior distribution for
each population-level parameter. Any parameter not supplied takes a
default.

## Usage

``` r
set_priors(
  treatment_effect_mean = normal(0, 10),
  treatment_effect_sd = cauchy(5),
  time_trend_mean = normal(0, 10),
  time_trend_sd = cauchy(5),
  rho_mean = normal(0, 1),
  rho_sd = normal(0, 0.5),
  nu = gamma(2, 0.1),
  delta_rct = normal(0, 10),
  delta_pp = normal(0, 10),
  sigma = cauchy(5),
  beta_cov = normal(0, 10),
  lkj_eta = lkj(2),
  baseline_difference_mean = normal(0, 0.05),
  baseline_difference_sd = cauchy(0.1),
  kappa = normal(0, 0.5),
  multiplier = lognormal(0, 0.7)
)
```

## Arguments

- treatment_effect_mean:

  Prior on the population treatment effect mean. Default:
  `normal(0, 10)`.

- treatment_effect_sd:

  Prior on the between-study SD. Default: `cauchy(5)`.

- time_trend_mean:

  Prior on the population time-trend mean. Default: `normal(0, 10)`.

- time_trend_sd:

  Prior on the between-study time-trend SD. Default: `cauchy(5)`.

- rho_mean:

  Prior on the mean of the Fisher-z transformed pre-post correlation
  (only used when `hierarchical_rho = TRUE`). Default: `normal(0, 1)`.

- rho_sd:

  Prior on the SD of the Fisher-z transformed pre-post correlation (only
  used when `hierarchical_rho = TRUE`). Default: `normal(0, 0.5)`.

- nu:

  Prior on the degrees of freedom for between-study heterogeneity (only
  used when `robust_heterogeneity = TRUE`). Default: `gamma(2, 0.1)`.

- delta_rct:

  Prior on the RCT design offset relative to DiD (only used when
  `design_effects = TRUE`). Default: `normal(0, 10)`.

- delta_pp:

  Prior on the Pre-Post design offset relative to DiD (only used when
  `design_effects = TRUE`). Default: `normal(0, 10)`.

- sigma:

  Prior on the study-level observation standard deviations (shared
  across all designs). Default: `cauchy(5)`.

- beta_cov:

  Prior on the covariate regression coefficients (only used when
  `covariates` is specified in
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)).
  Default: `normal(0, 10)`.

- lkj_eta:

  Prior on the Cholesky factor of the correlation matrix between
  treatment effects and time trends (only used when
  `correlated_effects = TRUE`). Default: `lkj(2)`, which gently
  regularises toward zero correlation.

- baseline_difference_mean:

  Prior on the population mean of the per-study baseline imbalance among
  **non-randomised** studies (treatment-arm vs control-arm pre-treatment
  mean, on the normalised fractional scale). Only used when
  `mu_gamma = "estimated"` in
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md);
  under the default `mu_gamma = "zero"` the population mean is pinned at
  zero and this prior is ignored. Default: `normal(0, 0.05)`.

  The old default was `normal(0, 0.5)`, which was both internally
  inconsistent with the `cauchy(0.1)` prior on the between-study SD (it
  asserted the *average* imbalance could be far larger than the *spread*
  around it) and materially informative about the pooled treatment
  effect, because baseline imbalance is unidentified for post-only
  studies and subtracts directly from their estimated effect.

- baseline_difference_sd:

  Prior on the between-study SD of the baseline imbalance among
  non-randomised studies. Default: `cauchy(0.1)`.

- kappa:

  Prior on the excess-imbalance factor for **randomised** studies,
  interpreted as half-normal because `kappa` is constrained positive.
  Only used when `kappa = "estimate"` in
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).
  `kappa^2` is the variance inflation of a randomised study's baseline
  contrast beyond simple random sampling, so `kappa = 0` is perfect
  randomisation. Default: `normal(0, 0.5)`, which places most mass below
  `kappa = 1` (a doubling of the baseline-contrast variance).

- multiplier:

  Prior on the multiplicative-covariate effect multiplier (only used
  when `multiplicative_covariate` is specified in
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)).
  With one or two multiplicative covariates the same prior is applied
  independently to every estimated non-reference-level factor (of either
  covariate). Must be a
  [`lognormal()`](https://ben18785.github.io/metadid/reference/lognormal.md)
  prior, placed on the log of the multiplier so it is strictly positive
  with no boundary at zero. Default: `lognormal(0, 0.7)` — a median of 1
  (the no-multiplicative-effect case), with a central 95% range of
  roughly `[0.25, 3.9]` on the natural scale.

## Value

A `did_priors` object.

## Examples

``` r
# Use defaults
set_priors()
#> Prior distributions:
#>   treatment_effect_mean ~ normal(mean = 0, sd = 10)
#>   treatment_effect_sd ~ cauchy(scale = 5)
#>   time_trend_mean ~ normal(mean = 0, sd = 10)
#>   time_trend_sd ~ cauchy(scale = 5)
#>   rho_mean ~ normal(mean = 0, sd = 1)
#>   rho_sd ~ normal(mean = 0, sd = 0.5)
#>   nu ~ gamma(shape = 2, rate = 0.1)
#>   delta_rct ~ normal(mean = 0, sd = 10)
#>   delta_pp ~ normal(mean = 0, sd = 10)
#>   sigma ~ cauchy(scale = 5)
#>   beta_cov ~ normal(mean = 0, sd = 10)
#>   lkj_eta ~ lkj(eta = 2)
#>   baseline_difference_mean ~ normal(mean = 0, sd = 0.05)
#>   baseline_difference_sd ~ cauchy(scale = 0.1)
#>   kappa ~ normal(mean = 0, sd = 0.5)
#>   multiplier ~ lognormal(meanlog = 0, sdlog = 0.7)

# Override one prior
set_priors(treatment_effect_sd = cauchy(2))
#> Prior distributions:
#>   treatment_effect_mean ~ normal(mean = 0, sd = 10)
#>   treatment_effect_sd ~ cauchy(scale = 2)
#>   time_trend_mean ~ normal(mean = 0, sd = 10)
#>   time_trend_sd ~ cauchy(scale = 5)
#>   rho_mean ~ normal(mean = 0, sd = 1)
#>   rho_sd ~ normal(mean = 0, sd = 0.5)
#>   nu ~ gamma(shape = 2, rate = 0.1)
#>   delta_rct ~ normal(mean = 0, sd = 10)
#>   delta_pp ~ normal(mean = 0, sd = 10)
#>   sigma ~ cauchy(scale = 5)
#>   beta_cov ~ normal(mean = 0, sd = 10)
#>   lkj_eta ~ lkj(eta = 2)
#>   baseline_difference_mean ~ normal(mean = 0, sd = 0.05)
#>   baseline_difference_sd ~ cauchy(scale = 0.1)
#>   kappa ~ normal(mean = 0, sd = 0.5)
#>   multiplier ~ lognormal(meanlog = 0, sdlog = 0.7)
```
