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
  baseline_difference_mean = normal(0, 0.5),
  baseline_difference_sd = cauchy(0.1),
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

  Prior on the population mean of the per-study baseline imbalance
  (treatment-arm vs control-arm pre-treatment mean, on the normalised
  fractional scale). Only used when `baseline_imbalance = "estimated"`.
  Default: `normal(0, 0.5)`.

- baseline_difference_sd:

  Prior on the between-study SD of the baseline imbalance. Only used
  when `baseline_imbalance = "estimated"`. Default: `cauchy(0.1)`.

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
#>   baseline_difference_mean ~ normal(mean = 0, sd = 0.5)
#>   baseline_difference_sd ~ cauchy(scale = 0.1)
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
#>   baseline_difference_mean ~ normal(mean = 0, sd = 0.5)
#>   baseline_difference_sd ~ cauchy(scale = 0.1)
#>   multiplier ~ lognormal(meanlog = 0, sdlog = 0.7)
```
