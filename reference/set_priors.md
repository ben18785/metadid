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
  delta_pp = normal(0, 10)
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
```
