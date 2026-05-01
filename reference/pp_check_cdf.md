# Posterior predictive check: CDF comparison

Compares observed data to the posterior predictive distribution using
cumulative distribution functions.

## Usage

``` r
pp_check_cdf(
  fit,
  type = c("individual", "summary"),
  study_id = NULL,
  prob = 0.9
)
```

## Arguments

- fit:

  A `meta_did_fit` object with `method = "sample"`.

- type:

  Character string: `"individual"` (default) for within-study outcome
  distributions, or `"summary"` for the distribution of study-level
  treatment effects.

- study_id:

  Character vector of study IDs to include. `NULL` (default) includes
  all studies of the relevant type.

- prob:

  Width of the credible ribbon. Default `0.9`.

## Value

A ggplot2 object.

## Details

Two modes are available via the `type` argument:

- `"individual"`:

  For each individual-level study, compares the empirical CDF of
  observed outcomes to the posterior predictive CDF (with uncertainty
  ribbon), faceted by study and group/time cell. Requires
  individual-level data in the fit.

- `"summary"`:

  Compares the empirical CDF of observed study-level treatment effects
  to the posterior predictive CDF of replicated study effects. A
  single-panel plot that provides an aggregate calibration check across
  all studies.
