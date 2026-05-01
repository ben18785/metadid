# Posterior predictive check: individual-level CDF comparison

For each individual-level study, compares the empirical CDF of observed
outcomes to the posterior predictive CDF (with uncertainty ribbon),
faceted by study and group/time cell.

## Usage

``` r
pp_check_cdf(fit, study_id = NULL, prob = 0.9)
```

## Arguments

- fit:

  A `meta_did_fit` object with `method = "sample"` and individual-level
  data.

- study_id:

  Character vector of study IDs to include. `NULL` (default) shows all
  individual-level studies.

- prob:

  Width of the credible ribbon. Default `0.9`.

## Value

A ggplot2 object.
