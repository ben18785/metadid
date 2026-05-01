# Posterior predictive check: study-level treatment effects

For each study, compares the observed naive treatment effect to the
posterior predictive distribution under the fitted model. Uses full
posterior draws for all quantities including standard errors.

## Usage

``` r
pp_check_effects(fit, study_id = NULL, prob = 0.9)
```

## Arguments

- fit:

  A `meta_did_fit` object with `method = "sample"`.

- study_id:

  Character vector of study IDs to include. `NULL` (default) shows all
  studies.

- prob:

  Width of the credible shading (not currently used but reserved).
  Default `0.9`.

## Value

A ggplot2 object, faceted by study.
