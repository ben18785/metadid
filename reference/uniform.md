# Specify a uniform prior

Used for the per-study latent baseline parameter in modelled
normalisation modes (`baseline_latent = "treatment"` or `"control"`).
Bounds must be non-negative and `lower < upper`. If no
`baseline_per_study` prior is supplied to
[`set_priors()`](https://ben18785.github.io/metadid/reference/set_priors.md),
the upper bound is computed automatically from the observed baseline
scale (100x the maximum observed pre-period or contemporaneous-control
mean across studies).

## Usage

``` r
uniform(lower = 0, upper)
```

## Arguments

- lower:

  Lower bound (must be non-negative; default 0).

- upper:

  Upper bound (must be positive and strictly greater than `lower`).

## Value

A `did_prior` object.
