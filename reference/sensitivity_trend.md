# Sensitivity of the pooled effect to trend exchangeability

Refits the model across a grid of \\\Delta\\, the systematic difference
between the pre-post and DiD trend distributions, and reports how the
pooled effect responds. This is the exact version of
[`delta_star()`](https://ben18785.github.io/metadid/reference/delta_star.md)'s
approximation: it captures the shrinkage and the nonlinearity of the
normalised post-only reparameterisation, at the cost of one fit per grid
point.

## Usage

``` r
sensitivity_trend(fit, delta = NULL, probs = c(0.05, 0.95), quiet = FALSE)
```

## Arguments

- fit:

  A
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  fit containing pre-post studies.

- delta:

  Numeric vector of shifts to refit at. Defaults to a symmetric grid
  spanning roughly the population trend, which is the scale on which a
  plausible exchangeability failure would sit.

- probs:

  Interval probabilities for the reported bounds.

- quiet:

  Suppress the per-refit progress message.

## Value

A list with `grid` (a data frame of `delta`, `estimate`, `lo`, `hi`,
`divergences`), the fitted `slope`, `omega_empirical` (which is
`-slope`, and is what
[`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md)
predicts), `delta_point`, `delta_ci`, and the `fits` themselves.

## Details

Each refit sets `pp_trend_shift = delta`, which asserts \\\beta_j \sim
N(\mu\_\beta + \Delta, \tau\_\beta^2)\\ for pre-post studies only. Every
other setting is taken from the original fit, so a sweep cannot differ
from it in a sampler setting or a modelling switch.

Two breakdown values are returned. `delta_point` is where the posterior
mean crosses zero — the SI's \\\Delta^\*\\. `delta_ci` is where the
interval first includes zero, which is the more useful of the two: it is
the shift at which the *conclusion*, rather than the point estimate,
stops holding. Both come from linear interpolation on the grid and are
`NA` when no crossing occurs inside it.

## See also

[`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md),
[`delta_star()`](https://ben18785.github.io/metadid/reference/delta_star.md)
