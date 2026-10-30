# Breakdown value for trend exchangeability

\\\Delta^\* = \hat\mu\_\theta / \omega_P\\: the systematic difference
between the pre-post and DiD trend distributions that would be needed to
move the pooled effect to zero. A large value relative to plausible
secular trends indicates robustness; a small one flags dependence on the
assumption.

## Usage

``` r
delta_star(fit)
```

## Arguments

- fit:

  A
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  fit containing both DiD and pre-post summary studies.

## Value

A list with `draws`, `median`, `q5`, `q95` and the `omega` object it was
derived from.

## Details

\\\Delta^\*\\ is a ratio of two posterior quantities, so its tails are
heavy when \\\omega_P\\ is near zero — which is the robust case, since a
vanishing share means no shift can explain the effect away. Read the
median and the interval, not the mean.

Units are those of the fitted effect, per study interval: on the
normalised scale a \\\Delta^\*\\ of \\-0.5\\ means pre-post trends would
have to run 50 percentage points of baseline below DiD trends **over
each study's own pre-to-post interval**. It is not an annual rate, since
interval lengths differ across studies.

## See also

[`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md),
[`sensitivity_trend()`](https://ben18785.github.io/metadid/reference/sensitivity_trend.md)
