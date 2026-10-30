# Test whether the designs agree

Refits the model with `design_effects = TRUE`, which adds an offset to
the population effect mean for RCT and pre-post studies relative to DiD,
and returns those offsets. They are the model's own estimate of how far
each design disagrees with the DiD anchor — the assumption that all
designs estimate one common effect, made checkable.

## Usage

``` r
design_agreement(fit, probs = c(0.05, 0.95), quiet = FALSE)
```

## Arguments

- fit:

  A
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  fit carrying more than one design.

- probs:

  Interval probabilities.

- quiet:

  Suppress the refit message.

## Value

A list with `offsets` (a data frame of `design`, `estimate`, `lo`, `hi`,
`n_studies`) and the refitted object `fit`.

## Details

Offsets near zero support pooling. An offset that excludes zero says the
designs are not exchangeable, which may reflect a real difference
(selection, different follow-up, a different estimand) or a fault in how
that design is being handled. It will not tell you which.

One refit, so it costs what the original fit cost. Note that
`design_effects = TRUE` re-references `treatment_effect_mean` to the DiD
studies, so the pooled number from the refit is not the one to report —
only the offsets.

Prefer this to fitting each design subset separately. Subsets of
post-only studies have to pin the time trend at zero to be identified at
all, which puts their effect on the control follow-up scale rather than
the baseline scale — a difference of a factor \\(1+\tilde\beta)\\,
around 9% at a typical trend, which is larger than the disagreement you
would be looking for.

## See also

[`plot_design_agreement()`](https://ben18785.github.io/metadid/reference/plot_design_agreement.md)
