# Share of the evidence each study and design contributes

Inverse-variance weights on the random-effects scale, \\w_i \propto
1/(s_i^2 + \tau\_\theta^2)\\, normalised to sum to one. The answer to
"which studies is this result actually resting on?", which a table of
estimates never shows.

## Usage

``` r
information_share(fit)
```

## Arguments

- fit:

  A
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  fit with summary-level studies.

## Value

A data frame with one row per study: `study_id`, `design`, `se`,
`weight` (summing to one), ordered by decreasing weight.

## Details

\\s_i\\ is the design-appropriate sampling SD of that study's effect
estimate: the double-difference SE for DiD, the change-score SE for
pre-post, the arm-contrast SE for post-only. \\\tau\_\theta\\ is taken
at its posterior median.

This is an approximation in one respect worth knowing: it ignores the
coupling through the nuisance parameters, so a post-only study's weight
is slightly overstated relative to a DiD study's, since part of its
information is spent on \\\gamma\\.
[`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md)
computes the exact two-design version. Summary-level data only.

## See also

[`plot_information_share()`](https://ben18785.github.io/metadid/reference/plot_information_share.md),
[`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md)
