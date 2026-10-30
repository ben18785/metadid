# Share of the evidence each study and design contributes

Which studies the pooled effect is actually resting on, which a table of
estimates never shows. Two columns, answering two different questions:

- `info_loss` is the fraction of the precision of \\\mu\_\theta\\ lost
  by dropping that study, from the information matrix over
  \\(\mu\_\beta, \mu\_\gamma, \mu\_\theta)\\ under the fitted hierarchy.
  **It does not sum to one.**

- `weight_naive` is the inverse-variance weight, \\w_i \propto
  1/(s_i^2 + \tau\_\theta^2)\\, which is what a conventional
  meta-analysis of the same point estimates would use. It does sum to
  one.

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
`info_loss` (not summing to one; `NA` outside the linear-Gaussian case)
and `weight_naive` (summing to one), ordered by decreasing `info_loss`.

## Details

`info_loss` sums to one only when no design enables another – a DiD-only
fit, where it reduces to a partition of the evidence (verified in
`test-sensitivity.R`). In a mixed analysis it sums to **more** than one,
because dropping a DiD study also costs the pre-post studies the trend
information they borrow from it. That excess is the point: on a 10/10/10
mixed fit the losses were 0.73 for the DiD arm, 0.19 for pre-post and
0.09 for post-only, summing to 1.01 – the DiD studies are close to
indispensable, and not because they carry most of the weight on the
mean.

For a design-level *share*, which does partition, use
[`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md).
The two are not interchangeable: on that same fit it puts the pre-post
share at 0.414 against a naive 0.410. Note also that the obvious fix to
the naive weight – adding \\\tau\_\beta^2\\ or \\\tau\_\gamma^2\\ to
those designs' denominators – **over**-corrects, giving 0.367, because
those designs help to estimate the nuisance means rather than only
consuming them.

Both columns concern the **mean**. Neither says anything about a study's
effect on the between-study SD, and a study carrying the smallest weight
in an analysis can still double the width of the pooled interval – see
[`study_influence()`](https://ben18785.github.io/metadid/reference/study_influence.md).

\\s_i\\ is the design-appropriate sampling SD: the double-difference SE
for DiD, the change-score SE for pre-post, the ratio SE for post-only.
Variance components are taken at their posterior medians. `info_loss`
needs the linear-Gaussian case (summary data, baseline normalisation)
and is `NA` outside it.

## See also

[`plot_information_share()`](https://ben18785.github.io/metadid/reference/plot_information_share.md),
[`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md),
[`study_influence()`](https://ben18785.github.io/metadid/reference/study_influence.md)
