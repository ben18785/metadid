# Pre-post share of the information about the pooled effect

The fraction of what is known about \\\mu\_\theta\\ that comes from
pre-post studies, which is also the factor by which a systematic
difference between their trend distribution and the DiD studies' biases
the pooled effect (see
[`vignette("model-details")`](https://ben18785.github.io/metadid/articles/model-details.md)).

## Usage

``` r
omega_pp(fit)
```

## Arguments

- fit:

  A
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  fit containing both DiD and pre-post summary studies.

## Value

A list with `draws` (numeric vector of \\\omega_P\\ per draw), `median`,
`q5`, `q95` and `cap`.

## Details

A DiD study supplies an effect estimate and a trend estimate that
**share its control arm**, so the two are correlated, with
\\\mathrm{Cov} = -\mathrm{var}(\text{control change})\\. Treating them
as independent overstates the share by a few percent, so the 2x2
information per study is summed rather than two scalars. Writing \\A\\
for the total pre-post precision, \$\$I = \sum_i \Sigma_i^{-1} + A
\mathbf{1}\mathbf{1}^\top, \qquad \omega\_{P} = \left(I^{-1} \[A,
A\]^\top\right)\_1 .\$\$

The returned `cap` is S3's saturation result \\h/(g+h)\\: the share
cannot exceed what the DiD studies know about the trend distribution,
however many pre-post studies are added. It uses the scalar
(independent) form, so treat it as the approximation it is.

Computed per posterior draw, since it depends on \\\tau\_\theta\\ and
\\\tau\_\beta\\. Summary-level data only.

## See also

[`sensitivity_trend()`](https://ben18785.github.io/metadid/reference/sensitivity_trend.md),
[`delta_star()`](https://ben18785.github.io/metadid/reference/delta_star.md)
