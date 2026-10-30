# Per-study influence on the pooled result

Leave-one-study-out influence, via Pareto-smoothed importance sampling
on the marginal likelihood, with optional exact refits for the studies
PSIS cannot approximate. Answers "would this result survive without
study *i*?".

## Usage

``` r
study_influence(
  fit,
  refit = c("none", "flagged", "all"),
  k_threshold = 0.7,
  quiet = FALSE
)
```

## Arguments

- fit:

  A
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  fit.

- refit:

  Whether to refit without studies PSIS flags. `"none"` (default) does
  no fitting and costs nothing. `"flagged"` refits once per study with
  `khat` above `k_threshold`. `"all"` refits once per study. Refits are
  expensive, especially with individual-level data, so nothing fits
  unless asked.

- k_threshold:

  Pareto k above which a study is flagged.

- quiet:

  Suppress the message naming how many refits will run.

## Value

A data frame with one row per study: `study_id`, `design`, `weight`,
`elpd_i`, `khat`, `flagged`, and — when refits ran — `d_effect_mean`,
`d_effect_sd` and `d_ci_width`, the changes from removing that study.

## Details

Two things are reported, and they are not the same:

- `weight` is that study's inverse-variance share, from
  [`information_share()`](https://ben18785.github.io/metadid/reference/information_share.md).
  It governs the pull on the pooled **mean**.

- `khat` is the Pareto diagnostic from PSIS. It is formally a statement
  about whether the importance-sampling approximation is reliable, but
  it is elevated precisely when the leave-this-out posterior is far from
  the full one – so a high value means removing the study moves the
  posterior.

A low weight does **not** make a high `khat` ignorable. Weight describes
the effect on the mean, while a discordant study acts on the
between-study SD, and that widens every interval. In testing, the
lowest-weighted study of thirteen (weight 0.069) doubled
`treatment_effect_sd` and doubled the width of the pooled interval. That
is why the refits report both.

PSIS runs on `log_lik_marginal`, which integrates out the study-level
parameters. The conditional `log_lik` is unusable for this: a study's
own theta is informed almost entirely by that study, so leaving it out
makes the importance ratios heavy-tailed for every study regardless of
influence (measured at 18 of 20 studies above 0.7 on a well-behaved
fit). The marginal version is only available for configurations where
the integral is exact — see `shared_generated_quantities.stan` — and
this function says so rather than falling back silently.

## See also

[`information_share()`](https://ben18785.github.io/metadid/reference/information_share.md),
[`sensitivity_trend()`](https://ben18785.github.io/metadid/reference/sensitivity_trend.md)
