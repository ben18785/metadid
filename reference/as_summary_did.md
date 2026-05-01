# Summarise simulated DiD data to study-level statistics

Aggregates individual-level data from
[`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md)
to the four-cell summary (pre/post × control/treatment) required by
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).
The pre-post correlation \\\rho\\ is estimated empirically from matched
pairs within each arm and then averaged.

## Usage

``` r
as_summary_did(sim)
```

## Arguments

- sim:

  A data frame produced by
  [`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md).

## Value

A data frame with one row per study in the format required by
[`validate_summary_data()`](https://ben18785.github.io/metadid/reference/validate_summary_data.md)
for design `"did"`.

## Examples

``` r
sim <- sim_meta(n_studies = 3, seed = 1)
#> Error in sim_meta(n_studies = 3, seed = 1): could not find function "sim_meta"
as_summary_did(sim)
#> Error: object 'sim' not found
```
