# Summarise simulated DiD data as change-score statistics

Computes within-individual change scores (post minus pre) matched on
`subject_id`, then returns means and SDs for each arm. Produces data in
the format expected by
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
for design `"did_change"`.

## Usage

``` r
as_summary_did_change(sim)
```

## Arguments

- sim:

  A data frame produced by
  [`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md).

## Value

A data frame with one row per study in the format required by
[`validate_summary_data()`](https://ben18785.github.io/metadid/reference/validate_summary_data.md)
for design `"did_change"`.

## Examples

``` r
sim <- sim_meta(n_studies = 3, seed = 1)
#> Error in sim_meta(n_studies = 3, seed = 1): could not find function "sim_meta"
as_summary_did_change(sim)
#> Error: object 'sim' not found
```
