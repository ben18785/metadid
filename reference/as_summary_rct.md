# Summarise simulated DiD data as RCT-style post-only statistics

Discards all pre-treatment observations and returns post-treatment means
and SDs for both arms. Produces data in the format expected by
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
for design `"rct"`.

## Usage

``` r
as_summary_rct(sim)
```

## Arguments

- sim:

  A data frame produced by
  [`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md).

## Value

A data frame with one row per study in the format required by
[`validate_summary_data()`](https://ben18785.github.io/metadid/reference/validate_summary_data.md)
for design `"rct"`.

## Examples

``` r
sim <- sim_meta(n_studies = 3, seed = 1)
#> Error in sim_meta(n_studies = 3, seed = 1): could not find function "sim_meta"
as_summary_rct(sim)
#> Error: object 'sim' not found
```
