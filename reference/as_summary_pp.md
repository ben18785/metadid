# Summarise simulated DiD data as pre-post (treatment arm only) statistics

Discards all control-arm observations and returns pre/post means and SDs
for the treatment arm. Produces data in the format expected by
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
for design `"pp"`.

## Usage

``` r
as_summary_pp(sim)
```

## Arguments

- sim:

  A data frame produced by
  [`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md).

## Value

A data frame with one row per study in the format required by
[`validate_summary_data()`](https://ben18785.github.io/metadid/reference/validate_summary_data.md)
for design `"pp"`.

## Examples

``` r
sim <- sim_meta(n_studies = 3, seed = 1)
#> Error in sim_meta(n_studies = 3, seed = 1): could not find function "sim_meta"
as_summary_pp(sim)
#> Error: object 'sim' not found
```
