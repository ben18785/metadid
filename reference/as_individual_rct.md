# Extract post-only individual-level RCT data for use with meta_did()

Discards all pre-treatment observations and returns only post-treatment
rows for both arms. Mimics a study in which only post-treatment
measurements were taken.

## Usage

``` r
as_individual_rct(sim)
```

## Arguments

- sim:

  A data frame produced by
  [`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md).

## Value

A data frame in the format required by
[`validate_individual_data()`](https://ben18785.github.io/metadid/reference/validate_individual_data.md)
for design `"rct"`.

## Examples

``` r
sim <- sim_meta(n_studies = 3, seed = 1)
#> Error in sim_meta(n_studies = 3, seed = 1): could not find function "sim_meta"
head(as_individual_rct(sim))
#> Error: object 'sim' not found
```
