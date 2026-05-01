# Extract treatment-arm individual-level pre-post data for use with meta_did()

Discards all control-arm observations and returns treatment-arm rows
(both pre and post). Mimics a study with no control group.

## Usage

``` r
as_individual_pp(sim)
```

## Arguments

- sim:

  A data frame produced by
  [`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md).

## Value

A data frame in the format required by
[`validate_individual_data()`](https://ben18785.github.io/metadid/reference/validate_individual_data.md)
for design `"pp"`.

## Examples

``` r
sim <- sim_meta(n_studies = 3, seed = 1)
#> Error in sim_meta(n_studies = 3, seed = 1): could not find function "sim_meta"
head(as_individual_pp(sim))
#> Error: object 'sim' not found
```
