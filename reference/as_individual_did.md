# Extract full individual-level DiD data for use with meta_did()

Returns the output of
[`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md)
reformatted as the `individual_data` argument of
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md):
one row per observation with columns `study_id`, `design`, `group`,
`time`, `value`.

## Usage

``` r
as_individual_did(sim)
```

## Arguments

- sim:

  A data frame produced by
  [`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md).

## Value

A data frame in the format required by
[`validate_individual_data()`](https://ben18785.github.io/metadid/reference/validate_individual_data.md)
for design `"did"`.

## Examples

``` r
sim <- sim_meta(n_studies = 3, seed = 1)
#> Error in sim_meta(n_studies = 3, seed = 1): could not find function "sim_meta"
head(as_individual_did(sim))
#> Error: object 'sim' not found
```
