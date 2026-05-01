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
sim <- simulate_meta_did(n_studies = 3, seed = 1)
head(as_individual_rct(sim))
#> # A tibble: 6 × 5
#>   study_id design group   time  value
#>   <chr>    <chr>  <chr>   <chr> <dbl>
#> 1 study_1  rct    control post  0.542
#> 2 study_1  rct    control post  0.382
#> 3 study_1  rct    control post  0.564
#> 4 study_1  rct    control post  0.534
#> 5 study_1  rct    control post  0.569
#> 6 study_1  rct    control post  0.504
```
