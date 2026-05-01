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
sim <- simulate_meta_did(n_studies = 3, seed = 1)
head(as_individual_did(sim))
#> # A tibble: 6 × 6
#>   study_id subject_id design group   time  value
#>   <chr>         <int> <chr>  <chr>   <chr> <dbl>
#> 1 study_1           1 did    control pre   0.641
#> 2 study_1           1 did    control post  0.542
#> 3 study_1           2 did    control pre   0.490
#> 4 study_1           2 did    control post  0.382
#> 5 study_1           3 did    control pre   0.352
#> 6 study_1           3 did    control post  0.564
```
