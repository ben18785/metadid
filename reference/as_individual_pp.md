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
sim <- simulate_meta_did(n_studies = 3, seed = 1)
head(as_individual_pp(sim))
#> # A tibble: 6 × 6
#>   study_id subject_id design group     time  value
#>   <chr>         <int> <chr>  <chr>     <chr> <dbl>
#> 1 study_1           1 pp     treatment pre   0.410
#> 2 study_1           1 pp     treatment post  0.201
#> 3 study_1           2 pp     treatment pre   0.176
#> 4 study_1           2 pp     treatment post  0.296
#> 5 study_1           3 pp     treatment pre   0.750
#> 6 study_1           3 pp     treatment post  0.568
```
