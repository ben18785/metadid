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
sim <- simulate_meta_did(n_studies = 3, seed = 1)
as_summary_did(sim)
#> # A tibble: 3 × 13
#>   study_id design n_control n_treatment mean_pre_control mean_post_control
#>   <chr>    <chr>      <int>       <int>            <dbl>             <dbl>
#> 1 study_1  did          100         100            0.463             0.438
#> 2 study_2  did          100         100            0.441             0.424
#> 3 study_3  did          100         100            0.452             0.434
#> # ℹ 7 more variables: sd_pre_control <dbl>, sd_post_control <dbl>,
#> #   mean_pre_treatment <dbl>, mean_post_treatment <dbl>,
#> #   sd_pre_treatment <dbl>, sd_post_treatment <dbl>, rho <dbl>
```
