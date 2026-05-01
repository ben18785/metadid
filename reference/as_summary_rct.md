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
sim <- simulate_meta_did(n_studies = 3, seed = 1)
as_summary_rct(sim)
#> # A tibble: 3 × 8
#>   study_id design n_control n_treatment mean_post_control sd_post_control
#>   <chr>    <chr>      <int>       <int>             <dbl>           <dbl>
#> 1 study_1  rct          100         100             0.438           0.113
#> 2 study_2  rct          100         100             0.424           0.116
#> 3 study_3  rct          100         100             0.434           0.137
#> # ℹ 2 more variables: mean_post_treatment <dbl>, sd_post_treatment <dbl>
```
