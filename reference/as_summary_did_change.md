# Summarise simulated DiD data as change-score statistics

Computes within-individual change scores (post minus pre) matched on
`subject_id`, then returns means and SDs for each arm. Produces data in
the format expected by
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
for design `"did_change"`.

## Usage

``` r
as_summary_did_change(sim)
```

## Arguments

- sim:

  A data frame produced by
  [`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md).

## Value

A data frame with one row per study in the format required by
[`validate_summary_data()`](https://ben18785.github.io/metadid/reference/validate_summary_data.md)
for design `"did_change"`.

## Examples

``` r
sim <- simulate_meta_did(n_studies = 3, seed = 1)
as_summary_did_change(sim)
#> # A tibble: 3 × 8
#>   study_id design    n_control n_treatment mean_change_control sd_change_control
#>   <chr>    <chr>         <int>       <int>               <dbl>             <dbl>
#> 1 study_1  did_chan…       100         100             -0.0250             0.117
#> 2 study_2  did_chan…       100         100             -0.0168             0.128
#> 3 study_3  did_chan…       100         100             -0.0181             0.118
#> # ℹ 2 more variables: mean_change_treatment <dbl>, sd_change_treatment <dbl>
```
