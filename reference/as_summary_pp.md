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
sim <- simulate_meta_did(n_studies = 3, seed = 1)
as_summary_pp(sim)
#> # A tibble: 3 × 8
#>   study_id design n_treatment mean_pre_treatment sd_pre_treatment
#>   <chr>    <chr>        <int>              <dbl>            <dbl>
#> 1 study_1  pp             100              0.451            0.124
#> 2 study_2  pp             100              0.425            0.131
#> 3 study_3  pp             100              0.448            0.124
#> # ℹ 3 more variables: mean_post_treatment <dbl>, sd_post_treatment <dbl>,
#> #   rho <dbl>
```
