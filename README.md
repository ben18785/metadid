
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# metadid

**metadid** is an R package for Bayesian meta-analysis that synthesises
treatment effects across studies with different designs:
difference-in-differences (DiD), randomised controlled trials (RCT), and
pre-post studies. It uses a hierarchical Stan model that places all
designs on a common normalised scale, accounting for design-specific
information and heterogeneity across studies.

## Installation

metadid depends on [cmdstanr](https://mc-stan.org/cmdstanr/) and
[instantiate](https://CRAN.R-project.org/package=instantiate), which
compile Stan models at package install time. Install them first if you
haven’t already:

``` r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()

install.packages("instantiate")
```

Then install metadid from GitHub:

``` r
# install.packages("pak")
pak::pak("bcl206/metadid")
```

## Quick start

### 1. Simulate studies

`simulate_meta_did()` generates individual-level pre/post data for both
arms across a set of studies from a known hierarchical model.
`as_summary_did()` then aggregates this to the four-cell summary
statistics (pre/post × control/treatment) that represent what is
typically reported in a published study.

``` r
library(metadid)
#> 
#> Attaching package: 'metadid'
#> The following object is masked from 'package:base':
#> 
#>     gamma

sim <- simulate_meta_did(
  n_studies     = 20,
  true_effect   = -0.15,
  sigma_effect  = 0.03,
  baseline_mean = 0.45,
  rho           = 0.5,
  seed          = 42
)

studies <- as_summary_did(sim)
head(studies)
#> # A tibble: 6 × 13
#>   study_id design n_control n_treatment mean_pre_control mean_post_control
#>   <chr>    <chr>      <int>       <int>            <dbl>             <dbl>
#> 1 study_1  did          100         100            0.450             0.418
#> 2 study_10 did          100         100            0.448             0.435
#> 3 study_11 did          100         100            0.420             0.409
#> 4 study_12 did          100         100            0.475             0.444
#> 5 study_13 did          100         100            0.447             0.435
#> 6 study_14 did          100         100            0.461             0.416
#> # ℹ 7 more variables: sd_pre_control <dbl>, sd_post_control <dbl>,
#> #   mean_pre_treatment <dbl>, mean_post_treatment <dbl>,
#> #   sd_pre_treatment <dbl>, sd_post_treatment <dbl>, rho <dbl>
```

The true population treatment effect is `-0.15` on the raw scale, or
approximately -0.333 after normalising by the baseline mean of `0.45`.

### 2. Fit the model

`meta_did()` fits a hierarchical Bayesian model. Setting
`method = "optimize"` finds the maximum a posteriori (MAP) estimate via
L-BFGS — much faster than full MCMC and useful for quick iteration.

``` r
fit <- meta_did(
  summary_data = studies,
  method       = "optimize",
  seed         = 42
)

print(fit)
```

    #> Bayesian meta-analysis (metadid)
    #> Studies: DiD = 20 | RCT = 0 | Pre-Post = 0 | DiD (change only) = 0 
    #> Population treatment effect: -0.303  (MAP estimate, no uncertainty)

### 3. Inspect study-level estimates

`summary()` returns a data frame of population- and study-level
parameters. With MAP optimisation, only point estimates are available
(`sd`, `lo`, and `hi` are `NA`); use `method = "sample"` for full
posterior uncertainty.

``` r
summary(fit)
```

    #>                       parameter       mean sd lo hi
    #> 1         treatment_effect_mean -0.3032553 NA NA NA
    #> 2           treatment_effect_sd  0.2621790 NA NA NA
    #> 3  treatment_effect_did_summary -0.2626185 NA NA NA
    #> 4  treatment_effect_did_summary -0.4008142 NA NA NA
    #> 5  treatment_effect_did_summary -0.1788253 NA NA NA
    #> ...
