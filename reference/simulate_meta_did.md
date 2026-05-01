# Simulate individual-level DiD data for a collection of studies

Generates individual-level pre/post observations for both arms of a set
of difference-in-differences studies under a hierarchical model. This is
the gold-standard view of the data; the `as_*` family of functions then
produce the data as it would appear under different levels of access.

## Usage

``` r
simulate_meta_did(
  n_studies = 20L,
  n_control = 100L,
  n_treatment = 100L,
  true_effect = -0.15,
  sigma_effect = 0.03,
  true_trend = -0.02,
  sigma_trend = 0,
  baseline_mean = 0.45,
  baseline_sd = 0,
  within_sd = 0.12,
  rho = 0.5,
  seed = NULL
)
```

## Arguments

- n_studies:

  Number of studies. Default `20`.

- n_control:

  Number of individuals per study in the control arm. Default `100L`.

- n_treatment:

  Number of individuals per study in the treatment arm. Default `100L`.

- true_effect:

  Population mean treatment effect. Default `-0.15`.

- sigma_effect:

  Between-study SD of treatment effects. Default `0.03`.

- true_trend:

  Population mean time trend. Default `-0.02`.

- sigma_trend:

  Between-study SD of time trends. Default `0`.

- baseline_mean:

  Population mean of study baselines. Default `0.45`.

- baseline_sd:

  Between-study SD of baselines. Default `0`.

- within_sd:

  Individual-level SD (the same for pre and post and for both groups).
  Default `0.12`.

- rho:

  Pre-post correlation within individuals. Directly parameterises the
  off-diagonal of the bivariate normal covariance matrix. Default `0.5`.

- seed:

  Integer random seed for reproducibility. Default `NULL`.

## Value

A data frame with columns `study_id`, `subject_id`, `group` (`"control"`
or `"treatment"`), `time` (`"pre"` or `"post"`), `value`. The true
study-level parameters are attached as attribute `"true_params"`.

## Data-generating model

Study-level parameters are drawn hierarchically: \$\$\theta_i \sim
\text{Normal}(\texttt{true\\effect},\\ \texttt{sigma\\effect}^2)\$\$
\$\$\gamma_i \sim \text{Normal}(\texttt{true\\trend},\\
\texttt{sigma\\trend}^2)\$\$ \$\$b_i \sim
\text{Normal}(\texttt{baseline\\mean},\\ \texttt{baseline\\sd}^2)\$\$

Within each study, individual (pre, post) pairs are drawn from a
bivariate normal distribution with covariance matrix \$\$\Sigma =
\sigma^2 \begin{pmatrix} 1 & \rho \\ \rho & 1 \end{pmatrix}\$\$ making
the role of \\\rho\\ as the pre-post correlation explicit. The mean
vector for the control group is \\(b_i,\\ b_i + \gamma_i)\\ and for the
treatment group \\(b_i,\\ b_i + \gamma_i + \theta_i)\\.

## See also

[`as_individual_did()`](https://ben18785.github.io/metadid/reference/as_individual_did.md),
[`as_individual_rct()`](https://ben18785.github.io/metadid/reference/as_individual_rct.md),
[`as_individual_pp()`](https://ben18785.github.io/metadid/reference/as_individual_pp.md),
[`as_summary_did()`](https://ben18785.github.io/metadid/reference/as_summary_did.md),
[`as_summary_rct()`](https://ben18785.github.io/metadid/reference/as_summary_rct.md),
[`as_summary_pp()`](https://ben18785.github.io/metadid/reference/as_summary_pp.md),
[`as_summary_did_change()`](https://ben18785.github.io/metadid/reference/as_summary_did_change.md)

## Examples

``` r
dat <- simulate_meta_did(n_studies = 10, true_effect = -0.15, seed = 42)
head(dat)
#> # A tibble: 6 × 5
#>   study_id subject_id group   time  value
#>   <chr>         <int> <chr>   <chr> <dbl>
#> 1 study_1           1 control pre   0.607
#> 2 study_1           1 control post  0.506
#> 3 study_1           2 control pre   0.724
#> 4 study_1           2 control post  0.578
#> 5 study_1           3 control pre   0.283
#> 6 study_1           3 control post  0.296
attr(dat, "true_params")
#> # A tibble: 10 × 4
#>    study_id   theta gamma baseline
#>    <chr>      <dbl> <dbl>    <dbl>
#>  1 study_1  -0.109  -0.02     0.45
#>  2 study_2  -0.167  -0.02     0.45
#>  3 study_3  -0.139  -0.02     0.45
#>  4 study_4  -0.131  -0.02     0.45
#>  5 study_5  -0.138  -0.02     0.45
#>  6 study_6  -0.153  -0.02     0.45
#>  7 study_7  -0.105  -0.02     0.45
#>  8 study_8  -0.153  -0.02     0.45
#>  9 study_9  -0.0894 -0.02     0.45
#> 10 study_10 -0.152  -0.02     0.45
```
