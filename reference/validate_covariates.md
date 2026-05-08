# Validate study-level covariates for meta-regression

Checks that covariate columns exist, are numeric, contain no `NA`
values, and (for individual-level data) are constant within each study.

## Usage

``` r
validate_covariates(covariate_names, summary_data, individual_data)
```

## Arguments

- covariate_names:

  Character vector of column names to use as covariates.

- summary_data:

  Summary-level data frame (or NULL).

- individual_data:

  Individual-level data frame (or NULL).

## Value

Invisible NULL. Stops with an error if validation fails.
