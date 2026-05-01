# Validate summary-level study data

Checks that `data` meets the structural requirements for use in
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).
Called automatically inside
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md);
also useful for diagnosing problems before fitting.

## Usage

``` r
validate_summary_data(data)
```

## Arguments

- data:

  A data frame with one row per study. Must contain `study_id` and
  `design` columns. Valid designs:

  - `"did"`: DiD studies with separate pre/post summaries for both
    groups.

  - `"did_change"`: DiD studies reporting only change means and SDs.

  - `"rct"`: RCT studies with post-treatment summaries only.

  - `"pp"`: Pre-post studies with no control group.

  The `rho` column (pre-post correlation) is optional for `"did"` and
  `"pp"` designs; missing values trigger hierarchical imputation when
  `hierarchical_rho = TRUE`.

## Value

`data` invisibly if valid; otherwise stops with an informative error.
