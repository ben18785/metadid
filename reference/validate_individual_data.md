# Validate individual-level study data

Checks that `data` meets the structural requirements for use in
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md).
Called automatically inside
[`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md);
also useful for diagnosing problems before fitting.

## Usage

``` r
validate_individual_data(data)
```

## Arguments

- data:

  A data frame in long format with one row per observation. Required
  columns: `study_id`, `design`, `group`, `time`, `value`. For
  repeated-measures designs (`"did"` and `"pp"`), a `subject_id` column
  is also required to correctly pair pre and post observations. Valid
  designs: `"did"`, `"rct"`, `"pp"`.

## Value

`data` invisibly if valid; otherwise stops with an informative error.
