# Validate the assignment-mechanism columns

Checks the optional `randomisation` column (and its `cluster_size` /
`icc` companions) in either data source. Randomisation is deliberately
**never** inferred from `design`: a post-only study may be a randomised
trial or an unrandomised matched cohort, and a DiD study may be a
cluster-randomised roll-out. Treating `design == "rct"` as a
randomisation claim is exactly the error this column exists to prevent,
so an absent or `NA` value is read as `"none"` (imbalance estimated)
rather than as randomisation.

## Usage

``` r
validate_randomisation(summary_data, individual_data)
```

## Arguments

- summary_data:

  Summary-level data frame (or NULL).

- individual_data:

  Individual-level data frame (or NULL).

## Value

Invisible NULL. Stops with an error if validation fails.
