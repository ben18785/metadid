# Validate study-level multiplicative covariate(s)

Validates the `multiplicative_covariate` specification (a single column
name or a one-sided formula naming up to two columns, whose factors
multiply) and checks each named column. For every column: it must exist,
be categorical (numeric, logical, character, or factor; no `NA`s), be
constant within each study for individual-level data, and not also
appear in `covariate_names`. Numeric columns with more than five
distinct values are rejected as likely continuous. One multiplier is
estimated per non-reference level. Two identifiability checks are run
per column:

## Usage

``` r
validate_multiplicative_covariate(
  multiplicative_covariate,
  covariate_names,
  summary_data,
  individual_data
)
```

## Arguments

- multiplicative_covariate:

  `NULL`, a single column name, or a one-sided formula naming one or two
  columns (see
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)).

- covariate_names:

  Character vector of additive covariate names (or NULL).

- summary_data:

  Summary-level data frame (or NULL).

- individual_data:

  Individual-level data frame (or NULL).

## Value

Invisible NULL. Stops with an error or emits a warning if a validation
or identifiability check fails.

## Details

1.  The column must take at least two distinct levels across studies. If
    every study sits at the same level, the multiplier(s) and the
    population mean \\\mu\_\theta\\ are jointly unidentified — any
    (mean, multiplier) pair with the same product gives identical
    likelihood. This is a hard error.

2.  No non-reference level may be perfectly collinear with an additive
    covariate. For each non-reference level the indicator
    \\1\\x\_{\mathrm{mult}} = \mathrm{level}\\\\ is correlated with each
    additive covariate; when `|cor| > 0.95` that level's multiplier and
    the covariate's coefficient become weakly identified: the model
    still runs but `effect_multiplier[level]` and `beta_cov[k]` will be
    highly correlated in the posterior. This is a soft warning.
