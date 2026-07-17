# Specify a log-normal prior

Used for strictly positive multiplicative factors (the
`multiplicative_covariate` effect multiplier). The prior is placed on
the natural logarithm of the parameter, so the parameter itself is
log-normally distributed with median `exp(meanlog)` and strictly
positive support. Unlike a normal prior truncated at zero, a log-normal
has no probability mass piling up against a hard boundary and is the
conventional choice for a ratio-scale quantity.

## Usage

``` r
lognormal(meanlog, sdlog)
```

## Arguments

- meanlog:

  Mean on the log scale. `meanlog = 0` gives a median of 1.

- sdlog:

  Standard deviation on the log scale (must be positive). Larger values
  allow the factor to depart further from 1 in either direction.

## Value

A `did_prior` object.
