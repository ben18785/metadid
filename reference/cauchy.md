# Specify a half-Cauchy prior

Used for scale parameters. The distribution is implicitly half-Cauchy
(positive support only) when applied to a `<lower=0>` Stan parameter.

## Usage

``` r
cauchy(scale)
```

## Arguments

- scale:

  Scale parameter (must be positive).

## Value

A `did_prior` object.
