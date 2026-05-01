# Summarise a meta_did_fit object

Returns a data frame of posterior summaries for the population-level
treatment effect parameters and each study-level treatment effect.

## Usage

``` r
# S3 method for class 'meta_did_fit'
summary(object, prob = 0.9, ...)
```

## Arguments

- object:

  A `meta_did_fit` object.

- prob:

  Width of the credible interval. Default `0.9`.

- ...:

  Ignored.

## Value

A data frame with columns `parameter`, `mean`, `sd`, `lo`, `hi`.
