# Specify an LKJ prior

Used for the correlation between treatment effects and time trends when
`correlated_effects = TRUE`. The LKJ distribution with concentration
parameter `eta` is placed on the Cholesky factor of the correlation
matrix. `eta = 1` is uniform over correlation matrices; `eta = 2` gently
regularises toward zero correlation.

## Usage

``` r
lkj(eta)
```

## Arguments

- eta:

  Concentration parameter (must be positive).

## Value

A `did_prior` object.
