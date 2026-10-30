# Plot the share of the evidence by study

Plot the share of the evidence by study

## Usage

``` r
plot_information_share(fit, top = 20L)
```

## Arguments

- fit:

  A
  [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  fit, or the data frame from
  [`information_share()`](https://ben18785.github.io/metadid/reference/information_share.md).

- top:

  Show only this many studies, pooling the rest. `NULL` shows all.

## Value

A ggplot2 object.

## See also

[`information_share()`](https://ben18785.github.io/metadid/reference/information_share.md)
