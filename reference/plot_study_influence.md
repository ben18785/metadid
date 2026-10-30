# Plot per-study influence against weight

Weight on the horizontal axis, Pareto k on the vertical, with the
flagging threshold marked. The top-right corner carries the result and
disagrees with it; the top-left disagrees without carrying much weight
on the mean, which typically means it is acting on the between-study SD
instead.

## Usage

``` r
plot_study_influence(influence, k_threshold = 0.7)
```

## Arguments

- influence:

  The data frame from
  [`study_influence()`](https://ben18785.github.io/metadid/reference/study_influence.md).

- k_threshold:

  Where to draw the threshold line.

## Value

A ggplot2 object.

## See also

[`study_influence()`](https://ben18785.github.io/metadid/reference/study_influence.md)
