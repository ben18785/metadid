# Plot a trend-exchangeability sweep

The tipping-point curve: the pooled effect against \\\Delta\\, the
systematic difference between the pre-post and DiD trend distributions,
with the interval that the sweep refitted at each point. The dashed
horizontal line is no effect; where the ribbon first touches it is the
shift at which the conclusion stops holding.

## Usage

``` r
plot_sensitivity_trend(sweep)
```

## Arguments

- sweep:

  The object returned by
  [`sensitivity_trend()`](https://ben18785.github.io/metadid/reference/sensitivity_trend.md).

## Value

A ggplot2 object.

## See also

[`sensitivity_trend()`](https://ben18785.github.io/metadid/reference/sensitivity_trend.md)
