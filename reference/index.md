# Package index

## Model fitting

Fit the hierarchical Bayesian meta-analysis model.

- [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  : Fit a Bayesian meta-analysis model across study designs
- [`set_priors()`](https://ben18785.github.io/metadid/reference/set_priors.md)
  : Set prior distributions for the meta-analysis model

## Prior distributions

Specify prior distributions for model parameters.

- [`normal()`](https://ben18785.github.io/metadid/reference/normal.md) :
  Specify a normal prior
- [`cauchy()`](https://ben18785.github.io/metadid/reference/cauchy.md) :
  Specify a half-Cauchy prior
- [`gamma()`](https://ben18785.github.io/metadid/reference/gamma.md) :
  Specify a gamma prior

## Results

Summarise and inspect fitted models.

- [`print(`*`<meta_did_fit>`*`)`](https://ben18785.github.io/metadid/reference/print.meta_did_fit.md)
  : Print a meta_did_fit object
- [`summary(`*`<meta_did_fit>`*`)`](https://ben18785.github.io/metadid/reference/summary.meta_did_fit.md)
  : Summarise a meta_did_fit object
- [`tidy.meta_did_fit()`](https://ben18785.github.io/metadid/reference/tidy.meta_did_fit.md)
  : Tidy a meta_did_fit object

## Posterior predictive checks

Diagnose model fit via posterior predictive comparisons.

- [`pp_check_effects()`](https://ben18785.github.io/metadid/reference/pp_check_effects.md)
  : Posterior predictive check: study-level treatment effects
- [`pp_check_cdf()`](https://ben18785.github.io/metadid/reference/pp_check_cdf.md)
  : Posterior predictive check: CDF comparison

## Simulation

Simulate data from the hierarchical DiD model.

- [`simulate_meta_did()`](https://ben18785.github.io/metadid/reference/simulate_meta_did.md)
  : Simulate individual-level DiD data for a collection of studies

## Design extractors

Convert simulated individual-level data to the format expected by
different study designs, at either the individual or summary level.

- [`as_individual_did()`](https://ben18785.github.io/metadid/reference/as_individual_did.md)
  : Extract full individual-level DiD data for use with meta_did()
- [`as_individual_pp()`](https://ben18785.github.io/metadid/reference/as_individual_pp.md)
  : Extract treatment-arm individual-level pre-post data for use with
  meta_did()
- [`as_individual_rct()`](https://ben18785.github.io/metadid/reference/as_individual_rct.md)
  : Extract post-only individual-level RCT data for use with meta_did()
- [`as_summary_did()`](https://ben18785.github.io/metadid/reference/as_summary_did.md)
  : Summarise simulated DiD data to study-level statistics
- [`as_summary_did_change()`](https://ben18785.github.io/metadid/reference/as_summary_did_change.md)
  : Summarise simulated DiD data as change-score statistics
- [`as_summary_pp()`](https://ben18785.github.io/metadid/reference/as_summary_pp.md)
  : Summarise simulated DiD data as pre-post (treatment arm only)
  statistics
- [`as_summary_rct()`](https://ben18785.github.io/metadid/reference/as_summary_rct.md)
  : Summarise simulated DiD data as RCT-style post-only statistics

## Validation

Validate data inputs before fitting.

- [`validate_summary_data()`](https://ben18785.github.io/metadid/reference/validate_summary_data.md)
  : Validate summary-level study data
- [`validate_individual_data()`](https://ben18785.github.io/metadid/reference/validate_individual_data.md)
  : Validate individual-level study data
