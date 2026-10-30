# Package index

## Model fitting

Fit the hierarchical Bayesian meta-analysis model.

- [`meta_did()`](https://ben18785.github.io/metadid/reference/meta_did.md)
  : Fit a Bayesian meta-analysis model across study designs
- [`meta_did_general()`](https://ben18785.github.io/metadid/reference/meta_did_general.md)
  : Fit a meta-analysis model with explicit control over design
  assumptions
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
- [`lkj()`](https://ben18785.github.io/metadid/reference/lkj.md) :
  Specify an LKJ prior
- [`lognormal()`](https://ben18785.github.io/metadid/reference/lognormal.md)
  : Specify a log-normal prior

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

## Sensitivity analysis

Probe the assumptions that license pooling across designs, which the
data cannot check: that pre-post studies share a trend distribution with
DiD studies, and that all designs estimate one effect.

- [`sensitivity_trend()`](https://ben18785.github.io/metadid/reference/sensitivity_trend.md)
  : Sensitivity of the pooled effect to trend exchangeability
- [`plot_sensitivity_trend()`](https://ben18785.github.io/metadid/reference/plot_sensitivity_trend.md)
  : Plot a trend-exchangeability sweep
- [`omega_pp()`](https://ben18785.github.io/metadid/reference/omega_pp.md)
  : Pre-post share of the information about the pooled effect
- [`delta_star()`](https://ben18785.github.io/metadid/reference/delta_star.md)
  : Breakdown value for trend exchangeability
- [`design_agreement()`](https://ben18785.github.io/metadid/reference/design_agreement.md)
  : Test whether the designs agree
- [`plot_design_agreement()`](https://ben18785.github.io/metadid/reference/plot_design_agreement.md)
  : Plot design offsets

## Influence diagnostics

Which studies the result rests on, and which would change it. Weight
governs the pull on the pooled mean; a discordant study acts on the
between-study SD instead, so the two are reported separately.

- [`information_share()`](https://ben18785.github.io/metadid/reference/information_share.md)
  : Share of the evidence each study and design contributes
- [`plot_information_share()`](https://ben18785.github.io/metadid/reference/plot_information_share.md)
  : Plot the share of the evidence by study
- [`study_influence()`](https://ben18785.github.io/metadid/reference/study_influence.md)
  : Per-study influence on the pooled result
- [`plot_study_influence()`](https://ben18785.github.io/metadid/reference/plot_study_influence.md)
  : Plot per-study influence against weight

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
