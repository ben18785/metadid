// rct_summary_model_parameters.stan

// Same reparameterisation as individual RCT: when normalised with time trends,
// sample apparent_effect (theta / (alpha + beta)); otherwise sample treatment_effect directly.
// apparent_effect_rct_summary is NOT non-centered (its prior is on the derived quantity).
vector[n_studies_rct_summary * is_baseline_normalised * (1 - is_time_trend_rct_summary_zero)] apparent_effect_rct_summary;
vector[n_studies_rct_summary * (1 - is_baseline_normalised * (1 - is_time_trend_rct_summary_zero))] treatment_effect_rct_summary_raw;

vector[n_studies_rct_summary * (1 - is_time_trend_rct_summary_zero)] time_trend_rct_summary_raw;
// When normalised: control baseline is fixed at 1, not estimated.
// baseline_difference_rct_summary_raw drives per-study imbalance under the unified
// baseline-difference machinery (DiD identifies it via hierarchical prior).
vector[n_studies_rct_summary * (1 - is_baseline_normalised)] baseline_control_rct_summary_raw;
vector[n_studies_rct_summary * is_baseline_difference_estimated] baseline_difference_rct_summary_raw;
