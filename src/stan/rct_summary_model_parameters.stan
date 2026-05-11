// rct_summary_model_parameters.stan

// Same reparameterisation as individual RCT: when normalised with time trends,
// sample apparent_effect (theta / (alpha + beta)); otherwise sample treatment_effect directly.
// apparent_effect_rct_summary is NOT non-centered (its prior is on the derived quantity).
vector[n_studies_rct_summary * is_baseline_normalised * (1 - is_time_trend_rct_summary_zero)] apparent_effect_rct_summary;
vector[n_studies_rct_summary * (1 - is_baseline_normalised * (1 - is_time_trend_rct_summary_zero))] treatment_effect_rct_summary_raw;

vector[n_studies_rct_summary * (1 - is_time_trend_rct_summary_zero)] time_trend_rct_summary_raw;
// When normalised: baselines are fixed at 1, not estimated.
vector[n_studies_rct_summary * (1 - is_baseline_normalised)] baseline_control_rct_summary_raw;
vector[n_studies_rct_summary * (1 - is_baseline_normalised) * (1 - is_baseline_control_equal_treatment_rct_summary)] baseline_treatment_rct_summary_raw;
