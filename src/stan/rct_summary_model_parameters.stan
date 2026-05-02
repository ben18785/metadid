// rct_summary_model_parameters.stan

vector[n_studies_rct_summary] treatment_effect_rct_summary;
vector[n_studies_rct_summary * (1 - is_time_trend_rct_summary_zero)] time_trend_rct_summary;
// When normalised: baselines are fixed at 1, not estimated.
vector[n_studies_rct_summary * (1 - is_baseline_normalised)] baseline_control_rct_summary;
vector[n_studies_rct_summary * (1 - is_baseline_normalised) * (1 - is_baseline_control_equal_treatment_rct_summary)] baseline_treatment_rct_summary;
