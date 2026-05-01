
vector[n_studies_rct_summary] treatment_effect_rct_summary;
vector[n_studies_rct_summary] baseline_control_rct_summary;
// When normalised: baseline_treatment = baseline_control (randomisation), so not a parameter.
vector[n_studies_rct_summary * (1 - is_baseline_normalised) * (1 - is_baseline_control_equal_treatment_rct_summary)] baseline_treatment_rct_summary;
vector[n_studies_rct_summary * (1 - is_time_trend_rct_summary_zero) * (1 - is_differenced_likelihood_rct_summary)] time_trend_rct_summary;
