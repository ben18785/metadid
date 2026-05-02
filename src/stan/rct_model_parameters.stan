// rct_model_parameters.stan

vector[n_studies_rct] treatment_effect_rct;
vector[n_studies_rct * (1 - is_time_trend_rct_zero)] time_trend_rct;
// When normalised: baselines are fixed at 1, not estimated.
vector[n_studies_rct * (1 - is_baseline_normalised)] baseline_control_rct;
vector[n_studies_rct * (1 - is_baseline_normalised) * (1 - is_baseline_control_equal_treatment_rct)] baseline_treatment_rct;
vector<lower=0>[n_studies_rct] sigma_control_after_rct;
vector<lower=0>[n_studies_rct] sigma_treatment_after_rct;
