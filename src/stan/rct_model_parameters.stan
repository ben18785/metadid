// rct_model_parameters.stan

vector[n_studies_rct] treatment_effect_rct;
// When normalised: baseline and time_trend are fixed (b=1, tt=0), not estimated.
vector[n_studies_rct * (1 - is_baseline_normalised) * (1 - is_baseline_control_rct_zero)] baseline_control_rct;
vector[n_studies_rct * (1 - is_baseline_normalised) * (1 - is_baseline_treatment_rct_zero)] baseline_treatment_rct;
vector[n_studies_rct * (1 - is_baseline_normalised) * (1 - is_time_trend_rct_zero)] time_trend_rct;
vector<lower=0>[n_studies_rct] sigma_control_after_rct;
vector<lower=0>[n_studies_rct] sigma_treatment_after_rct;
