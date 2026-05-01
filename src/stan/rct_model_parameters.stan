// rct_model.stan

vector[n_studies_rct] treatment_effect_rct;
// When normalised: baseline_control always exists (unknown pre-treatment level);
// when not normalised: conditional on is_baseline_control_rct_zero.
vector[n_studies_rct * (1 - (1 - is_baseline_normalised) * is_baseline_control_rct_zero)] baseline_control_rct;
// When normalised: baseline_treatment = baseline_control (randomisation), so not a parameter.
vector[n_studies_rct * (1 - is_baseline_normalised) * (1 - is_baseline_treatment_rct_zero)] baseline_treatment_rct;
vector[n_studies_rct * (1 - is_time_trend_rct_zero)] time_trend_rct;
vector<lower=0>[n_studies_rct] sigma_control_after_rct;
vector<lower=0>[n_studies_rct] sigma_treatment_after_rct;
