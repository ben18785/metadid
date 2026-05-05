// rct_model_parameters.stan

// When normalised, we sample the "apparent" effect — what the RCT data directly
// measures: theta / (alpha + beta).  The true treatment effect on the alpha-
// normalised scale is derived in the transformed parameters block.
// When NOT normalised, we sample treatment_effect_rct directly.
vector[n_studies_rct * is_baseline_normalised * (1 - is_time_trend_rct_zero)] apparent_effect_rct;
vector[n_studies_rct * (1 - is_baseline_normalised * (1 - is_time_trend_rct_zero))] treatment_effect_rct;

vector[n_studies_rct * (1 - is_time_trend_rct_zero)] time_trend_rct;
// When normalised: baselines are fixed at 1, not estimated.
vector[n_studies_rct * (1 - is_baseline_normalised)] baseline_control_rct;
vector[n_studies_rct * (1 - is_baseline_normalised) * (1 - is_baseline_control_equal_treatment_rct)] baseline_treatment_rct;
vector<lower=0>[n_studies_rct] sigma_control_after_rct;
vector<lower=0>[n_studies_rct] sigma_treatment_after_rct;
