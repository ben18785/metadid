// rct_model_parameters.stan

// When normalised, we sample the "apparent" effect — what the RCT data directly
// measures: theta / (alpha + beta).  The true treatment effect on the alpha-
// normalised scale is derived in the transformed parameters block.
// When NOT normalised, we sample treatment_effect_rct directly.
// apparent_effect_rct is NOT non-centered (its prior is on the derived quantity).
vector[n_studies_rct * is_baseline_normalised * (1 - is_time_trend_rct_zero)] apparent_effect_rct;
vector[n_studies_rct * (1 - is_baseline_normalised * (1 - is_time_trend_rct_zero))] treatment_effect_rct_raw;

vector[n_studies_rct * (1 - is_time_trend_rct_zero)] time_trend_rct_raw;
// When normalised: control baseline is fixed at 1, not estimated.
// baseline_difference_rct_raw drives per-study imbalance under the unified
// baseline-difference machinery (DiD identifies it via hierarchical prior).
vector[n_studies_rct * (1 - is_baseline_normalised)] baseline_control_rct_raw;
vector[n_studies_rct * is_baseline_difference_estimated] baseline_difference_rct_raw;
vector<lower=0>[n_studies_rct] sigma_control_after_rct;
vector<lower=0>[n_studies_rct] sigma_treatment_after_rct;
