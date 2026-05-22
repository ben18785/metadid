// did_model_parameters.stan

vector[n_studies_did] treatment_effect_did_raw;

// Non-centered raw for the hierarchical pop-level control baseline. Only
// present in none mode; in modelled modes the per-study baseline is the
// uniform-prior latent declared below.
vector[n_studies_did * is_none_mode] baseline_control_did_raw;

// Per-study γ = (b_T - b_C) / b_C on the control-pre reference convention.
// Directly sampled with <lower=-1> constraint so the derived (1 + γ) factor
// stays positive. See did_summary equivalent for full rationale.
vector<lower=-1>[n_studies_did] baseline_difference_did;
vector[n_studies_did] time_trend_did_raw;

// Per-study latent baseline parameter for modelled normalisation modes.
// Sized > 0 only when is_modelled == 1. Holds b_T_pre[i] in treatment-latent
// mode and b_C_pre[i] in control-latent mode. The "other" baseline is
// derived in transformed parameters via baseline_difference.
vector<lower=0, upper=baseline_prior_upper>[n_studies_did * is_modelled] baseline_per_study_latent_did;

vector<lower=-1, upper=1>[n_studies_did] rho_did;
vector<lower=0>[n_studies_did] sigma_control_before_did;
vector<lower=0>[n_studies_did] sigma_control_after_did;
vector<lower=0>[n_studies_did] sigma_treatment_before_did;
vector<lower=0>[n_studies_did] sigma_treatment_after_did;
