// rct_model_parameters.stan

vector[n_studies_rct] treatment_effect_rct_raw;
vector[n_studies_rct * (1 - is_time_trend_rct_zero)] time_trend_rct_raw;

// Non-centered raw for the hierarchical pop-level control baseline. Only
// present in none mode; in modelled modes the per-study baseline is the
// uniform-prior latent declared below.
vector[n_studies_rct * is_none_mode] baseline_control_rct_raw;

// Per-study latent baseline parameter for modelled normalisation modes.
// Sized > 0 only when is_modelled == 1. Holds b_T_pre[i] in treatment-latent
// mode and b_C_pre[i] in control-latent mode. For RCT studies neither pre
// baseline is directly observed (no pre period); the latent is informed
// jointly with the per-study θ, γ, and δ via the post-period observations
// and the hierarchical priors from the rest of the meta-analysis.
vector<lower=0, upper=baseline_prior_upper>[n_studies_rct * is_modelled] baseline_per_study_latent_rct;

// Per-study δ directly sampled with <lower=-1> constraint (avoids the
// (1 + δ) singularity in treatment-latent mode).
vector<lower=-1>[n_studies_rct * is_baseline_difference_estimated] baseline_difference_rct;
vector<lower=0>[n_studies_rct] sigma_control_after_rct;
vector<lower=0>[n_studies_rct] sigma_treatment_after_rct;
