// rct_summary_model_parameters.stan

// Per-study treatment effect (always sampled, non-centered).
// In modelled modes this is on the canonical fractional scale (fraction of
// treatment-pre baseline); in none mode it is on the absolute scale.
vector[n_studies_rct_summary] treatment_effect_rct_summary_raw;

// Per-study time trend (only sampled when time trends are estimated).
// Same scale convention as treatment_effect: canonical fractional in modelled
// modes, absolute in none mode.
vector[n_studies_rct_summary * (1 - is_time_trend_rct_summary_zero)] time_trend_rct_summary_raw;

// Non-centered raw for the hierarchical pop-level control baseline. Only
// present in none mode; in modelled modes the per-study baseline is the
// uniform-prior latent declared below.
vector[n_studies_rct_summary * is_none_mode] baseline_control_rct_summary_raw;

// Per-study latent baseline parameter for modelled normalisation modes.
// Sized > 0 only when is_modelled == 1. Holds b_T_pre[i] in treatment-latent
// mode and b_C_pre[i] in control-latent mode. The "other" baseline is derived
// in transformed parameters via baseline_difference.
// For RCT studies neither pre baseline is directly observed (no pre period);
// the latent is informed jointly with the per-study θ, β, and γ via the
// post-period observations and the hierarchical priors from the rest of the
// meta-analysis.
vector<lower=0, upper=baseline_prior_upper>[n_studies_rct_summary * is_modelled] baseline_per_study_latent_rct_summary;

// Per-study baseline difference (only sampled when imbalance is estimated).
// Per-study γ = (b_T - b_C) / b_C on the control-pre reference convention.
// Directly sampled with <lower=-1> constraint so the (1 + γ) factor stays
// positive in both parameterisations.
vector<lower=-1>[n_studies_rct_summary * is_baseline_difference_estimated] baseline_difference_rct_summary;
