// did_summary_model_parameters.stan

vector[n_studies_did_summary] treatment_effect_did_summary_raw;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary)] time_trend_did_summary_raw;

// Non-centered raw for the hierarchical pop-level control baseline. Only
// present in "none" mode; in modelled modes the per-study baseline is the
// uniform-prior latent declared below.
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary) * is_none_mode] baseline_control_did_summary_raw;

// Per-study baseline imbalance δ = (b_T - b_C) / b_C, directly sampled with
// a lower bound of -1 so that the derived control baseline in treatment-latent
// mode (b_C = b_T / (1 + δ)) cannot diverge through the singularity at δ = -1.
// The lower bound is functionally a hard physical constraint (negative
// baselines are not meaningful), and the bulk of the prior + likelihood mass
// sits well away from it, so the constraint doesn't bind in practice.
vector<lower=-1>[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary)] baseline_difference_did_summary;

// Per-study latent baseline parameter for the modelled normalisation modes.
// Sized > 0 only when is_modelled == 1. Holds b_T_pre[i] in treatment-latent
// mode (mode 1) and b_C_pre[i] in control-latent mode (mode 2). The "other"
// baseline is derived in transformed parameters via baseline_difference.
vector<lower=0, upper=baseline_prior_upper>[
  n_studies_did_summary * (1 - is_differenced_likelihood_did_summary) * is_modelled
] baseline_per_study_latent_did_summary;

// Fisher z values for studies with missing rho
vector[n_rho_missing_did_summary] z_rho_missing_did_summary;

// Change-only study treatment effects
vector[n_studies_did_change_only] treatment_effect_did_change_only_raw;

