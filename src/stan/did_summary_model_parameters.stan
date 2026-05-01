// did_summary_model_parameters.stan

vector[n_studies_did_summary] treatment_effect_did_summary;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary)] time_trend_did_summary;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary) * (1 - is_baseline_normalised)] baseline_control_did_summary;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary) * (1 - is_baseline_normalised)] baseline_treatment_did_summary;

// Fisher z values for studies with missing rho
vector[n_rho_missing_did_summary] z_rho_missing_did_summary;

// Change-only study treatment effects
vector[n_studies_did_change_only] treatment_effect_did_change_only;

