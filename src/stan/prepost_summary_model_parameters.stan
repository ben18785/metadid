// prepost_summary_model_parameters.stan

vector[n_studies_pp_summary] treatment_effect_pp_summary;
vector[n_studies_pp_summary * (1 - is_time_trend_pp_summary_zero)] time_trend_pp_summary;
vector[n_studies_pp_summary * (1 - is_differenced_likelihood_pp_summary) * (1 - is_baseline_normalised)] baseline_treatment_pp_summary;

// Fisher z values for studies with missing rho
vector[n_rho_missing_pp_summary] z_rho_missing_pp_summary;

