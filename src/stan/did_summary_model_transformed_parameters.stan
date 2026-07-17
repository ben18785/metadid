// did_summary_model_transformed_parameters.stan
// Non-centered parameterization for study-level DiD summary parameters.

vector[n_studies_did_summary] treatment_effect_did_summary;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary)] time_trend_did_summary;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary) * (1 - is_baseline_normalised)] baseline_control_did_summary;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary)] baseline_difference_did_summary;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary) * (1 - is_baseline_normalised)] baseline_treatment_did_summary;
vector[n_studies_did_change_only] treatment_effect_did_change_only;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_did_summary)
    treatment_effect_did_summary[i] = overall_mult(effect_multiplier, x_mult_did_summary[i], effect_multiplier2, x_mult2_did_summary[i]) * (treatment_effect_mean + X_cov_did_summary[i] * beta_cov) + treatment_effect_sd * treatment_effect_did_summary_raw[i];
  for (i in 1:n_studies_did_change_only)
    treatment_effect_did_change_only[i] = overall_mult(effect_multiplier, x_mult_did_change_only[i], effect_multiplier2, x_mult2_did_change_only[i]) * (treatment_effect_mean + X_cov_did_change_only[i] * beta_cov) + treatment_effect_sd * treatment_effect_did_change_only_raw[i];
} else {
  treatment_effect_did_summary = treatment_effect_did_summary_raw;
  treatment_effect_did_change_only = treatment_effect_did_change_only_raw;
}

if (!is_correlated_effects) {
  for (i in 1:size(time_trend_did_summary_raw))
    time_trend_did_summary[i] = time_trend_mean + time_trend_sd * time_trend_did_summary_raw[i];
} else {
  time_trend_did_summary = time_trend_did_summary_raw;
}

for (i in 1:size(baseline_control_did_summary_raw))
  baseline_control_did_summary[i] = baseline_control_mean[1] + baseline_control_sd[1] * baseline_control_did_summary_raw[i];

for (i in 1:size(baseline_difference_did_summary_raw))
  baseline_difference_did_summary[i] = baseline_difference_mean + baseline_difference_sd * baseline_difference_did_summary_raw[i];

// baseline_treatment_did_summary exists only in unnormalised, non-differenced mode
// and always uses the hierarchical baseline_difference (DiD identifies it from data).
for (i in 1:size(baseline_treatment_did_summary))
  baseline_treatment_did_summary[i] = baseline_control_did_summary[i] * (1 + baseline_difference_did_summary[i]);
