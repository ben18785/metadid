// rct_summary_model_transformed_parameters.stan
// Non-centered parameterization for study-level RCT summary parameters,
// plus derivation of treatment_effect from apparent_effect when normalised.

vector[n_studies_rct_summary * (1 - is_baseline_normalised * (1 - is_time_trend_rct_summary_zero))] treatment_effect_rct_summary;
vector[n_studies_rct_summary * (1 - is_time_trend_rct_summary_zero)] time_trend_rct_summary;
vector[n_studies_rct_summary * (1 - is_baseline_normalised)] baseline_control_rct_summary;
vector[n_studies_rct_summary * (1 - is_baseline_normalised) * (1 - is_baseline_control_equal_treatment_rct_summary)] baseline_treatment_rct_summary;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:size(treatment_effect_rct_summary_raw))
    treatment_effect_rct_summary[i] = treatment_effect_mean_rct + X_cov_rct_summary[i] * beta_cov + treatment_effect_sd * treatment_effect_rct_summary_raw[i];
} else {
  treatment_effect_rct_summary = treatment_effect_rct_summary_raw;
}

if (!is_correlated_effects) {
  for (i in 1:size(time_trend_rct_summary_raw))
    time_trend_rct_summary[i] = time_trend_mean + time_trend_sd * time_trend_rct_summary_raw[i];
} else {
  time_trend_rct_summary = time_trend_rct_summary_raw;
}

for (i in 1:size(baseline_control_rct_summary_raw))
  baseline_control_rct_summary[i] = baseline_control_mean[1] + baseline_control_sd[1] * baseline_control_rct_summary_raw[i];
for (i in 1:size(baseline_treatment_rct_summary_raw))
  baseline_treatment_rct_summary[i] = baseline_treatment_mean[1] + baseline_treatment_sd[1] * baseline_treatment_rct_summary_raw[i];

// Derive true treatment effect from apparent effect when normalised with time trends.
vector[n_studies_rct_summary] treatment_effect_rct_summary_derived;
if (is_baseline_normalised && !is_time_trend_rct_summary_zero) {
  for (i in 1:n_studies_rct_summary)
    treatment_effect_rct_summary_derived[i] = apparent_effect_rct_summary[i] * (1 + time_trend_rct_summary[i]);
} else {
  treatment_effect_rct_summary_derived = treatment_effect_rct_summary;
}
