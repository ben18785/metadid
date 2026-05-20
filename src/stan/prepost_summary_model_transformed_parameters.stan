// prepost_summary_model_transformed_parameters.stan
// Non-centered parameterization for study-level pre-post summary parameters.

vector[n_studies_pp_summary] treatment_effect_pp_summary;
vector[n_studies_pp_summary * (1 - is_time_trend_pp_summary_zero)] time_trend_pp_summary;
vector[n_studies_pp_summary * (1 - is_differenced_likelihood_pp_summary) * (1 - is_baseline_normalised)] baseline_treatment_pp_summary;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_pp_summary)
    treatment_effect_pp_summary[i] = mult_factor(has_multiplicative_covariate, gamma_mult, x_mult_pp_summary[i]) * (treatment_effect_mean_pp + X_cov_pp_summary[i] * beta_cov) + treatment_effect_sd * treatment_effect_pp_summary_raw[i];
} else {
  treatment_effect_pp_summary = treatment_effect_pp_summary_raw;
}

if (!is_correlated_effects) {
  for (i in 1:size(time_trend_pp_summary_raw))
    time_trend_pp_summary[i] = time_trend_mean + time_trend_sd * time_trend_pp_summary_raw[i];
} else {
  time_trend_pp_summary = time_trend_pp_summary_raw;
}

for (i in 1:size(baseline_treatment_pp_summary_raw))
  baseline_treatment_pp_summary[i] = baseline_treatment_mean[1] + baseline_treatment_sd[1] * baseline_treatment_pp_summary_raw[i];
