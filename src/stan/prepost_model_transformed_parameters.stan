// prepost_model_transformed_parameters.stan
// Non-centered parameterization for study-level pre-post (individual) parameters.

vector[n_studies_pp] treatment_effect_pp;
vector[n_studies_pp * (1 - is_time_trend_pp_zero)] time_trend_pp;
vector[n_studies_pp * (1 - is_differenced_likelihood_pp) * (1 - is_baseline_normalised)] baseline_treatment_pp;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_pp)
    treatment_effect_pp[i] = mult_factor(has_multiplicative_covariate, gamma_mult, x_mult_pp[i]) * (treatment_effect_mean_pp + X_cov_pp[i] * beta_cov) + treatment_effect_sd * treatment_effect_pp_raw[i];
} else {
  treatment_effect_pp = treatment_effect_pp_raw;
}

if (!is_correlated_effects) {
  for (i in 1:size(time_trend_pp_raw))
    time_trend_pp[i] = time_trend_mean + time_trend_sd * time_trend_pp_raw[i];
} else {
  time_trend_pp = time_trend_pp_raw;
}

for (i in 1:size(baseline_treatment_pp_raw))
  baseline_treatment_pp[i] = baseline_treatment_mean[1] + baseline_treatment_sd[1] * baseline_treatment_pp_raw[i];
