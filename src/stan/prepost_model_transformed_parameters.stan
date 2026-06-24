// prepost_model_transformed_parameters.stan
//
// Reconstructs per-study PP (individual-data) parameters. Same canonical /
// absolute split as the summary path: likelihood operates on absolute scale,
// hierarchical pooling on canonical fractional scale in modelled modes,
// pop-level baselines used in none mode.

vector[n_studies_pp] treatment_effect_pp;
vector[n_studies_pp * (1 - is_time_trend_pp_zero)] time_trend_pp;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_pp)
    treatment_effect_pp[i] = overall_mult(effect_multiplier, x_mult_pp[i], effect_multiplier2, x_mult2_pp[i]) * (treatment_effect_mean_pp + X_cov_pp[i] * beta_cov) + treatment_effect_sd * treatment_effect_pp_raw[i];
} else {
  treatment_effect_pp = treatment_effect_pp_raw;
}

if (!is_correlated_effects) {
  for (i in 1:size(time_trend_pp_raw))
    time_trend_pp[i] = time_trend_mean + time_trend_sd * time_trend_pp_raw[i];
} else {
  time_trend_pp = time_trend_pp_raw;
}

// ---- Per-study absolute treatment-pre baseline ----------------------------
// Sized whenever modelled mode is active OR none mode with the full
// (non-differenced) likelihood. The only combination this is unused is
// none + differenced, where the baseline cancels everywhere.
vector[n_studies_pp * (1 - is_differenced_likelihood_pp * is_none_mode)] baseline_treatment_pp;

if (is_modelled) {
  if (is_modelled_treatment) {
    for (i in 1:n_studies_pp)
      baseline_treatment_pp[i] = baseline_per_study_latent_pp[i];
  } else {
    // Control-latent for PP: latent is per-study b_C_pre; derive b_T_pre via
    // the pop-level baseline_difference. Under control-pre reference
    // convention: b_T = b_C * (1 + γ).
    for (i in 1:n_studies_pp)
      baseline_treatment_pp[i] = baseline_per_study_latent_pp[i] *
                                  (1 + baseline_difference_mean);
  }
} else {
  for (i in 1:size(baseline_treatment_pp_raw))
    baseline_treatment_pp[i] = baseline_treatment_mean[1] + baseline_treatment_sd[1] * baseline_treatment_pp_raw[i];
}
