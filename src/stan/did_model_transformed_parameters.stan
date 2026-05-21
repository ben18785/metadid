// did_model_transformed_parameters.stan
//
// Reconstructs per-study DiD (individual-data) parameters from non-centered
// raws and (in modelled modes) from the per-study latent baseline.
// Same canonical/absolute split as the summary-data path:
//   * Likelihood operates on absolute (user-units) scale.
//   * Hierarchical pooling in modelled modes lives on the canonical
//     fractional scale (fractions of treatment-pre baseline); per-study
//     θ_T, γ_T, δ are bridged to absolute units at the likelihood call site.
//   * In none mode the canonical and absolute scales coincide.

vector[n_studies_did] treatment_effect_did;
vector[n_studies_did] time_trend_did;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_did)
    treatment_effect_did[i] = treatment_effect_mean + X_cov_did[i] * beta_cov + treatment_effect_sd * treatment_effect_did_raw[i];
} else {
  treatment_effect_did = treatment_effect_did_raw;
}

if (!is_correlated_effects) {
  for (i in 1:n_studies_did)
    time_trend_did[i] = time_trend_mean + time_trend_sd * time_trend_did_raw[i];
} else {
  time_trend_did = time_trend_did_raw;
}

// baseline_difference_did is sampled directly with <lower=-1> constraint
// (see did_model_parameters.stan).

// ---- Per-study absolute baselines -----------------------------------------
vector[n_studies_did] baseline_control_did;
vector[n_studies_did] baseline_treatment_did;

if (is_modelled) {
  if (is_modelled_treatment) {
    for (i in 1:n_studies_did) {
      baseline_treatment_did[i] = baseline_per_study_latent_did[i];
      baseline_control_did[i]   = baseline_treatment_did[i] /
                                  (1 + baseline_difference_did[i]);
    }
  } else {
    for (i in 1:n_studies_did) {
      baseline_control_did[i]   = baseline_per_study_latent_did[i];
      baseline_treatment_did[i] = baseline_control_did[i] *
                                  (1 + baseline_difference_did[i]);
    }
  }
} else {
  for (i in 1:n_studies_did) {
    baseline_control_did[i] = baseline_control_mean[1] + baseline_control_sd[1] * baseline_control_did_raw[i];
    baseline_treatment_did[i] = baseline_control_did[i] * (1 + baseline_difference_did[i]);
  }
}
