// rct_model_transformed_parameters.stan
//
// Reconstructs per-study RCT (individual-data) parameters from non-centered
// raws and (in modelled modes) from the per-study latent baseline.
//
// As in the summary-data paths:
//   * Likelihood operates on absolute scale.
//   * Hierarchical pooling in modelled modes is on the canonical fractional
//     scale; bridging happens at the likelihood call site.
//   * In none mode the canonical and absolute scales coincide.
//
// The legacy `apparent_effect_rct` reparameterisation has been removed; raw
// data with per-study latent baselines handles all cases uniformly.

vector[n_studies_rct] treatment_effect_rct;
vector[n_studies_rct * (1 - is_time_trend_rct_zero)] time_trend_rct;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_rct)
    treatment_effect_rct[i] = treatment_effect_mean_rct + X_cov_rct[i] * beta_cov + treatment_effect_sd * treatment_effect_rct_raw[i];
} else {
  treatment_effect_rct = treatment_effect_rct_raw;
}

if (!is_correlated_effects) {
  for (i in 1:size(time_trend_rct_raw))
    time_trend_rct[i] = time_trend_mean + time_trend_sd * time_trend_rct_raw[i];
} else {
  time_trend_rct = time_trend_rct_raw;
}

// baseline_difference_rct is sampled directly with <lower=-1> constraint
// (see rct_model_parameters.stan).

// ---- Per-study absolute baselines -----------------------------------------
vector[n_studies_rct] baseline_control_rct;
vector[n_studies_rct] baseline_treatment_rct;

if (is_modelled) {
  if (is_modelled_treatment) {
    for (i in 1:n_studies_rct) {
      baseline_treatment_rct[i] = baseline_per_study_latent_rct[i];
      if (is_baseline_difference_estimated) {
        baseline_control_rct[i] = baseline_treatment_rct[i] /
                                  (1 + baseline_difference_rct[i]);
      } else {
        baseline_control_rct[i] = baseline_treatment_rct[i];
      }
    }
  } else {
    for (i in 1:n_studies_rct) {
      baseline_control_rct[i] = baseline_per_study_latent_rct[i];
      if (is_baseline_difference_estimated) {
        baseline_treatment_rct[i] = baseline_control_rct[i] *
                                    (1 + baseline_difference_rct[i]);
      } else {
        baseline_treatment_rct[i] = baseline_control_rct[i];
      }
    }
  }
} else {
  for (i in 1:n_studies_rct) {
    baseline_control_rct[i] = baseline_control_mean[1] + baseline_control_sd[1] * baseline_control_rct_raw[i];
    if (is_baseline_difference_estimated) {
      baseline_treatment_rct[i] = baseline_control_rct[i] *
                                  (1 + baseline_difference_rct[i]);
    } else {
      baseline_treatment_rct[i] = baseline_control_rct[i];
    }
  }
}
