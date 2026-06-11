// rct_summary_model_transformed_parameters.stan
//
// Reconstructs per-study RCT parameters from non-centered raws and (in
// modelled modes) from the per-study latent baseline.
//
// As in the DiD summary path:
//   * The likelihood operates on the **absolute** (user-units) scale; the
//     post-period observations are passed to Stan raw and the per-study
//     baselines are in absolute units.
//   * The hierarchical pooling layer in modelled modes lives on the
//     canonical fractional scale (fractions of treatment-pre baseline);
//     per-study θ_T, β_T, γ are bridged to absolute units at the likelihood
//     call site in rct_summary_model.stan.
//   * In none mode the canonical and absolute scales coincide, preserving
//     the existing absolute-scale behaviour of that mode.
//
// γ = baseline_difference uses the control-pre reference convention
// (b_T - b_C) / b_C. The derivation formulas are b_T = b_C * (1 + γ) and
// b_C = b_T / (1 + γ). The lower bound γ > -1 keeps (1 + γ) positive in
// either parameterisation.
//
// The legacy `apparent_effect` reparameterisation has been removed — it was
// only needed under the legacy plug-in normalised mode where the data
// itself was on the fractional scale. With per-study latent baselines and
// raw data flowing in, the absolute-scale parameterisation handles all
// cases uniformly.

vector[n_studies_rct_summary] treatment_effect_rct_summary;
vector[n_studies_rct_summary * (1 - is_time_trend_rct_summary_zero)] time_trend_rct_summary;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_rct_summary)
    treatment_effect_rct_summary[i] = mult_factor(has_multiplicative_covariate, effect_multiplier, x_mult_rct_summary[i]) * (treatment_effect_mean_rct + X_cov_rct_summary[i] * beta_cov) + treatment_effect_sd * treatment_effect_rct_summary_raw[i];
} else {
  treatment_effect_rct_summary = treatment_effect_rct_summary_raw;
}

if (!is_correlated_effects) {
  for (i in 1:size(time_trend_rct_summary_raw))
    time_trend_rct_summary[i] = time_trend_mean + time_trend_sd * time_trend_rct_summary_raw[i];
} else {
  time_trend_rct_summary = time_trend_rct_summary_raw;
}

// baseline_difference_rct_summary is sampled directly with <lower=-1>
// constraint (see rct_summary_model_parameters.stan).

// ---- Per-study absolute baselines -----------------------------------------
vector[n_studies_rct_summary] baseline_control_rct_summary;
vector[n_studies_rct_summary] baseline_treatment_rct_summary;

if (is_modelled) {
  if (is_modelled_treatment) {
    for (i in 1:n_studies_rct_summary) {
      baseline_treatment_rct_summary[i] = baseline_per_study_latent_rct_summary[i];
      if (is_baseline_difference_estimated) {
        baseline_control_rct_summary[i] = baseline_treatment_rct_summary[i] /
                                          (1 + baseline_difference_rct_summary[i]);
      } else {
        baseline_control_rct_summary[i] = baseline_treatment_rct_summary[i];
      }
    }
  } else {
    for (i in 1:n_studies_rct_summary) {
      baseline_control_rct_summary[i] = baseline_per_study_latent_rct_summary[i];
      if (is_baseline_difference_estimated) {
        baseline_treatment_rct_summary[i] = baseline_control_rct_summary[i] *
                                            (1 + baseline_difference_rct_summary[i]);
      } else {
        baseline_treatment_rct_summary[i] = baseline_control_rct_summary[i];
      }
    }
  }
} else {
  for (i in 1:n_studies_rct_summary) {
    baseline_control_rct_summary[i] = baseline_control_mean[1] + baseline_control_sd[1] * baseline_control_rct_summary_raw[i];
    if (is_baseline_difference_estimated) {
      baseline_treatment_rct_summary[i] = baseline_control_rct_summary[i] *
                                          (1 + baseline_difference_rct_summary[i]);
    } else {
      baseline_treatment_rct_summary[i] = baseline_control_rct_summary[i];
    }
  }
}
