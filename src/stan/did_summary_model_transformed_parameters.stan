// did_summary_model_transformed_parameters.stan
//
// Reconstructs per-study parameters from non-centered raws and (in modelled
// modes) from the per-study latent baseline.
//
// Two scales coexist:
//
//   * The **absolute** (user-units) scale, used by the likelihood. The
//     observed data (mean_pre_*, mean_post_*) are passed to Stan raw, the
//     per-study latent baselines (baseline_control_did_summary[i],
//     baseline_treatment_did_summary[i]) are in those same units, and the
//     likelihood compares raw observed means against raw expected means.
//
//   * The **canonical fractional** scale, used by the hierarchical pooling
//     layer in modelled modes. Per-study treatment_effect_did_summary[i],
//     time_trend_did_summary[i], and baseline_difference_did_summary[i]
//     are drawn from population-level priors and represent fractions of
//     the treatment-pre baseline (the canonical scale).
//
// The bridge from canonical to absolute happens at the likelihood call
// site in did_summary_model.stan: θ_abs = θ_norm * b_T_pre[i], etc. This
// keeps the data likelihood on its natural scale while making the
// hierarchical pooling target consistent with the user's chosen estimand.
// In none mode the canonical and absolute scales coincide (no baseline
// scaling), preserving the existing behaviour of that mode.

// ---- Canonical-scale per-study quantities ---------------------------------
// In modelled modes (is_modelled == 1), treatment_effect_did_summary[i] and
// time_trend_did_summary[i] are interpreted as fractions of the treatment-pre
// baseline (the canonical scale). In none mode they are on the absolute
// (user-units) scale, preserving the existing semantics for that mode.

vector[n_studies_did_summary] treatment_effect_did_summary;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary)] time_trend_did_summary;
vector[n_studies_did_change_only] treatment_effect_did_change_only;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_did_summary)
    treatment_effect_did_summary[i] = mult_factor(effect_multiplier, x_mult_did_summary[i]) * (treatment_effect_mean + X_cov_did_summary[i] * beta_cov) + treatment_effect_sd * treatment_effect_did_summary_raw[i];
  for (i in 1:n_studies_did_change_only)
    treatment_effect_did_change_only[i] = mult_factor(effect_multiplier, x_mult_did_change_only[i]) * (treatment_effect_mean + X_cov_did_change_only[i] * beta_cov) + treatment_effect_sd * treatment_effect_did_change_only_raw[i];
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

// baseline_difference_did_summary is now sampled directly as a parameter
// with a <lower=-1> constraint (see did_summary_model_parameters.stan); the
// non-centered raw + reconstruction has been removed.

// ---- Per-study absolute baselines -----------------------------------------
// baseline_control_did_summary[i] and baseline_treatment_did_summary[i] are
// the per-study control-pre and treatment-pre baselines on the **absolute**
// (user-units) scale. Always sized whenever the full (non-differenced)
// likelihood is in use; their source depends on baseline_latent_mode.
//
// γ = baseline_difference uses the control-pre reference convention
// (b_T - b_C) / b_C. The derivation formulas are:
//   b_T = b_C * (1 + γ)
//   b_C = b_T / (1 + γ)
// The lower bound γ > -1 (enforced at the parameter declaration in the
// parameters block) guarantees that both baselines remain positive in
// either parameterisation.

vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary)] baseline_control_did_summary;
vector[n_studies_did_summary * (1 - is_differenced_likelihood_did_summary)] baseline_treatment_did_summary;

if (is_modelled) {
  if (is_modelled_treatment) {
    // Treatment-latent: b_T_pre is the latent (uniform prior); b_C_pre derived
    // via division. The lower bound γ > -1 keeps the divisor (1 + γ) positive.
    for (i in 1:size(baseline_treatment_did_summary)) {
      baseline_treatment_did_summary[i] = baseline_per_study_latent_did_summary[i];
      baseline_control_did_summary[i]   = baseline_treatment_did_summary[i] /
                                          (1 + baseline_difference_did_summary[i]);
    }
  } else {
    // Control-latent: b_C_pre is the latent; b_T_pre derived via multiplication.
    for (i in 1:size(baseline_control_did_summary)) {
      baseline_control_did_summary[i]   = baseline_per_study_latent_did_summary[i];
      baseline_treatment_did_summary[i] = baseline_control_did_summary[i] *
                                          (1 + baseline_difference_did_summary[i]);
    }
  }
} else {
  // "none" mode: hierarchical pop-level control baseline; treatment baseline
  // derived from b_C via the imbalance under the legacy convention.
  for (i in 1:size(baseline_control_did_summary_raw))
    baseline_control_did_summary[i] = baseline_control_mean[1] + baseline_control_sd[1] * baseline_control_did_summary_raw[i];
  for (i in 1:size(baseline_treatment_did_summary))
    baseline_treatment_did_summary[i] = baseline_control_did_summary[i] *
                                        (1 + baseline_difference_did_summary[i]);
}
