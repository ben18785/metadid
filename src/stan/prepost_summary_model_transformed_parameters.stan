// prepost_summary_model_transformed_parameters.stan
//
// Reconstructs per-study PP parameters from non-centered raws and (in
// modelled modes) from the per-study latent baseline.
//
// As in the DiD / RCT summary paths:
//   * The likelihood operates on the **absolute** (user-units) scale; the
//     pre and post observations are passed to Stan raw and the per-study
//     treatment-pre baseline is in absolute units.
//   * The hierarchical pooling layer in modelled modes lives on the
//     canonical fractional scale (fractions of treatment-pre baseline);
//     per-study θ_T, β_T are bridged to absolute units at the likelihood
//     call site in prepost_summary_model.stan.
//   * In none mode the canonical and absolute scales coincide, preserving
//     the existing absolute-scale behaviour of that mode.
//
// PP has no control arm, so neither b_C_pre nor the per-study baseline
// imbalance γ is identified from PP data alone. They are pulled from the
// hierarchical priors that DiD studies (and any other studies estimating
// γ) populate in the same fit.

vector[n_studies_pp_summary] treatment_effect_pp_summary;
vector[n_studies_pp_summary * (1 - is_time_trend_pp_summary_zero)] time_trend_pp_summary;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_pp_summary)
    treatment_effect_pp_summary[i] = treatment_effect_mean_pp + X_cov_pp_summary[i] * beta_cov + treatment_effect_sd * treatment_effect_pp_summary_raw[i];
} else {
  treatment_effect_pp_summary = treatment_effect_pp_summary_raw;
}

if (!is_correlated_effects) {
  for (i in 1:size(time_trend_pp_summary_raw))
    time_trend_pp_summary[i] = time_trend_mean + time_trend_sd * time_trend_pp_summary_raw[i];
} else {
  time_trend_pp_summary = time_trend_pp_summary_raw;
}

// ---- Per-study absolute treatment-pre baseline ----------------------------
// Sized whenever modelled mode is active OR none mode with the full
// (non-differenced) likelihood. The only case this is unused is none mode
// + differenced likelihood, where the baseline cancels everywhere.
//
// Critically, this is sized > 0 in modelled mode REGARDLESS of the
// likelihood form, because in modelled mode the per-study θ_T and β_T are
// on the canonical fractional scale and need to be multiplied by the
// per-study baseline to enter the (absolute-scale) likelihood — even the
// differenced likelihood needs the multiplication, despite cancelling
// the baseline from its mean structure.
//
// PP only ever sees the treatment-arm pre-period observation. In
// treatment-latent mode the latent is b_T_pre directly (informed by
// x_bar_treatment_before via an explicit likelihood term in
// prepost_summary_model.stan). In control-latent mode the latent is the
// per-study b_C_pre, with b_T_pre derived via the hierarchical
// baseline_difference (PP can't identify γ from its own data, so the
// latent's posterior is hierarchy-driven in that branch).

vector[n_studies_pp_summary * (1 - is_differenced_likelihood_pp_summary * is_none_mode)] baseline_treatment_pp_summary;

if (is_modelled) {
  if (is_modelled_treatment) {
    // Treatment-latent: latent is b_T_pre directly.
    for (i in 1:n_studies_pp_summary)
      baseline_treatment_pp_summary[i] = baseline_per_study_latent_pp_summary[i];
  } else {
    // Control-latent: latent is per-study b_C_pre; derive b_T_pre via the
    // pop-level baseline_difference (no per-study γ for PP).
    // Under control-pre reference convention: b_T = b_C * (1 + γ).
    for (i in 1:n_studies_pp_summary)
      baseline_treatment_pp_summary[i] = baseline_per_study_latent_pp_summary[i] *
                                          (1 + baseline_difference_mean);
  }
} else {
  // None mode + non-differenced: hierarchical pop-level treatment baseline.
  for (i in 1:size(baseline_treatment_pp_summary_raw))
    baseline_treatment_pp_summary[i] = baseline_treatment_mean[1] + baseline_treatment_sd[1] * baseline_treatment_pp_summary_raw[i];
}
