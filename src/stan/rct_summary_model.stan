// rct_summary_model.stan

if(n_studies_rct_summary > 0) {

  // Effective time trend (zero when fixed).
  vector[n_studies_rct_summary] time_trend_rct_summary_eff;
  if (is_time_trend_rct_summary_zero)
    time_trend_rct_summary_eff = rep_vector(0.0, n_studies_rct_summary);
  else
    time_trend_rct_summary_eff = time_trend_rct_summary;

  // Precompute L_Sigma for joint prior (only used when correlated + time
  // trends estimated).
  matrix[2, 2] L_Sigma_rct_summary;
  if (is_correlated_effects && !is_time_trend_rct_summary_zero) {
    L_Sigma_rct_summary = diag_pre_multiply(
      [treatment_effect_sd, time_trend_sd]', L_corr_theta_beta[1]
    );
  }

  // Multiplicative-covariate factor per study (vector of 1s when feature off)
  vector[n_studies_rct_summary] mult_rct_summary;
  for (i in 1:n_studies_rct_summary)
    mult_rct_summary[i] = overall_mult(effect_multiplier, x_mult_rct_summary[i], effect_multiplier2, x_mult2_rct_summary[i]);

  for (i in 1:n_studies_rct_summary) {
    // Bridge from the canonical fractional scale (where the hierarchical
    // pooling lives) to the absolute scale (where the data and the
    // likelihood live). In modelled modes we multiply by the per-study
    // treatment-pre baseline to recover absolute units. In none mode the
    // canonical and absolute scales coincide.
    real bl_c   = baseline_control_rct_summary[i];
    real bl_t   = baseline_treatment_rct_summary[i];
    real te_abs = is_modelled
                  ? treatment_effect_rct_summary[i] * bl_t
                  : treatment_effect_rct_summary[i];
    real tt_abs = is_modelled
                  ? time_trend_rct_summary_eff[i] * bl_t
                  : time_trend_rct_summary_eff[i];

    target += rct_summary_study_lpdf_from_data(
      x_bar_control_after_rct_summary[i],
      x_bar_treatment_after_rct_summary[i],
      bl_c,
      bl_t,
      tt_abs,
      te_abs,
      sd_control_after_rct_summary[i],
      sd_treatment_after_rct_summary[i],
      sample_size_control_rct_summary[i],
      sample_size_treatment_rct_summary[i]
    );

    // Hierarchical prior on the per-study treatment effect (canonical
    // fractional scale in modelled modes, absolute in none mode).
    if (is_correlated_effects && !is_time_trend_rct_summary_zero) {
      target += multi_normal_cholesky_lpdf(
        [treatment_effect_rct_summary[i], time_trend_rct_summary[i]]' |
        [mult_rct_summary[i] * (treatment_effect_mean_rct + X_cov_rct_summary[i] * beta_cov), time_trend_mean]',
        L_Sigma_rct_summary
      );
    } else if (is_student_t_heterogeneity) {
      target += student_t_lpdf(treatment_effect_rct_summary[i] | nu_treatment_vec[1], mult_rct_summary[i] * (treatment_effect_mean_rct + X_cov_rct_summary[i] * beta_cov), treatment_effect_sd);
    }
    // Normal case: handled by treatment_effect_rct_summary_raw ~ std_normal()
    // below.
  }

  // Non-centered priors. In modelled modes the per-study latent baseline
  // has a uniform prior from its declared bounds, so no explicit ~
  // statement is needed for it.
  if (is_none_mode) {
    baseline_control_rct_summary_raw ~ std_normal();
  }
  if (is_baseline_difference_estimated) {
    baseline_difference_rct_summary ~ normal(baseline_difference_mean, baseline_difference_sd);
  }
  if (!is_time_trend_rct_summary_zero && !is_correlated_effects)
    time_trend_rct_summary_raw ~ std_normal();
  if (!is_student_t_heterogeneity && !(is_correlated_effects && !is_time_trend_rct_summary_zero))
    treatment_effect_rct_summary_raw ~ std_normal();
}
