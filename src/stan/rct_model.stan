// rct_model.stan

if(n_studies_rct > 0) {
  // Effective time trend (zero when fixed).
  vector[n_studies_rct] time_trend_rct_eff;
  if (is_time_trend_rct_zero)
    time_trend_rct_eff = rep_vector(0.0, n_studies_rct);
  else
    time_trend_rct_eff = time_trend_rct;

  // Precompute L_Sigma for joint prior (only used when correlated + time
  // trends estimated).
  matrix[2, 2] L_Sigma_rct;
  if (is_correlated_effects && !is_time_trend_rct_zero) {
    L_Sigma_rct = diag_pre_multiply(
      [treatment_effect_sd, time_trend_sd]', L_corr_theta_beta[1]
    );
  }

  // Multiplicative-covariate factor per study (vector of 1s when feature off)
  vector[n_studies_rct] mult_rct;
  for (i in 1:n_studies_rct)
    mult_rct[i] = mult_factor(has_multiplicative_covariate, effect_multiplier, x_mult_rct[i]);

  for (i in 1:n_studies_rct) {
    // Bridge canonical fractional scale → absolute scale at the likelihood
    // call site. In modelled modes scale per-study θ and γ by the per-study
    // treatment-pre baseline; in none mode pass through.
    real bl_c   = baseline_control_rct[i];
    real bl_t   = baseline_treatment_rct[i];
    real te_abs = is_modelled
                  ? treatment_effect_rct[i] * bl_t
                  : treatment_effect_rct[i];
    real tt_abs = is_modelled
                  ? time_trend_rct_eff[i] * bl_t
                  : time_trend_rct_eff[i];

    target += rct_study_lpdf_from_data(
      study_start_control_rct[i], study_end_control_rct[i],
      study_start_treatment_rct[i], study_end_treatment_rct[i],
      x_control_after_rct,
      x_treatment_after_rct,
      bl_c,
      bl_t,
      tt_abs,
      te_abs,
      sigma_control_after_rct[i],
      sigma_treatment_after_rct[i]
    );

    // Hierarchical prior on the per-study treatment effect (canonical
    // fractional scale in modelled modes, absolute in none mode).
    if (is_correlated_effects && !is_time_trend_rct_zero) {
      target += multi_normal_cholesky_lpdf(
        [treatment_effect_rct[i], time_trend_rct[i]]' |
        [mult_rct[i] * (treatment_effect_mean_rct + X_cov_rct[i] * beta_cov), time_trend_mean]',
        L_Sigma_rct
      );
    } else if (is_student_t_heterogeneity) {
      target += student_t_lpdf(treatment_effect_rct[i] | nu_treatment_vec[1], mult_rct[i] * (treatment_effect_mean_rct + X_cov_rct[i] * beta_cov), treatment_effect_sd);
    }
    // Normal case: handled by treatment_effect_rct_raw ~ std_normal() below.
  }

  if (is_none_mode) {
    baseline_control_rct_raw ~ std_normal();
  }
  if (is_baseline_difference_estimated) {
    baseline_difference_rct ~ normal(baseline_difference_mean, baseline_difference_sd);
  }
  if (!is_time_trend_rct_zero && !is_correlated_effects)
    time_trend_rct_raw ~ std_normal();
  if (!is_student_t_heterogeneity && !(is_correlated_effects && !is_time_trend_rct_zero))
    treatment_effect_rct_raw ~ std_normal();
  sigma_control_after_rct ~ cauchy(0, sigma_prior_scale);
  sigma_treatment_after_rct ~ cauchy(0, sigma_prior_scale);
}
