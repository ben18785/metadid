// did_model.stan

if(n_studies_did > 0) {
  for (i in 1:n_studies_did) {
    // Bridge from canonical fractional scale to absolute. In modelled modes
    // scale per-study θ and γ by the per-study treatment-pre baseline; in
    // none mode pass them through (already absolute).
    real bl_c   = baseline_control_did[i];
    real bl_t   = baseline_treatment_did[i];
    real te_abs = is_modelled
                  ? treatment_effect_did[i] * bl_t
                  : treatment_effect_did[i];
    real tt_abs = is_modelled
                  ? time_trend_did[i] * bl_t
                  : time_trend_did[i];

    target += did_study_lpdf_from_data(
      study_start_control_did[i], study_end_control_did[i],
      study_start_treatment_did[i], study_end_treatment_did[i],
      x_control_before_did,
      x_control_after_did,
      x_treatment_before_did,
      x_treatment_after_did,
      bl_c,
      bl_t,
      tt_abs,
      te_abs,
      sigma_control_before_did[i],
      sigma_control_after_did[i],
      sigma_treatment_before_did[i],
      sigma_treatment_after_did[i],
      rho_did[i]
    );
  }

  // Hierarchical prior on rho via Fisher z-transform.
  if (is_correlation_coefficient_hierarchical) {
    for (i in 1:n_studies_did) {
      int n_i = sample_size_control_did[i] + sample_size_treatment_did[i];
      atanh(rho_did[i]) ~ normal(mu_z, sqrt(square(tau_z) + 1.0 / (n_i - 3)));
      target += -log1m(square(rho_did[i]));
    }
  }

  if (is_none_mode) {
    baseline_control_did_raw ~ std_normal();
  }
  baseline_difference_did ~ normal(baseline_difference_mean, baseline_difference_sd);
  sigma_control_before_did ~ cauchy(0, sigma_prior_scale);
  sigma_control_after_did ~ cauchy(0, sigma_prior_scale);
  sigma_treatment_before_did ~ cauchy(0, sigma_prior_scale);
  sigma_treatment_after_did ~ cauchy(0, sigma_prior_scale);
  vector[n_studies_did] mult_did;
  for (i in 1:n_studies_did)
    mult_did[i] = overall_mult(effect_multiplier, x_mult_did[i], effect_multiplier2, x_mult2_did[i]);
  if (is_correlated_effects) {
    matrix[2, 2] L_Sigma_did = diag_pre_multiply(
      [treatment_effect_sd, time_trend_sd]', L_corr_theta_beta[1]
    );
    for (i in 1:n_studies_did) {
      target += multi_normal_cholesky_lpdf(
        [treatment_effect_did[i], time_trend_did[i]]' |
        [mult_did[i] * (treatment_effect_mean + X_cov_did[i] * beta_cov), time_trend_mean]',
        L_Sigma_did
      );
    }
  } else {
    time_trend_did_raw ~ std_normal();
    if (is_student_t_heterogeneity) {
      treatment_effect_did ~ student_t(nu_treatment_vec[1], mult_did .* (treatment_effect_mean + X_cov_did * beta_cov), treatment_effect_sd);
    } else {
      treatment_effect_did_raw ~ std_normal();
    }
  }
}
