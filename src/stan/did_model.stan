// did_model.stan

if(n_studies_did > 0) {
  // Construct effective baselines. DiD always estimates baseline_difference
  // per-study (data identifies it). When normalised: control = 1, treatment =
  // 1 + baseline_difference. When unnormalised: use the transformed parameters
  // baseline_control_did and baseline_treatment_did (which already incorporates
  // baseline_difference).
  vector[n_studies_did] baseline_control_did_eff;
  vector[n_studies_did] baseline_treatment_did_eff;
  if (is_baseline_normalised) {
    baseline_control_did_eff = rep_vector(1.0, n_studies_did);
    baseline_treatment_did_eff = rep_vector(1.0, n_studies_did) + baseline_difference_did;
  } else {
    baseline_control_did_eff = baseline_control_did;
    baseline_treatment_did_eff = baseline_treatment_did;
  }

  for (i in 1:n_studies_did) {
    target += did_study_lpdf_from_data(
      study_start_control_did[i], study_end_control_did[i],
      study_start_treatment_did[i], study_end_treatment_did[i],
      x_control_before_did,
      x_control_after_did,
      x_treatment_before_did,
      x_treatment_after_did,
      baseline_control_did_eff[i],
      baseline_treatment_did_eff[i],
      time_trend_did[i],
      treatment_effect_did[i],
      sigma_control_before_did[i],
      sigma_control_after_did[i],
      sigma_treatment_before_did[i],
      sigma_treatment_after_did[i],
      rho_did[i]
    );
  }

  // Hierarchical prior on rho via Fisher z-transform.
  // rho is the parameter; prior is on z = atanh(rho).
  // Jacobian: |dz/drho| = 1/(1 - rho^2), so log|J| = -log(1 - rho^2).
  if (is_correlation_coefficient_hierarchical) {
    for (i in 1:n_studies_did) {
      int n_i = sample_size_control_did[i] + sample_size_treatment_did[i];
      atanh(rho_did[i]) ~ normal(mu_z, sqrt(square(tau_z) + 1.0 / (n_i - 3)));
      target += -log1m(square(rho_did[i]));
    }
  }

  if (!is_baseline_normalised) {
    baseline_control_did_raw ~ std_normal();
  }
  baseline_difference_did_raw ~ std_normal();
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
