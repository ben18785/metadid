// rct_model.stan

if(n_studies_rct > 0) {
  // Construct effective baselines.
  // When normalised: control = 1; treatment = 1 + baseline_difference (if estimated) else 1.
  // When unnormalised: treatment baseline is the transformed parameter
  // baseline_treatment_rct, which itself is baseline_control * (1 + baseline_difference)
  // when estimated, else baseline_control.
  vector[n_studies_rct] baseline_control_rct_eff;
  vector[n_studies_rct] baseline_treatment_rct_eff;
  if (is_baseline_normalised) {
    baseline_control_rct_eff = rep_vector(1.0, n_studies_rct);
    if (is_baseline_difference_estimated)
      baseline_treatment_rct_eff = rep_vector(1.0, n_studies_rct) + baseline_difference_rct;
    else
      baseline_treatment_rct_eff = rep_vector(1.0, n_studies_rct);
  } else {
    baseline_control_rct_eff = baseline_control_rct;
    baseline_treatment_rct_eff = baseline_treatment_rct;
  }

  // Construct effective time trends (zero when flag is set)
  vector[n_studies_rct] time_trend_rct_eff;
  if (is_time_trend_rct_zero)
    time_trend_rct_eff = rep_vector(0.0, n_studies_rct);
  else
    time_trend_rct_eff = time_trend_rct;

  // Precompute L_Sigma for joint prior (only used when correlated + time trends estimated)
  matrix[2, 2] L_Sigma_rct;
  if (is_correlated_effects && !is_time_trend_rct_zero) {
    L_Sigma_rct = diag_pre_multiply(
      [treatment_effect_sd, time_trend_sd]', L_corr_theta_beta[1]
    );
  }

  // Multiplicative-covariate factor per study (vector of 1s when feature off)
  vector[n_studies_rct] mult_rct;
  if (has_multiplicative_covariate) {
    for (i in 1:n_studies_rct) mult_rct[i] = pow(gamma_mult[1], x_mult_rct[i]);
  } else {
    mult_rct = rep_vector(1.0, n_studies_rct);
  }

  for (i in 1:n_studies_rct) {
    if (is_baseline_normalised && !is_time_trend_rct_zero) {
      // Reparameterised: apparent_effect_rct[i] is theta/(alpha+beta),
      // which the normalised data directly measures.
      target += rct_study_normalised_lpdf_from_data(
        study_start_control_rct[i], study_end_control_rct[i],
        study_start_treatment_rct[i], study_end_treatment_rct[i],
        x_control_after_rct,
        x_treatment_after_rct,
        apparent_effect_rct[i],
        sigma_control_after_rct[i],
        sigma_treatment_after_rct[i]
      );

      // Hierarchical prior on the derived true treatment effect (stays centered;
      // treatment_effect_rct_derived is a nonlinear function of apparent_effect).
      if (is_correlated_effects) {
        target += multi_normal_cholesky_lpdf(
          [treatment_effect_rct_derived[i], time_trend_rct[i]]' |
          [mult_rct[i] * (treatment_effect_mean_rct + X_cov_rct[i] * beta_cov), time_trend_mean]',
          L_Sigma_rct
        );
      } else if (is_student_t_heterogeneity) {
        target += student_t_lpdf(treatment_effect_rct_derived[i] | nu_treatment_vec[1], mult_rct[i] * (treatment_effect_mean_rct + X_cov_rct[i] * beta_cov), treatment_effect_sd);
      } else {
        target += normal_lpdf(treatment_effect_rct_derived[i] | mult_rct[i] * (treatment_effect_mean_rct + X_cov_rct[i] * beta_cov), treatment_effect_sd);
      }

      // Jacobian: |d(te)/d(apparent)| = |1 + time_trend|
      target += log(abs(1 + time_trend_rct_eff[i]));

    } else {
      // Unnormalised or time trends forced to zero: treatment_effect_rct[i]
      // is sampled directly (non-centered via treatment_effect_rct_raw).
      target += rct_study_lpdf_from_data(
        study_start_control_rct[i], study_end_control_rct[i],
        study_start_treatment_rct[i], study_end_treatment_rct[i],
        x_control_after_rct,
        x_treatment_after_rct,
        baseline_control_rct_eff[i],
        baseline_treatment_rct_eff[i],
        time_trend_rct_eff[i],
        treatment_effect_rct[i],
        sigma_control_after_rct[i],
        sigma_treatment_after_rct[i]
      );

      // Hierarchical prior on treatment effect (student-t and correlated stay centered)
      if (is_correlated_effects && !is_time_trend_rct_zero) {
        target += multi_normal_cholesky_lpdf(
          [treatment_effect_rct[i], time_trend_rct[i]]' |
          [mult_rct[i] * (treatment_effect_mean_rct + X_cov_rct[i] * beta_cov), time_trend_mean]',
          L_Sigma_rct
        );
      } else if (is_student_t_heterogeneity) {
        target += student_t_lpdf(treatment_effect_rct[i] | nu_treatment_vec[1], mult_rct[i] * (treatment_effect_mean_rct + X_cov_rct[i] * beta_cov), treatment_effect_sd);
      }
      // Normal case: handled by treatment_effect_rct_raw ~ std_normal() below
    }
  }
  
  if (!is_baseline_normalised) {
    baseline_control_rct_raw ~ std_normal();
  }
  if (is_baseline_difference_estimated) {
    baseline_difference_rct_raw ~ std_normal();
  }
  // Time trend prior (non-centered; when correlated, included in joint prior above)
  if (!is_time_trend_rct_zero && !is_correlated_effects)
    time_trend_rct_raw ~ std_normal();
  // Treatment effect prior for the non-apparent-effect branch (normal non-centered case).
  // In the apparent_effect branch, treatment_effect_rct_raw has size 0 (no-op).
  if (!is_student_t_heterogeneity && !(is_correlated_effects && !is_time_trend_rct_zero))
    treatment_effect_rct_raw ~ std_normal();
  sigma_control_after_rct ~ cauchy(0, sigma_prior_scale);
  sigma_treatment_after_rct ~ cauchy(0, sigma_prior_scale);
}

