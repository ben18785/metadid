// prepost_model.stan

if(n_studies_pp > 0) {

  if (is_differenced_likelihood_pp) {
    // Differenced form: d_j = post_j - pre_j ~ N(beta + theta, sigma_d)
    for (i in 1:n_studies_pp) {
      real tt_pp_temp = 0;
      if(!is_time_trend_pp_zero)
        tt_pp_temp = time_trend_pp[i];

      target += prepost_study_differenced_lpdf_from_data(
        study_start_treatment_pp[i], study_end_treatment_pp[i],
        x_treatment_before_pp,
        x_treatment_after_pp,
        tt_pp_temp,
        treatment_effect_pp[i],
        sigma_d_pp[i]
      );
    }
    sigma_d_pp ~ cauchy(0, sigma_prior_scale);

  } else {
    // Non-differenced (bivariate) form
    vector[n_studies_pp] baseline_treatment_pp_eff;
    // When differenced likelihood is active, baseline_treatment_pp has size 0;
    // this dummy value is set but never read (the differenced branch does not
    // use baseline_treatment_pp_eff).
    if (is_baseline_normalised || is_differenced_likelihood_pp) {
      baseline_treatment_pp_eff = rep_vector(1.0, n_studies_pp);
    } else {
      baseline_treatment_pp_eff = baseline_treatment_pp;
    }

    for (i in 1:n_studies_pp) {
      real tt_pp_temp = 0;
      if(!is_time_trend_pp_zero)
        tt_pp_temp = time_trend_pp[i];

      target += prepost_study_lpdf_from_data(
        study_start_treatment_pp[i], study_end_treatment_pp[i],
        x_treatment_before_pp,
        x_treatment_after_pp,
        baseline_treatment_pp_eff[i],
        tt_pp_temp,
        treatment_effect_pp[i],
        sigma_treatment_before_pp[i],
        sigma_treatment_after_pp[i],
        rho_pp[i]
      );
    }

    // Hierarchical prior on rho via Fisher z-transform.
    if (is_correlation_coefficient_hierarchical) {
      for (i in 1:n_studies_pp) {
        int n_i = sample_size_treatment_pp[i];
        atanh(rho_pp[i]) ~ normal(mu_z, sqrt(square(tau_z) + 1.0 / (n_i - 3)));
        target += -log1m(square(rho_pp[i]));
      }
    }

    sigma_treatment_before_pp ~ cauchy(0, sigma_prior_scale);
    sigma_treatment_after_pp ~ cauchy(0, sigma_prior_scale);
    if (!is_baseline_normalised) {
      baseline_treatment_pp_raw ~ std_normal();
    }
  }

  vector[n_studies_pp] mult_pp;
  for (i in 1:n_studies_pp)
    mult_pp[i] = overall_mult(effect_multiplier, x_mult_pp[i], effect_multiplier2, x_mult2_pp[i]);
  if (is_correlated_effects && !is_time_trend_pp_zero) {
    matrix[2, 2] L_Sigma_pp = diag_pre_multiply(
      [treatment_effect_sd, time_trend_sd]', L_corr_theta_beta[1]
    );
    for (i in 1:n_studies_pp) {
      target += multi_normal_cholesky_lpdf(
        [treatment_effect_pp[i], time_trend_pp[i]]' |
        [mult_pp[i] * (treatment_effect_mean_pp + X_cov_pp[i] * beta_cov), time_trend_mean]',
        L_Sigma_pp
      );
    }
  } else {
    time_trend_pp_raw ~ std_normal();
    if (is_student_t_heterogeneity) {
      treatment_effect_pp ~ student_t(nu_treatment_vec[1], mult_pp .* (treatment_effect_mean_pp + X_cov_pp * beta_cov), treatment_effect_sd);
    } else {
      treatment_effect_pp_raw ~ std_normal();
    }
  }
}

