// prepost_summary_model.stan

if(n_studies_pp_summary > 0) {

  // Construct effective rho vector from known data and inferred missing values
  vector[n_studies_pp_summary] rho_eff_pp_summary;
  for (k in 1:n_rho_known_pp_summary)
    rho_eff_pp_summary[idx_rho_known_pp_summary[k]] = rho_known_pp_summary[k];
  for (k in 1:n_rho_missing_pp_summary)
    rho_eff_pp_summary[idx_rho_missing_pp_summary[k]] = tanh(z_rho_missing_pp_summary[k]);

  for (i in 1:n_studies_pp_summary) {

    real tt_pp_summary_temp = 0;
    if(!is_time_trend_pp_summary_zero)
      tt_pp_summary_temp = time_trend_pp_summary[i];

    if(!is_differenced_likelihood_pp_summary) {
      // Bridge from canonical fractional scale to absolute. In modelled
      // modes we scale by the per-study b_T_pre to recover absolute units;
      // in none mode treatment effect and time trend are already absolute.
      real bl_t   = baseline_treatment_pp_summary[i];
      real te_abs = is_modelled
                    ? treatment_effect_pp_summary[i] * bl_t
                    : treatment_effect_pp_summary[i];
      real tt_abs = is_modelled
                    ? tt_pp_summary_temp * bl_t
                    : tt_pp_summary_temp;

      target += prepost_summary_study_lpdf_from_data(
        x_bar_treatment_before_pp_summary[i],
        x_bar_treatment_after_pp_summary[i],
        bl_t,
        tt_abs,
        te_abs,
        sd_treatment_before_pp_summary[i],
        sd_treatment_after_pp_summary[i],
        rho_eff_pp_summary[i],
        sample_size_treatment_pp_summary[i]
      );
    } else {
      // Differenced likelihood: the mean of the difference cancels the
      // baseline (mu = time_trend + treatment_effect), but in modelled
      // mode we still need to bridge from the canonical fractional scale
      // to the absolute scale before passing the quantities into the
      // likelihood. We do that by multiplying by the per-study baseline,
      // and we anchor the latent baseline by an explicit likelihood term
      // on x_bar_treatment_before below (the differenced likelihood
      // itself does not inform the baseline).
      real bl_t   = is_modelled ? baseline_treatment_pp_summary[i] : 1.0;
      real te_abs = is_modelled
                    ? treatment_effect_pp_summary[i] * bl_t
                    : treatment_effect_pp_summary[i];
      real tt_abs = is_modelled
                    ? tt_pp_summary_temp * bl_t
                    : tt_pp_summary_temp;

      target += prepost_summary_study_lpdf_from_data_differenced_form(
        x_bar_treatment_before_pp_summary[i],
        x_bar_treatment_after_pp_summary[i],
        tt_abs,
        te_abs,
        sd_treatment_before_pp_summary[i],
        sd_treatment_after_pp_summary[i],
        rho_eff_pp_summary[i],
        sample_size_treatment_pp_summary[i]
      );

      // Anchor the per-study latent baseline directly from the observed
      // pre-period mean. This is what makes the latent identifiable when
      // the differenced effect-likelihood has cancelled it out.
      if (is_modelled) {
        x_bar_treatment_before_pp_summary[i]
          ~ normal(bl_t, sd_treatment_before_pp_summary[i] /
                          sqrt(sample_size_treatment_pp_summary[i] * 1.0));
      }
    }
  }

  // Hierarchical prior on rho via Fisher z-transform
  if (is_correlation_coefficient_hierarchical) {
    for (k in 1:n_rho_known_pp_summary) {
      int idx = idx_rho_known_pp_summary[k];
      int n_i = sample_size_treatment_pp_summary[idx];
      atanh(rho_known_pp_summary[k]) ~ normal(mu_z, sqrt(square(tau_z) + 1.0 / (n_i - 3)));
    }
    for (k in 1:n_rho_missing_pp_summary) {
      int idx = idx_rho_missing_pp_summary[k];
      int n_i = sample_size_treatment_pp_summary[idx];
      z_rho_missing_pp_summary[k] ~ normal(mu_z, sqrt(square(tau_z) + 1.0 / (n_i - 3)));
    }
  }

  if (is_none_mode) {
    baseline_treatment_pp_summary_raw ~ std_normal();
  }
  vector[n_studies_pp_summary] mult_pp_summary;
  for (i in 1:n_studies_pp_summary)
    mult_pp_summary[i] = mult_factor(effect_multiplier, x_mult_pp_summary[i]);
  if (is_correlated_effects && !is_time_trend_pp_summary_zero) {
    matrix[2, 2] L_Sigma_pp_summary = diag_pre_multiply(
      [treatment_effect_sd, time_trend_sd]', L_corr_theta_beta[1]
    );
    for (i in 1:n_studies_pp_summary) {
      target += multi_normal_cholesky_lpdf(
        [treatment_effect_pp_summary[i], time_trend_pp_summary[i]]' |
        [mult_pp_summary[i] * (treatment_effect_mean_pp + X_cov_pp_summary[i] * beta_cov), time_trend_mean]',
        L_Sigma_pp_summary
      );
    }
  } else {
    time_trend_pp_summary_raw ~ std_normal();
    if (is_student_t_heterogeneity) {
      treatment_effect_pp_summary ~ student_t(nu_treatment_vec[1], mult_pp_summary .* (treatment_effect_mean_pp + X_cov_pp_summary * beta_cov), treatment_effect_sd);
    } else {
      treatment_effect_pp_summary_raw ~ std_normal();
    }
  }
}
