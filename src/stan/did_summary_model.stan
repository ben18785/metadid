// did_summary_model.stan

if(n_studies_did_summary > 0) {

  // Construct effective rho vector from known data and inferred missing values
  vector[n_studies_did_summary] rho_eff_did_summary;
  for (k in 1:n_rho_known_did_summary)
    rho_eff_did_summary[idx_rho_known_did_summary[k]] = rho_known_did_summary[k];
  for (k in 1:n_rho_missing_did_summary)
    rho_eff_did_summary[idx_rho_missing_did_summary[k]] = tanh(z_rho_missing_did_summary[k]);

  for (i in 1:n_studies_did_summary) {

    if(!is_differenced_likelihood_did_summary) {
      // Bridge from the canonical fractional scale (where the hierarchical
      // pooling lives) to the absolute scale (where the data and the
      // likelihood live). In modelled modes the per-study θ and γ are
      // drawn from the population-level prior on the fractional scale, so
      // we multiply by the per-study treatment-pre baseline to recover
      // absolute units that match the raw observations.
      // In none mode the canonical and absolute scales coincide (no
      // scaling), so we pass through unchanged.
      real bl_c   = baseline_control_did_summary[i];
      real bl_t   = baseline_treatment_did_summary[i];
      real te_abs = is_modelled
                    ? treatment_effect_did_summary[i] * bl_t
                    : treatment_effect_did_summary[i];
      real tt_abs = is_modelled
                    ? time_trend_did_summary[i] * bl_t
                    : time_trend_did_summary[i];

      target += did_summary_study_lpdf_from_data(
        x_bar_control_before_did_summary[i],
        x_bar_control_after_did_summary[i],
        x_bar_treatment_before_did_summary[i],
        x_bar_treatment_after_did_summary[i],
        bl_c,
        bl_t,
        tt_abs,
        te_abs,
        sd_control_before_did_summary[i],
        sd_control_after_did_summary[i],
        sd_treatment_before_did_summary[i],
        sd_treatment_after_did_summary[i],
        rho_eff_did_summary[i],
        sample_size_control_did_summary[i],
        sample_size_treatment_did_summary[i]
      );
    } else {
      // Differenced likelihood: baselines cancel; only the absolute-scale
      // treatment effect enters. The double-difference cleanly identifies
      // it in modelled mode without an explicit baseline-data term because
      // the per-study latent has no other path into this likelihood.
      // (DiD studies using the differenced form will see the latent
      // baseline informed solely by its uniform prior; this is acceptable
      // because the differenced likelihood is opt-in and users who select
      // it have decided to discard baseline-level information anyway.)
      real bl_t_for_scaling = is_modelled ? baseline_treatment_did_summary[i] : 1.0;
      real te_abs = is_modelled
                    ? treatment_effect_did_summary[i] * bl_t_for_scaling
                    : treatment_effect_did_summary[i];
      target += did_summary_study_lpdf_from_data_differenced_form(
        x_bar_control_before_did_summary[i],
        x_bar_control_after_did_summary[i],
        x_bar_treatment_before_did_summary[i],
        x_bar_treatment_after_did_summary[i],
        te_abs,
        sd_control_before_did_summary[i],
        sd_control_after_did_summary[i],
        sd_treatment_before_did_summary[i],
        sd_treatment_after_did_summary[i],
        rho_eff_did_summary[i],
        sample_size_control_did_summary[i],
        sample_size_treatment_did_summary[i]
      );
    }
  }

  // Hierarchical prior on rho via Fisher z-transform
  if (is_correlation_coefficient_hierarchical) {
    for (k in 1:n_rho_known_did_summary) {
      int idx = idx_rho_known_did_summary[k];
      int n_i = sample_size_control_did_summary[idx] + sample_size_treatment_did_summary[idx];
      atanh(rho_known_did_summary[k]) ~ normal(mu_z, sqrt(square(tau_z) + 1.0 / (n_i - 3)));
    }
    for (k in 1:n_rho_missing_did_summary) {
      int idx = idx_rho_missing_did_summary[k];
      int n_i = sample_size_control_did_summary[idx] + sample_size_treatment_did_summary[idx];
      z_rho_missing_did_summary[k] ~ normal(mu_z, sqrt(square(tau_z) + 1.0 / (n_i - 3)));
    }
  }

  // Non-centered priors. In modelled modes the per-study latent baseline
  // has a uniform prior (declared in the parameters block via its upper
  // bound), so no explicit ~ statement is needed for it.
  if (is_none_mode) {
    baseline_control_did_summary_raw ~ std_normal();
  }
  if (!is_differenced_likelihood_did_summary) {
    // Direct hierarchical prior on the constrained γ. The <lower=-1>
    // constraint is enforced at declaration; this just provides the
    // population-level Normal pull.
    baseline_difference_did_summary ~ normal(baseline_difference_mean, baseline_difference_sd);
  }
  vector[n_studies_did_summary] mult_did_summary;
  for (i in 1:n_studies_did_summary)
    mult_did_summary[i] = overall_mult(effect_multiplier, x_mult_did_summary[i], effect_multiplier2, x_mult2_did_summary[i]);
  if (is_correlated_effects) {
    matrix[2, 2] L_Sigma_did_summary = diag_pre_multiply(
      [treatment_effect_sd, time_trend_sd]', L_corr_theta_beta[1]
    );
    for (i in 1:n_studies_did_summary) {
      target += multi_normal_cholesky_lpdf(
        [treatment_effect_did_summary[i], time_trend_did_summary[i]]' |
        [mult_did_summary[i] * (treatment_effect_mean + X_cov_did_summary[i] * beta_cov), time_trend_mean]',
        L_Sigma_did_summary
      );
    }
  } else {
    time_trend_did_summary_raw ~ std_normal();
    if (is_student_t_heterogeneity) {
      treatment_effect_did_summary ~ student_t(nu_treatment_vec[1], mult_did_summary .* (treatment_effect_mean + X_cov_did_summary * beta_cov), treatment_effect_sd);
    } else {
      treatment_effect_did_summary_raw ~ std_normal();
    }
  }
}

// Change-only studies: time trend and baseline cancel in the double-difference,
// so only treatment_effect is needed. User-provided change values are assumed
// to already be on the relative scale appropriate for the chosen mode (the
// package does not pre-normalise them).
if (n_studies_did_change_only > 0) {
  for (i in 1:n_studies_did_change_only) {
    target += did_summary_study_lpdf_from_change_data(
      x_bar_change_control_did_change_only[i],
      x_bar_change_treatment_did_change_only[i],
      treatment_effect_did_change_only[i],
      sd_change_control_did_change_only[i],
      sd_change_treatment_did_change_only[i],
      sample_size_control_did_change_only[i],
      sample_size_treatment_did_change_only[i]
    );
  }
  if (is_student_t_heterogeneity) {
    vector[n_studies_did_change_only] mult_did_change_only;
    for (i in 1:n_studies_did_change_only)
      mult_did_change_only[i] = overall_mult(effect_multiplier, x_mult_did_change_only[i], effect_multiplier2, x_mult2_did_change_only[i]);
    treatment_effect_did_change_only ~ student_t(nu_treatment_vec[1], mult_did_change_only .* (treatment_effect_mean + X_cov_did_change_only * beta_cov), treatment_effect_sd);
  } else {
    treatment_effect_did_change_only_raw ~ std_normal();
  }
}
