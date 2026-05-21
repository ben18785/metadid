// prepost_model.stan

if(n_studies_pp > 0) {

  if (is_differenced_likelihood_pp) {
    // Differenced form: d_j = post_j - pre_j ~ N(beta + theta, sigma_d).
    // The mean of the difference cancels the baseline, but in modelled
    // mode we still need to bridge from the canonical fractional scale to
    // the absolute scale before passing γ_T and θ_T into the likelihood.
    // We multiply by the per-study latent baseline; the latent is anchored
    // by an explicit likelihood term below on the individual pre-treatment
    // observations (the differenced likelihood itself does not inform it).
    for (i in 1:n_studies_pp) {
      real tt_pp_temp = 0;
      if(!is_time_trend_pp_zero)
        tt_pp_temp = time_trend_pp[i];

      real bl_t   = is_modelled ? baseline_treatment_pp[i] : 1.0;
      real te_abs = is_modelled ? treatment_effect_pp[i] * bl_t : treatment_effect_pp[i];
      real tt_abs = is_modelled ? tt_pp_temp * bl_t : tt_pp_temp;

      target += prepost_study_differenced_lpdf_from_data(
        study_start_treatment_pp[i], study_end_treatment_pp[i],
        x_treatment_before_pp,
        x_treatment_after_pp,
        tt_abs,
        te_abs,
        sigma_d_pp[i]
      );

      // Anchor the per-study latent baseline from the individual pre-period
      // observations. We use sigma_d_pp[i] / sqrt(2) as the per-subject
      // observation SD on the pre side — a reasonable approximation when
      // pre and post within-subject SDs are similar (Var(d) ≈ 2σ²(1-ρ),
      // so σ_pre ≈ σ_d / sqrt(2(1-ρ)); the sqrt(2) here corresponds to
      // ρ ≈ 0.5, the package's typical default). This is approximate but
      // makes the latent identifiable; users wanting tight per-study
      // baselines should switch to the bivariate (non-differenced) form.
      if (is_modelled) {
        for (j in study_start_treatment_pp[i]:study_end_treatment_pp[i])
          target += normal_lpdf(x_treatment_before_pp[j] | bl_t, sigma_d_pp[i] / sqrt(2.0));
      }
    }
    sigma_d_pp ~ cauchy(0, sigma_prior_scale);

  } else {
    // Non-differenced (bivariate) form. Bridge canonical → absolute by
    // scaling per-study θ and γ by the per-study treatment-pre baseline in
    // modelled modes.
    for (i in 1:n_studies_pp) {
      real tt_pp_temp = 0;
      if(!is_time_trend_pp_zero)
        tt_pp_temp = time_trend_pp[i];

      real bl_t   = baseline_treatment_pp[i];
      real te_abs = is_modelled
                    ? treatment_effect_pp[i] * bl_t
                    : treatment_effect_pp[i];
      real tt_abs = is_modelled
                    ? tt_pp_temp * bl_t
                    : tt_pp_temp;

      target += prepost_study_lpdf_from_data(
        study_start_treatment_pp[i], study_end_treatment_pp[i],
        x_treatment_before_pp,
        x_treatment_after_pp,
        bl_t,
        tt_abs,
        te_abs,
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
    if (is_none_mode) {
      baseline_treatment_pp_raw ~ std_normal();
    }
  }

  if (is_correlated_effects && !is_time_trend_pp_zero) {
    matrix[2, 2] L_Sigma_pp = diag_pre_multiply(
      [treatment_effect_sd, time_trend_sd]', L_corr_theta_beta[1]
    );
    for (i in 1:n_studies_pp) {
      target += multi_normal_cholesky_lpdf(
        [treatment_effect_pp[i], time_trend_pp[i]]' |
        [treatment_effect_mean_pp + X_cov_pp[i] * beta_cov, time_trend_mean]',
        L_Sigma_pp
      );
    }
  } else {
    time_trend_pp_raw ~ std_normal();
    if (is_student_t_heterogeneity) {
      treatment_effect_pp ~ student_t(nu_treatment_vec[1], treatment_effect_mean_pp + X_cov_pp * beta_cov, treatment_effect_sd);
    } else {
      treatment_effect_pp_raw ~ std_normal();
    }
  }
}
