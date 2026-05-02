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
    if (is_baseline_normalised) {
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
      baseline_treatment_pp ~ normal(baseline_treatment_mean[1], baseline_treatment_sd[1]);
    }
  }

  time_trend_pp ~ normal(time_trend_mean, time_trend_sd);
  if (is_student_t_heterogeneity) {
    treatment_effect_pp ~ student_t(nu_treatment_vec[1], treatment_effect_mean_pp, treatment_effect_sd);
  } else {
    treatment_effect_pp ~ normal(treatment_effect_mean_pp, treatment_effect_sd);
  }
}

