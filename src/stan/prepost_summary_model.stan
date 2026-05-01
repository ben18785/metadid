// prepost_summary_model.stan

if(n_studies_pp_summary > 0) {

  // Construct effective rho vector from known data and inferred missing values
  vector[n_studies_pp_summary] rho_eff_pp_summary;
  for (k in 1:n_rho_known_pp_summary)
    rho_eff_pp_summary[idx_rho_known_pp_summary[k]] = rho_known_pp_summary[k];
  for (k in 1:n_rho_missing_pp_summary)
    rho_eff_pp_summary[idx_rho_missing_pp_summary[k]] = tanh(z_rho_missing_pp_summary[k]);

  // Construct effective baseline (fixed at 1 when normalised)
  vector[n_studies_pp_summary] baseline_treatment_pp_summary_eff;
  if (is_baseline_normalised) {
    baseline_treatment_pp_summary_eff = rep_vector(1.0, n_studies_pp_summary);
  } else {
    baseline_treatment_pp_summary_eff = baseline_treatment_pp_summary;
  }

  for (i in 1:n_studies_pp_summary) {
    
    real tt_pp_summary_temp = 0;
    if(!is_time_trend_pp_summary_zero)
      tt_pp_summary_temp = time_trend_pp_summary[i];
    
    if(!is_differenced_likelihood_pp_summary) {
      target += prepost_summary_study_lpdf_from_data(
        x_bar_treatment_before_pp_summary[i],
        x_bar_treatment_after_pp_summary[i],
        baseline_treatment_pp_summary_eff[i],
        tt_pp_summary_temp,
        treatment_effect_pp_summary[i],
        sd_treatment_before_pp_summary[i],
        sd_treatment_after_pp_summary[i],
        rho_eff_pp_summary[i],
        sample_size_treatment_pp_summary[i]
      );
    } else {
      target += prepost_summary_study_lpdf_from_data_differenced_form(
        x_bar_treatment_before_pp_summary[i],
        x_bar_treatment_after_pp_summary[i],
        tt_pp_summary_temp,
        treatment_effect_pp_summary[i],
        sd_treatment_before_pp_summary[i],
        sd_treatment_after_pp_summary[i],
        rho_eff_pp_summary[i],
        sample_size_treatment_pp_summary[i]
      );
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

  time_trend_pp_summary ~ normal(time_trend_mean, time_trend_sd);
  if (!is_baseline_normalised) {
    baseline_treatment_pp_summary ~ normal(baseline_treatment_mean[1], baseline_treatment_sd[1]);
  }
  if (is_student_t_heterogeneity) {
    treatment_effect_pp_summary ~ student_t(nu_treatment_vec[1], treatment_effect_mean_pp, treatment_effect_sd);
  } else {
    treatment_effect_pp_summary ~ normal(treatment_effect_mean_pp, treatment_effect_sd);
  }
}

