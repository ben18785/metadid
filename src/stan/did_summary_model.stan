// did_summary_model.stan

if(n_studies_did_summary > 0) {

  // Construct effective rho vector from known data and inferred missing values
  vector[n_studies_did_summary] rho_eff_did_summary;
  for (k in 1:n_rho_known_did_summary)
    rho_eff_did_summary[idx_rho_known_did_summary[k]] = rho_known_did_summary[k];
  for (k in 1:n_rho_missing_did_summary)
    rho_eff_did_summary[idx_rho_missing_did_summary[k]] = tanh(z_rho_missing_did_summary[k]);

  // Construct effective baselines (fixed at 1 when normalised)
  vector[n_studies_did_summary] baseline_control_did_summary_eff;
  vector[n_studies_did_summary] baseline_treatment_did_summary_eff;
  if (is_baseline_normalised) {
    baseline_control_did_summary_eff = rep_vector(1.0, n_studies_did_summary);
    baseline_treatment_did_summary_eff = rep_vector(1.0, n_studies_did_summary);
  } else {
    baseline_control_did_summary_eff = baseline_control_did_summary;
    baseline_treatment_did_summary_eff = baseline_treatment_did_summary;
  }

  for (i in 1:n_studies_did_summary) {
    
    if(!is_differenced_likelihood_did_summary) {
      target += did_summary_study_lpdf_from_data(
        x_bar_control_before_did_summary[i],
        x_bar_control_after_did_summary[i],
        x_bar_treatment_before_did_summary[i],
        x_bar_treatment_after_did_summary[i],
        baseline_control_did_summary_eff[i],
        baseline_treatment_did_summary_eff[i],
        time_trend_did_summary[i],
        treatment_effect_did_summary[i],
        sd_control_before_did_summary[i],
        sd_control_after_did_summary[i],
        sd_treatment_before_did_summary[i],
        sd_treatment_after_did_summary[i],
        rho_eff_did_summary[i],
        sample_size_control_did_summary[i],
        sample_size_treatment_did_summary[i]
      );
    } else{
      target += did_summary_study_lpdf_from_data_differenced_form(
        x_bar_control_before_did_summary[i],
        x_bar_control_after_did_summary[i],
        x_bar_treatment_before_did_summary[i],
        x_bar_treatment_after_did_summary[i],
        treatment_effect_did_summary[i],
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

  time_trend_did_summary ~ normal(time_trend_mean, time_trend_sd);
  if (!is_baseline_normalised) {
    baseline_control_did_summary ~ normal(baseline_control_mean[1], baseline_control_sd[1]);
    baseline_treatment_did_summary ~ normal(baseline_treatment_mean[1], baseline_treatment_sd[1]);
  }
  if (is_student_t_heterogeneity) {
    treatment_effect_did_summary ~ student_t(nu_treatment_vec[1], treatment_effect_mean, treatment_effect_sd);
  } else {
    treatment_effect_did_summary ~ normal(treatment_effect_mean, treatment_effect_sd);
  }
}

// Change-only studies: time trend and baseline cancel in the double-difference,
// so only treatment_effect is needed.
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
    treatment_effect_did_change_only ~ student_t(nu_treatment_vec[1], treatment_effect_mean, treatment_effect_sd);
  } else {
    treatment_effect_did_change_only ~ normal(treatment_effect_mean, treatment_effect_sd);
  }
}
