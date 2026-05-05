// did_model.stan

if(n_studies_did > 0) {
  // Construct effective baselines (fixed at 1 when normalised)
  vector[n_studies_did] baseline_control_did_eff;
  vector[n_studies_did] baseline_treatment_did_eff;
  if (is_baseline_normalised) {
    baseline_control_did_eff = rep_vector(1.0, n_studies_did);
    baseline_treatment_did_eff = rep_vector(1.0, n_studies_did);
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

  time_trend_did ~ normal(time_trend_mean, time_trend_sd);
  if (!is_baseline_normalised) {
    baseline_control_did ~ normal(baseline_control_mean[1], baseline_control_sd[1]);
    baseline_treatment_did ~ normal(baseline_treatment_mean[1], baseline_treatment_sd[1]);
  }
  sigma_control_before_did ~ cauchy(0, sigma_prior_scale);
  sigma_control_after_did ~ cauchy(0, sigma_prior_scale);
  sigma_treatment_before_did ~ cauchy(0, sigma_prior_scale);
  sigma_treatment_after_did ~ cauchy(0, sigma_prior_scale);
  if (is_student_t_heterogeneity) {
    treatment_effect_did ~ student_t(nu_treatment_vec[1], treatment_effect_mean, treatment_effect_sd);
  } else {
    treatment_effect_did ~ normal(treatment_effect_mean, treatment_effect_sd);
  }
}
