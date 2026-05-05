// rct_model.stan

if(n_studies_rct > 0) {
  // Construct effective baselines
  vector[n_studies_rct] baseline_control_rct_eff;
  vector[n_studies_rct] baseline_treatment_rct_eff;
  if (is_baseline_normalised) {
    baseline_control_rct_eff = rep_vector(1.0, n_studies_rct);
    baseline_treatment_rct_eff = rep_vector(1.0, n_studies_rct);
  } else {
    baseline_control_rct_eff = baseline_control_rct;
    if (is_baseline_control_equal_treatment_rct)
      baseline_treatment_rct_eff = baseline_control_rct;
    else
      baseline_treatment_rct_eff = baseline_treatment_rct;
  }

  // Construct effective time trends (zero when flag is set)
  vector[n_studies_rct] time_trend_rct_eff;
  if (is_time_trend_rct_zero)
    time_trend_rct_eff = rep_vector(0.0, n_studies_rct);
  else
    time_trend_rct_eff = time_trend_rct;

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

      // Hierarchical prior on the derived true treatment effect
      if (is_student_t_heterogeneity) {
        target += student_t_lpdf(treatment_effect_rct_derived[i] | nu_treatment_vec[1], treatment_effect_mean_rct, treatment_effect_sd);
      } else {
        target += normal_lpdf(treatment_effect_rct_derived[i] | treatment_effect_mean_rct, treatment_effect_sd);
      }

      // Jacobian: |d(te)/d(apparent)| = |1 + time_trend|
      target += log(abs(1 + time_trend_rct_eff[i]));

    } else {
      // Unnormalised or time trends forced to zero: treatment_effect_rct[i]
      // is sampled directly.
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

      // Hierarchical prior on treatment effect
      if (is_student_t_heterogeneity) {
        target += student_t_lpdf(treatment_effect_rct[i] | nu_treatment_vec[1], treatment_effect_mean_rct, treatment_effect_sd);
      } else {
        target += normal_lpdf(treatment_effect_rct[i] | treatment_effect_mean_rct, treatment_effect_sd);
      }
    }
  }
  
  if (!is_baseline_normalised) {
    baseline_control_rct ~ normal(baseline_control_mean[1], baseline_control_sd[1]);
    if (!is_baseline_control_equal_treatment_rct)
      baseline_treatment_rct ~ normal(baseline_treatment_mean[1], baseline_treatment_sd[1]);
  }
  if (!is_time_trend_rct_zero)
    time_trend_rct ~ normal(time_trend_mean, time_trend_sd);
  sigma_control_after_rct ~ cauchy(0, sigma_prior_scale);
  sigma_treatment_after_rct ~ cauchy(0, sigma_prior_scale);
}

