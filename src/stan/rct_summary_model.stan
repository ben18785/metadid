
if(n_studies_rct_summary > 0) {
  // Construct effective baselines
  vector[n_studies_rct_summary] baseline_control_rct_summary_eff;
  vector[n_studies_rct_summary] baseline_treatment_rct_summary_eff;
  if (is_baseline_normalised) {
    baseline_control_rct_summary_eff = rep_vector(1.0, n_studies_rct_summary);
    baseline_treatment_rct_summary_eff = rep_vector(1.0, n_studies_rct_summary);
  } else {
    baseline_control_rct_summary_eff = baseline_control_rct_summary;
    if (is_baseline_control_equal_treatment_rct_summary)
      baseline_treatment_rct_summary_eff = baseline_control_rct_summary;
    else
      baseline_treatment_rct_summary_eff = baseline_treatment_rct_summary;
  }

  // Construct effective time trends (zero when flag is set)
  vector[n_studies_rct_summary] time_trend_rct_summary_eff;
  if (is_time_trend_rct_summary_zero)
    time_trend_rct_summary_eff = rep_vector(0.0, n_studies_rct_summary);
  else
    time_trend_rct_summary_eff = time_trend_rct_summary;

  for (i in 1:n_studies_rct_summary) {
    if (is_baseline_normalised && !is_time_trend_rct_summary_zero) {
      // Reparameterised: apparent_effect is sampled, control mean is 1 by construction.
      target += rct_summary_study_normalised_lpdf_from_data(
        x_bar_treatment_after_rct_summary[i],
        apparent_effect_rct_summary[i],
        sd_treatment_after_rct_summary[i],
        sample_size_treatment_rct_summary[i]
      );

      // Hierarchical prior on the derived true treatment effect
      if (is_student_t_heterogeneity) {
        target += student_t_lpdf(treatment_effect_rct_summary_derived[i] | nu_treatment_vec[1], treatment_effect_mean_rct, treatment_effect_sd);
      } else {
        target += normal_lpdf(treatment_effect_rct_summary_derived[i] | treatment_effect_mean_rct, treatment_effect_sd);
      }

      // Jacobian: |d(te)/d(apparent)| = |1 + time_trend|
      target += log(abs(1 + time_trend_rct_summary_eff[i]));

    } else {
      // Unnormalised or time trends forced to zero
      target += rct_summary_study_lpdf_from_data(
        x_bar_control_after_rct_summary[i],
        x_bar_treatment_after_rct_summary[i],
        baseline_control_rct_summary_eff[i],
        baseline_treatment_rct_summary_eff[i],
        time_trend_rct_summary_eff[i],
        treatment_effect_rct_summary[i],
        sd_control_after_rct_summary[i],
        sd_treatment_after_rct_summary[i],
        sample_size_control_rct_summary[i],
        sample_size_treatment_rct_summary[i]
      );

      // Hierarchical prior on treatment effect
      if (is_student_t_heterogeneity) {
        target += student_t_lpdf(treatment_effect_rct_summary[i] | nu_treatment_vec[1], treatment_effect_mean_rct, treatment_effect_sd);
      } else {
        target += normal_lpdf(treatment_effect_rct_summary[i] | treatment_effect_mean_rct, treatment_effect_sd);
      }
    }
  }

  if (!is_baseline_normalised) {
    baseline_control_rct_summary ~ normal(baseline_control_mean[1], baseline_control_sd[1]);
    if (!is_baseline_control_equal_treatment_rct_summary)
      baseline_treatment_rct_summary ~ normal(baseline_treatment_mean[1], baseline_treatment_sd[1]);
  }
  if (!is_time_trend_rct_summary_zero)
    time_trend_rct_summary ~ normal(time_trend_mean, time_trend_sd);
}

