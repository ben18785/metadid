
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
    if (is_baseline_normalised) {
      target += rct_summary_study_normalised_lpdf_from_data(
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
    } else {
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
    }
  }

  if (!is_baseline_normalised) {
    baseline_control_rct_summary ~ normal(baseline_control_mean[1], baseline_control_sd[1]);
    if (!is_baseline_control_equal_treatment_rct_summary)
      baseline_treatment_rct_summary ~ normal(baseline_treatment_mean[1], baseline_treatment_sd[1]);
  }
  if (!is_time_trend_rct_summary_zero)
    time_trend_rct_summary ~ normal(time_trend_mean, time_trend_sd);
  if (is_student_t_heterogeneity) {
    treatment_effect_rct_summary ~ student_t(nu_treatment_vec[1], treatment_effect_mean_rct, treatment_effect_sd);
  } else {
    treatment_effect_rct_summary ~ normal(treatment_effect_mean_rct, treatment_effect_sd);
  }
}

