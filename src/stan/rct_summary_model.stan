
if(n_studies_rct_summary > 0) {
  for (i in 1:n_studies_rct_summary) {

    if (is_baseline_normalised) {
      // Normalised RCT: data divided by post-control mean, so
      // control_post = 1 by construction. Fix baseline = 1, time_trend = 0.
      if(!is_differenced_likelihood_rct_summary) {
        target += rct_summary_study_lpdf_from_data(
          x_bar_control_after_rct_summary[i],
          x_bar_treatment_after_rct_summary[i],
          1.0,                          // baseline_control = 1
          1.0,                          // baseline_treatment = 1
          0.0,                          // time_trend = 0
          treatment_effect_rct_summary[i],
          sd_control_after_rct_summary[i],
          sd_treatment_after_rct_summary[i],
          sample_size_control_rct_summary[i],
          sample_size_treatment_rct_summary[i]
        );
      } else {
        target += rct_summary_study_lpdf_from_data_differenced_form(
          x_bar_control_after_rct_summary[i],
          x_bar_treatment_after_rct_summary[i],
          1.0,                          // baseline_control = 1
          1.0,                          // baseline_treatment = 1
          treatment_effect_rct_summary[i],
          sd_control_after_rct_summary[i],
          sd_treatment_after_rct_summary[i],
          sample_size_control_rct_summary[i],
          sample_size_treatment_rct_summary[i]
        );
      }
    } else {
      // Original additive model
      real tt_rct_summary = 0;
      if(!is_time_trend_rct_summary_zero)
        if(!is_differenced_likelihood_rct_summary)
          tt_rct_summary = time_trend_rct_summary[i];

      if(!is_differenced_likelihood_rct_summary) {
        target += rct_summary_study_lpdf_from_data(
          x_bar_control_after_rct_summary[i],
          x_bar_treatment_after_rct_summary[i],
          baseline_control_rct_summary[i],
          baseline_treatment_rct_summary_effective[i],
          tt_rct_summary,
          treatment_effect_rct_summary[i],
          sd_control_after_rct_summary[i],
          sd_treatment_after_rct_summary[i],
          sample_size_control_rct_summary[i],
          sample_size_treatment_rct_summary[i]
        );
      } else {
        target += rct_summary_study_lpdf_from_data_differenced_form(
          x_bar_control_after_rct_summary[i],
          x_bar_treatment_after_rct_summary[i],
          baseline_control_rct_summary[i],
          baseline_treatment_rct_summary_effective[i],
          treatment_effect_rct_summary[i],
          sd_control_after_rct_summary[i],
          sd_treatment_after_rct_summary[i],
          sample_size_control_rct_summary[i],
          sample_size_treatment_rct_summary[i]
        );
      }
    }
  }
  
  if (!is_baseline_normalised) {
    time_trend_rct_summary ~ normal(time_trend_mean, time_trend_sd);
    baseline_control_rct_summary ~ normal(baseline_control_mean[1], baseline_control_sd[1]);
    baseline_treatment_rct_summary ~ normal(baseline_treatment_mean[1], baseline_treatment_sd[1]);
  }
  if (is_student_t_heterogeneity) {
    treatment_effect_rct_summary ~ student_t(nu_treatment_vec[1], treatment_effect_mean_rct, treatment_effect_sd);
  } else {
    treatment_effect_rct_summary ~ normal(treatment_effect_mean_rct, treatment_effect_sd);
  }
}

