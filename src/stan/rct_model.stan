// rct_model.stan

if(n_studies_rct > 0) {
  for (i in 1:n_studies_rct) {

    if (is_baseline_normalised) {
      // Normalised RCT: data divided by post-control mean, so
      // control_post = 1 by construction. Fix baseline = 1, time_trend = 0.
      // Model: control ~ N(1, sigma^2), treatment ~ N(1 + te, sigma^2)
      target += rct_study_lpdf_from_data(
        study_start_control_rct[i], study_end_control_rct[i],
        study_start_treatment_rct[i], study_end_treatment_rct[i],
        x_control_after_rct,
        x_treatment_after_rct,
        1.0,                          // baseline_control = 1
        1.0,                          // baseline_treatment = 1
        0.0,                          // time_trend = 0
        treatment_effect_rct[i],
        sigma_control_after_rct[i],
        sigma_treatment_after_rct[i]
      );
    } else {
      // Original additive model
      real tt_rct = 0;
      if(!is_time_trend_rct_zero)
        tt_rct = time_trend_rct[i];

      real b_c_rct = 0;
      if(!is_baseline_control_rct_zero)
        b_c_rct = baseline_control_rct[i];
        
      real b_t_rct = 0;
      if(!is_baseline_treatment_rct_zero)
        b_t_rct = baseline_treatment_rct[i];

      target += rct_study_lpdf_from_data(
        study_start_control_rct[i], study_end_control_rct[i],
        study_start_treatment_rct[i], study_end_treatment_rct[i],
        x_control_after_rct,
        x_treatment_after_rct,
        b_c_rct,
        b_t_rct,
        tt_rct,
        treatment_effect_rct[i],
        sigma_control_after_rct[i],
        sigma_treatment_after_rct[i]
      );
    }
  }
  
  if (!is_baseline_normalised) {
    time_trend_rct ~ normal(time_trend_mean, time_trend_sd);
    baseline_control_rct ~ normal(baseline_control_mean[1], baseline_control_sd[1]);
    baseline_treatment_rct ~ normal(baseline_treatment_mean[1], baseline_treatment_sd[1]);
  }
  if (is_student_t_heterogeneity) {
    treatment_effect_rct ~ student_t(nu_treatment_vec[1], treatment_effect_mean_rct, treatment_effect_sd);
  } else {
    treatment_effect_rct ~ normal(treatment_effect_mean_rct, treatment_effect_sd);
  }
}

