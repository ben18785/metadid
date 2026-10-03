real rct_summary_study_lpdf_from_data(
  real x_bar_control_after,
  real x_bar_treatment_after,
  real baseline_control,
  real baseline_treatment,
  real time_trend,
  real treatment_effect,
  real sigma_ca,
  real sigma_ta,
  int n_control,
  int n_treatment
) {
  real sigma_ca_n = sigma_ca / sqrt(n_control);
  real sigma_ta_n = sigma_ta / sqrt(n_treatment);

  real mu_control_after = baseline_control + time_trend;
  real mu_treatment_after = baseline_treatment + time_trend + treatment_effect;

  return normal_lpdf(x_bar_control_after | mu_control_after, sigma_ca_n) +
         normal_lpdf(x_bar_treatment_after | mu_treatment_after, sigma_ta_n);
}

real rct_summary_study_lpdf_from_data_differenced_form(
  real x_bar_control_after,
  real x_bar_treatment_after,
  real baseline_control,
  real baseline_treatment,
  real treatment_effect,
  real sigma_ca,
  real sigma_ta,
  int n_control,
  int n_treatment
) {
  real sigma_ca_n = sigma_ca / sqrt(n_control);
  real sigma_ta_n = sigma_ta / sqrt(n_treatment);

  real mu_diff_treatment_control = treatment_effect + baseline_treatment - baseline_control;
  real x_diff = x_bar_treatment_after - x_bar_control_after;
  real sigma_diff = sqrt(sigma_ca_n^2 + sigma_ta_n^2);

  return normal_lpdf(x_diff| mu_diff_treatment_control, sigma_diff);
}

// Normalised RCT summary likelihood (reparameterised).
// mean_offset is the expected treatment-control contrast on the normalised
// scale: apparent_effect when the time trend is estimated, gamma + theta when
// it is fixed at zero.
//
// Dividing both arm means by the observed control mean makes the normalised
// control mean exactly 1, so it cannot enter as a separate data point -- its
// residual is identically zero whatever the parameters are. The statistic the
// likelihood actually sees is the RATIO x_bar_t / x_bar_c, whose sampling error
// comes from both arms. Keeping only sigma_ta^2/n_t dropped the denominator's
// contribution, which is first-order here because the control mean is also one
// of the two cells being contrasted: with equal arms it is ~64% of the retained
// term, and with a small control arm it dominates.
//
// The delta-method coefficient on the denominator, d(x_t/x_c)/d(x_c) = -mu_t,
// is evaluated at the OBSERVED ratio rather than at mu_t itself. That keeps the
// variance free of the parameters.
real rct_summary_study_normalised_lpdf_from_data(
  real x_bar_treatment_after,
  real mean_offset,
  real sigma_ta,
  int n_treatment,
  real sigma_ca,
  int n_control
) {
  real var_treatment = square(sigma_ta) / n_treatment;
  real var_control   = square(x_bar_treatment_after) * square(sigma_ca) / n_control;
  return normal_lpdf(x_bar_treatment_after | 1.0 + mean_offset,
                     sqrt(var_treatment + var_control));
}
