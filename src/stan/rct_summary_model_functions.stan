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
// apparent_effect = theta / (alpha + beta).  For summary data the control mean
// is exactly 1 by construction, so only the treatment mean contributes.
real rct_summary_study_normalised_lpdf_from_data(
  real x_bar_treatment_after,
  real apparent_effect,
  real sigma_ta,
  int n_treatment
) {
  real sigma_ta_n = sigma_ta / sqrt(n_treatment);
  return normal_lpdf(x_bar_treatment_after | 1.0 + apparent_effect, sigma_ta_n);
}
