
real did_summary_study_lpdf_from_data(
  real x_bar_control_before,
  real x_bar_control_after,
  real x_bar_treatment_before,
  real x_bar_treatment_after,
  real baseline_control,
  real baseline_treatment,
  real time_trend,
  real treatment_effect,
  real sigma_cb,
  real sigma_ca,
  real sigma_tb,
  real sigma_ta,
  real rho,
  int n_control,
  int n_treatment
) {
  matrix[1, 2] x_bar_control;
  x_bar_control[1] = [x_bar_control_before, x_bar_control_after];

  real sigma_cb_n = sigma_cb / sqrt(n_control);
  real sigma_ca_n = sigma_ca / sqrt(n_control);

  matrix[2, 2] Sigma_control;
  Sigma_control[1, 1] = square(sigma_cb_n);
  Sigma_control[1, 2] = rho * sigma_cb_n * sigma_ca_n;
  Sigma_control[2, 1] = Sigma_control[1, 2];
  Sigma_control[2, 2] = square(sigma_ca_n);

  vector[2] mu_control = [baseline_control,
                          baseline_control + time_trend]';

  matrix[1, 2] x_bar_treatment;
  x_bar_treatment[1] = [x_bar_treatment_before, x_bar_treatment_after];

  real sigma_tb_n = sigma_tb / sqrt(n_treatment);
  real sigma_ta_n = sigma_ta / sqrt(n_treatment);

  matrix[2, 2] Sigma_treatment;
  Sigma_treatment[1, 1] = square(sigma_tb_n);
  Sigma_treatment[1, 2] = rho * sigma_tb_n * sigma_ta_n;
  Sigma_treatment[2, 1] = Sigma_treatment[1, 2];
  Sigma_treatment[2, 2] = square(sigma_ta_n);

  vector[2] mu_treatment = [baseline_treatment,
                            baseline_treatment + time_trend + treatment_effect]';

  return multi_normal_lpdf(to_vector(x_bar_control) | mu_control, Sigma_control)
       + multi_normal_lpdf(to_vector(x_bar_treatment) | mu_treatment, Sigma_treatment);
}

// Change-only likelihood: studies reporting change means and SDs directly,
// without separate pre/post values. The double-difference is sufficient to
// identify the treatment effect; time trend and baseline cancel out.
real did_summary_study_lpdf_from_change_data(
  real x_bar_change_control,
  real x_bar_change_treatment,
  real treatment_effect,
  real sd_change_control,
  real sd_change_treatment,
  int n_control,
  int n_treatment
) {
  real x_bar_double_diff = x_bar_change_treatment - x_bar_change_control;
  real sigma = sqrt(
    square(sd_change_control)   / n_control +
    square(sd_change_treatment) / n_treatment
  );
  return normal_lpdf(x_bar_double_diff | treatment_effect, sigma);
}

real did_summary_study_lpdf_from_data_differenced_form(
  real x_bar_control_before,
  real x_bar_control_after,
  real x_bar_treatment_before,
  real x_bar_treatment_after,
  real treatment_effect,
  real sigma_cb,
  real sigma_ca,
  real sigma_tb,
  real sigma_ta,
  real rho,
  int n_control,
  int n_treatment
) {
  
  real x_bar_control_ba = x_bar_control_after - x_bar_control_before;
  real x_bar_treatment_ba = x_bar_treatment_after - x_bar_treatment_before;
  real x_bar_double_diff = x_bar_treatment_ba - x_bar_control_ba;
  real sc_sq = (sigma_cb^2 + sigma_ca^2 - 2 * rho * sigma_cb * sigma_ca) / n_control;
  real st_sq = (sigma_tb^2 + sigma_ta^2 - 2 * rho * sigma_tb * sigma_ta) / n_treatment;
  real sigma = sqrt(sc_sq + st_sq);
  
  return normal_lpdf(x_bar_double_diff | treatment_effect, sigma);
}
