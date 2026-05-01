real prepost_summary_study_lpdf_from_data(
  real x_bar_treatment_before,
  real x_bar_treatment_after,
  real baseline_treatment,
  real time_trend,
  real treatment_effect,
  real sigma_tb,
  real sigma_ta,
  real rho,
  int n_treatment
) {
  matrix[1, 2] x_bar_treatment;
  x_bar_treatment[1] = [x_bar_treatment_before, x_bar_treatment_after];

  real sigma_tb_n = sigma_tb / sqrt(n_treatment);
  real sigma_ta_n = sigma_ta / sqrt(n_treatment);

  matrix[2, 2] Sigma;
  Sigma[1, 1] = square(sigma_tb_n);
  Sigma[1, 2] = rho * sigma_tb_n * sigma_ta_n;
  Sigma[2, 1] = Sigma[1, 2];
  Sigma[2, 2] = square(sigma_ta_n);

  vector[2] mu = [baseline_treatment,
                 baseline_treatment + time_trend + treatment_effect]';

  return multi_normal_lpdf(to_vector(x_bar_treatment) | mu, Sigma);
}

real prepost_summary_study_lpdf_from_data_differenced_form(
  real x_bar_treatment_before,
  real x_bar_treatment_after,
  real time_trend,
  real treatment_effect,
  real sigma_tb,
  real sigma_ta,
  real rho,
  int n_treatment
) {
  
  real x_bar_diff = x_bar_treatment_after - x_bar_treatment_before;
  real mu = time_trend + treatment_effect;
  real sigma = sqrt((sigma_ta^2 + sigma_tb^2 - 2 * rho * sigma_ta * sigma_tb)/n_treatment);
  
  return normal_lpdf(x_bar_diff | mu, sigma);
}
