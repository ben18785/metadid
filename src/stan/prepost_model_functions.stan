real prepost_study_lpdf_from_data(
  int start_t, int end_t,
  vector x_treatment_before,
  vector x_treatment_after,
  real baseline_treatment,
  real time_trend,
  real treatment_effect,
  real sigma_tb,
  real sigma_ta,
  real rho
) {
  int n_t = end_t - start_t + 1;

  matrix[n_t, 2] m_treatment;
  m_treatment[, 1] = segment(x_treatment_before, start_t, n_t);
  m_treatment[, 2] = segment(x_treatment_after,  start_t, n_t);

  vector[2] mu = [baseline_treatment,
                  baseline_treatment + time_trend + treatment_effect]';

  matrix[2, 2] Sigma;
  Sigma[1, 1] = square(sigma_tb);
  Sigma[1, 2] = rho * sigma_tb * sigma_ta;
  Sigma[2, 1] = Sigma[1, 2];
  Sigma[2, 2] = square(sigma_ta);

  array[n_t] vector[2] x_vec;
  for (n in 1:n_t)
    x_vec[n] = to_vector(m_treatment[n]);

  return multi_normal_lpdf(x_vec | mu, Sigma);
}

real prepost_study_differenced_lpdf_from_data(
  int start_t, int end_t,
  vector x_treatment_before,
  vector x_treatment_after,
  real time_trend,
  real treatment_effect,
  real sigma_d
) {
  int n_t = end_t - start_t + 1;
  vector[n_t] d = segment(x_treatment_after, start_t, n_t)
                - segment(x_treatment_before, start_t, n_t);
  return normal_lpdf(d | time_trend + treatment_effect, sigma_d);
}
