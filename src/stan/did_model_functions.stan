real did_lpdf(
    matrix x,
    real baseline,
    real time_trend,
    real treatment,
    real sigma_before,
    real sigma_after,
    real rho
  ) {
    int N = rows(x);
    array[N] vector[2] x_vec;

    for (n in 1:N)
      x_vec[n] = to_vector(x[n]);

    vector[2] mu = [baseline, baseline + time_trend + treatment]';

    matrix[2, 2] Sigma;
    Sigma[1, 1] = square(sigma_before);
    Sigma[1, 2] = rho * sigma_before * sigma_after;
    Sigma[2, 1] = Sigma[1, 2];
    Sigma[2, 2] = square(sigma_after);

    return multi_normal_lpdf(x_vec | mu, Sigma);
  }

real did_study_lpdf_from_data(
  int start_c, int end_c,
  int start_t, int end_t,
  vector x_control_before,
  vector x_control_after,
  vector x_treatment_before,
  vector x_treatment_after,
  real baseline_control,
  real baseline_treatment,
  real time_trend,
  real treatment_effect,
  real sigma_cb,
  real sigma_ca,
  real sigma_tb,
  real sigma_ta,
  real rho
) {
  int n_c = end_c - start_c + 1;
  int n_t = end_t - start_t + 1;

  matrix[n_c, 2] m_control;
  m_control[, 1] = segment(x_control_before, start_c, n_c);
  m_control[, 2] = segment(x_control_after,  start_c, n_c);

  matrix[n_t, 2] m_treatment;
  m_treatment[, 1] = segment(x_treatment_before, start_t, n_t);
  m_treatment[, 2] = segment(x_treatment_after,  start_t, n_t);

  return
    did_lpdf(m_control| baseline_control, time_trend, 0, sigma_cb, sigma_ca, rho) +
    did_lpdf(m_treatment| baseline_treatment, time_trend, treatment_effect, sigma_tb, sigma_ta, rho);
}
