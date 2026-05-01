real rct_lpdf(
  vector x,
  real baseline,
  real time_trend,
  real treatment,
  real sigma_after
) {
  return normal_lpdf(x | baseline + time_trend + treatment, sigma_after);
}

real rct_study_lpdf_from_data(
  int start_c, int end_c,
  int start_t, int end_t,
  vector x_control_after,
  vector x_treatment_after,
  real baseline_control,
  real baseline_treatment,
  real time_trend,
  real treatment_effect,
  real sigma_ca,
  real sigma_ta
) {
  int n_c = end_c - start_c + 1;
  int n_t = end_t - start_t + 1;

  vector[n_c] x_c = segment(x_control_after, start_c, n_c);
  vector[n_t] x_t = segment(x_treatment_after, start_t, n_t);

  return
    rct_lpdf(x_c | baseline_control, time_trend, 0, sigma_ca) +
    rct_lpdf(x_t | baseline_treatment, time_trend, treatment_effect, sigma_ta);
}
