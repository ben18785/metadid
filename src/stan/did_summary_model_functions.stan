
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

// Normalised DiD summary likelihood.
//
// Dividing all four cells by the observed pre-control mean makes that cell
// exactly 1, so it cannot enter as a separate data point: its residual is
// identically zero whatever the parameters are. What the likelihood sees is the
// three remaining cells as RATIOS sharing one noisy denominator. Treating that
// denominator as known understates the control arm's change variance -- the
// bivariate form is left with the conditional sigma_ca^2(1 - rho^2) in place of
// the paired sigma_cb^2 + sigma_ca^2 - 2 rho sigma_cb sigma_ca, a factor of
// (1 + rho)/2 -- and drops the covariance the shared denominator induces
// between the two arms.
//
// Sigma below is the delta-method covariance of (r_ca, r_tb, r_ta), where
// r_j = x_bar_j / x_bar_cb. With Cov(r_j, r_k) = V_jk - m_j V_cb,k
// - m_k V_cb,j + m_j m_k V_cb,cb, and V_cb,tb = V_cb,ta = 0 because the arms are
// sampled independently. The coefficients m_j are evaluated at the OBSERVED
// ratios, not at their expectations, so the covariance stays free of the
// parameters.
real did_summary_study_normalised_lpdf_from_data(
  real x_bar_control_after,
  real x_bar_treatment_before,
  real x_bar_treatment_after,
  real baseline_difference,
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
  real v_cb = square(sigma_cb) / n_control;          // the denominator cell
  real v_ca = square(sigma_ca) / n_control;
  real c_c  = rho * sigma_cb * sigma_ca / n_control; // within-control pre/post
  real v_tb = square(sigma_tb) / n_treatment;
  real v_ta = square(sigma_ta) / n_treatment;
  real c_t  = rho * sigma_tb * sigma_ta / n_treatment;

  vector[3] m = [x_bar_control_after,
                 x_bar_treatment_before,
                 x_bar_treatment_after]';

  // Indices 1, 2, 3 are the control-post, treatment-pre and treatment-post
  // ratios. Every entry carries a + m_j m_k v_cb term: that is the shared
  // denominator, and it is what couples the two arms. The -c_c terms appear
  // only in row/column 1, because the pre-control cell is correlated with the
  // post-control cell but independent of the treatment arm.
  matrix[3, 3] Sigma;
  Sigma[1, 1] = v_ca - 2 * m[1] * c_c + square(m[1]) * v_cb;  // control change: the paired variance
  Sigma[2, 2] = v_tb + square(m[2]) * v_cb;                   // treatment pre level
  Sigma[3, 3] = v_ta + square(m[3]) * v_cb;                   // treatment post level
  Sigma[1, 2] = -m[2] * c_c + m[1] * m[2] * v_cb;             // control change vs treatment pre
  Sigma[1, 3] = -m[3] * c_c + m[1] * m[3] * v_cb;             // control change vs treatment post
  Sigma[2, 3] = c_t + m[2] * m[3] * v_cb;                     // within-treatment pre/post, plus denominator
  Sigma[2, 1] = Sigma[1, 2];
  Sigma[3, 1] = Sigma[1, 3];
  Sigma[3, 2] = Sigma[2, 3];

  // Cell means relative to the pinned pre-control baseline of 1: the control
  // arm moves by the time trend, the treatment arm starts off by the baseline
  // difference, and its follow-up adds the trend and the treatment effect.
  vector[3] mu = [1 + time_trend,                                            // control post:    alpha + beta
                  1 + baseline_difference,                                   // treatment pre:   alpha + gamma
                  1 + baseline_difference + time_trend + treatment_effect]'; // treatment post:  alpha + gamma + beta + theta

  return multi_normal_lpdf(m | mu, Sigma);
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
