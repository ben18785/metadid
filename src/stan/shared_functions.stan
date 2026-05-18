// shared_functions.stan
//
// Helper functions shared across design blocks.
// Currently provides the multiplicative-covariate factor used to shrink
// or expand the per-study linear predictor when one or more covariates
// are flagged as multiplicative (rather than additive).

// Apply multiplicative covariates to a scalar linear predictor.
//   mu       : additive part (treatment_effect_mean + X_cov[i] * beta_cov)
//   x_mult   : row of the multiplicative covariate matrix for study i
//   gamma_mult : vector of multipliers (one per multiplicative covariate)
//
// When K_mult == 0 the factor is the empty product (= 1) and mu is
// returned unchanged. With binary x_mult values, pow(gamma, x) reduces
// to a switch between 1 and gamma without an `if` branch (smoother AD).
real apply_mult_factor(real mu, row_vector x_mult, vector gamma_mult) {
  int K = num_elements(gamma_mult);
  if (K == 0) return mu;
  real factor = 1.0;
  for (k in 1:K) factor *= pow(gamma_mult[k], x_mult[k]);
  return factor * mu;
}

// Vectorised version: each row of X_mult selects a per-study factor.
vector apply_mult_factor_vec(vector mu, matrix X_mult, vector gamma_mult) {
  int N = num_elements(mu);
  int K = num_elements(gamma_mult);
  if (K == 0) return mu;
  vector[N] result;
  for (i in 1:N) {
    real factor = 1.0;
    for (k in 1:K) factor *= pow(gamma_mult[k], X_mult[i, k]);
    result[i] = factor * mu[i];
  }
  return result;
}
