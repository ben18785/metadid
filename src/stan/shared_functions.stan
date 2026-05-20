// shared_functions.stan
//
// Helper used to apply the optional multiplicative covariate to a study's
// linear predictor. With a binary covariate value x in {0, 1}, the factor is
//   1                  when has_multiplicative_covariate == 0 (feature off)
//   1                  when x == 0 (study is in the reference group)
//   gamma_mult[1]      when x == 1 (study is in the multiplied group)
//
// gamma_mult is passed in as the (possibly length-0) parameter vector so this
// function can be called unconditionally — when the feature is off it never
// dereferences the empty vector. Implemented as pow(gamma, x) for AD smoothness;
// with binary x this reduces to either 1 or gamma without an `if` branch on x.

real mult_factor(int has_multiplicative_covariate, vector gamma_mult, real x) {
  if (!has_multiplicative_covariate) return 1.0;
  return pow(gamma_mult[1], x);
}
