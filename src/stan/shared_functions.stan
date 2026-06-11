// shared_functions.stan
//
// Helper applying the optional multiplicative covariate to a study's
// linear predictor. The covariate has a single binary level: a study either
// belongs to the multiplied group (x == 1, factor = effect_multiplier[1]) or to
// the reference group (x == 0, factor = 1).
//
// effect_multiplier is passed in as the (possibly length-0) parameter vector so the
// function can be called unconditionally -- when the feature is off it never
// dereferences the empty vector.

real mult_factor(int has_multiplicative_covariate, vector effect_multiplier, int x) {
  if (!has_multiplicative_covariate || x == 0) {
    return 1.0;
  }
  return effect_multiplier[1];
}
