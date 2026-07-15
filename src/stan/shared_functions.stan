// shared_functions.stan
//
// Helper applying the optional multiplicative covariate to a study's
// linear predictor. The covariate is categorical: level 0 is the
// reference (factor = 1); level x in {1, ..., n_effect_multipliers}
// selects the corresponding estimated factor, effect_multiplier[x].
// The simplest case is a two-level covariate (reference 0, one
// multiplier for level 1). When the feature is off every x is 0 (the
// data bounds enforce this), so the function never dereferences the
// empty effect_multiplier vector.

real mult_factor(vector effect_multiplier, int x) {
  if (x == 0) {
    return 1.0;
  }
  return effect_multiplier[x];
}

// Product of the (up to two) per-covariate factors for a study.
real overall_mult(vector effect_multiplier, int x1,
                  vector effect_multiplier2, int x2) {
  return mult_factor(effect_multiplier, x1) * mult_factor(effect_multiplier2, x2);
}
