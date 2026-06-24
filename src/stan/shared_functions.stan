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

// Overall multiplicative factor for a study: the product of up to two
// per-covariate factors. Each covariate contributes 1.0 at its reference level
// (code 0) or when that covariate is absent (its effect_multiplier vector is
// empty), so a single-covariate fit reduces exactly to mult_factor() alone.
real overall_mult(vector effect_multiplier, int x1,
                  vector effect_multiplier2, int x2) {
  return mult_factor(effect_multiplier, x1) * mult_factor(effect_multiplier2, x2);
}
