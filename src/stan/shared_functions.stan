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

// Reconstruct a study's baseline imbalance from its standard-normal raw value,
// according to that study's assignment mechanism:
//   mode 0: fixed at zero
//   mode 1: non-randomised -- mu_gamma + tau_gamma * raw
//   mode 2: randomised     -- kappa * s_i * raw, mean zero by construction
//
// This is a non-centred reconstruction in every branch: the density sits on
// `raw ~ std_normal()` and gamma is a deterministic transform, so no Jacobian
// is required even in mode 2 where the scale is itself a parameter.
//
// gamma_scale (s_i) is the sampling SD of the observed baseline contrast. It is
// DATA for summary-level studies (computed from reported SDs in R) but a
// TRANSFORMED PARAMETER for individual-level studies (the observation SDs are
// sampled there), which is why it is passed in rather than looked up.
real gamma_from_raw(int mode, real raw, real mu_gamma, real tau_gamma,
                    real kappa, real gamma_scale) {
  if (mode == 0) {
    return 0.0;
  }
  if (mode == 2) {
    return kappa * gamma_scale * raw;
  }
  return mu_gamma + tau_gamma * raw;
}
