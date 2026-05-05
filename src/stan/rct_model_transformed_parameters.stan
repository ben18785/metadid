// rct_model_transformed_parameters.stan
//
// When normalised with time trends estimated, derive the true treatment effect
// on the alpha-normalised scale from the apparent effect:
//   treatment_effect = apparent_effect * (1 + time_trend)
// When NOT normalised (or time trends forced to zero), treatment_effect_rct is
// sampled directly and this block simply aliases it.

vector[n_studies_rct] treatment_effect_rct_derived;
if (is_baseline_normalised && !is_time_trend_rct_zero) {
  for (i in 1:n_studies_rct)
    treatment_effect_rct_derived[i] = apparent_effect_rct[i] * (1 + time_trend_rct[i]);
} else {
  treatment_effect_rct_derived = treatment_effect_rct;
}
