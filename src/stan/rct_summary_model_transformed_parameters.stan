// rct_summary_model_transformed_parameters.stan
//
// Same logic as individual RCT: derive true treatment effect from apparent
// when normalised with time trends estimated.

vector[n_studies_rct_summary] treatment_effect_rct_summary_derived;
if (is_baseline_normalised && !is_time_trend_rct_summary_zero) {
  for (i in 1:n_studies_rct_summary)
    treatment_effect_rct_summary_derived[i] = apparent_effect_rct_summary[i] * (1 + time_trend_rct_summary[i]);
} else {
  treatment_effect_rct_summary_derived = treatment_effect_rct_summary;
}
