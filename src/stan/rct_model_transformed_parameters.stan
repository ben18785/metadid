// rct_model_transformed_parameters.stan
// Non-centered parameterization for study-level RCT (individual) parameters,
// plus derivation of treatment_effect_rct_derived from apparent_effect when
// normalised with time trends.

vector[n_studies_rct * (1 - is_baseline_normalised * (1 - is_time_trend_rct_zero))] treatment_effect_rct;
vector[n_studies_rct * (1 - is_time_trend_rct_zero)] time_trend_rct;
vector[n_studies_rct * (1 - is_baseline_normalised)] baseline_control_rct;
vector[n_studies_rct * is_baseline_difference_estimated] baseline_difference_rct;
vector[n_studies_rct * (1 - is_baseline_normalised)] baseline_treatment_rct;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:size(treatment_effect_rct_raw))
    treatment_effect_rct[i] = treatment_effect_mean_rct + X_cov_rct[i] * beta_cov + treatment_effect_sd * treatment_effect_rct_raw[i];
} else {
  treatment_effect_rct = treatment_effect_rct_raw;
}

if (!is_correlated_effects) {
  for (i in 1:size(time_trend_rct_raw))
    time_trend_rct[i] = time_trend_mean + time_trend_sd * time_trend_rct_raw[i];
} else {
  time_trend_rct = time_trend_rct_raw;
}

for (i in 1:size(baseline_control_rct_raw))
  baseline_control_rct[i] = baseline_control_mean[1] + baseline_control_sd[1] * baseline_control_rct_raw[i];

for (i in 1:size(baseline_difference_rct_raw))
  baseline_difference_rct[i] = baseline_difference_mean + baseline_difference_sd * baseline_difference_rct_raw[i];

// baseline_treatment_rct exists only in unnormalised mode. When the imbalance
// is estimated it equals baseline_control * (1 + baseline_difference); otherwise
// it is constrained equal to baseline_control (was previously the
// is_baseline_control_equal_treatment_rct = 1 path).
for (i in 1:size(baseline_treatment_rct)) {
  if (is_baseline_difference_estimated)
    baseline_treatment_rct[i] = baseline_control_rct[i] * (1 + baseline_difference_rct[i]);
  else
    baseline_treatment_rct[i] = baseline_control_rct[i];
}

// Derive true treatment effect.
// In the normalised + non-zero time-trend branch: the data measures the apparent
// jump (gamma + theta) / (b_c + beta). apparent_total = apparent * (1 + time_trend)
// recovers gamma + theta on the normalised scale; subtracting baseline_difference
// isolates theta.
// In all other branches, treatment_effect_rct is sampled directly and aliases through.
vector[n_studies_rct] treatment_effect_rct_derived;
if (is_baseline_normalised && !is_time_trend_rct_zero) {
  for (i in 1:n_studies_rct) {
    real apparent_total = apparent_effect_rct[i] * (1 + time_trend_rct[i]);
    if (is_baseline_difference_estimated)
      treatment_effect_rct_derived[i] = apparent_total - baseline_difference_rct[i];
    else
      treatment_effect_rct_derived[i] = apparent_total;
  }
} else {
  treatment_effect_rct_derived = treatment_effect_rct;
}
