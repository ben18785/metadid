// did_model_transformed_parameters.stan
// Non-centered parameterization for study-level DiD (individual) parameters.
// Student-t and correlated-effects branches remain centered (identity pass-through).

vector[n_studies_did] treatment_effect_did;
vector[n_studies_did] time_trend_did;
vector[n_studies_did * (1 - is_baseline_normalised)] baseline_control_did;
vector[n_studies_did] baseline_difference_did;
vector[n_studies_did * (1 - is_baseline_normalised)] baseline_treatment_did;

if (!is_student_t_heterogeneity && !is_correlated_effects) {
  for (i in 1:n_studies_did)
    treatment_effect_did[i] = overall_mult(effect_multiplier, x_mult_did[i], effect_multiplier2, x_mult2_did[i]) * (treatment_effect_mean + X_cov_did[i] * beta_cov) + treatment_effect_sd * treatment_effect_did_raw[i];
} else {
  treatment_effect_did = treatment_effect_did_raw;
}

if (!is_correlated_effects) {
  for (i in 1:n_studies_did)
    time_trend_did[i] = time_trend_mean + time_trend_sd * time_trend_did_raw[i];
} else {
  time_trend_did = time_trend_did_raw;
}

for (i in 1:size(baseline_control_did_raw))
  baseline_control_did[i] = baseline_control_mean[1] + baseline_control_sd[1] * baseline_control_did_raw[i];

// s_i for individual-level studies is a TRANSFORMED PARAMETER, not data: the
// observation SDs are sampled here (sample sizes are still data). Making the
// gamma prior scale depend on sigma couples them in the prior, but not in the
// likelihood -- for a Gaussian the sample mean vector and sample covariance are
// independent, so sigma is pinned by within-arm dispersion while gamma moves
// only arm means. The upside over the summary path is that uncertainty in s_i
// is propagated rather than plugged in.
vector<lower=0>[n_studies_did] gamma_scale_did;
for (i in 1:n_studies_did)
  gamma_scale_did[i] = sqrt(
    square(sigma_treatment_before_did[i]) / sample_size_treatment_did[i] +
    square(sigma_control_before_did[i])   / sample_size_control_did[i]);

for (i in 1:n_studies_did)
  baseline_difference_did[i] = gamma_from_raw(
    gamma_mode_did[i], baseline_difference_did_raw[i],
    baseline_difference_mean, baseline_difference_sd,
    kappa, gamma_scale_did[i]);

// baseline_treatment_did exists only in unnormalised mode and always uses the
// hierarchical baseline_difference, since DiD studies identify it from data.
for (i in 1:size(baseline_treatment_did))
  baseline_treatment_did[i] = baseline_control_did[i] * (1 + baseline_difference_did[i]);
