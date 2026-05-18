
time_trend_mean ~ normal(time_trend_mean_prior_mean, time_trend_mean_prior_sd);
time_trend_sd ~ cauchy(0, time_trend_sd_prior_scale);
if (!is_baseline_normalised) {
  baseline_control_mean[1] ~ normal(0, 10);
  baseline_control_sd[1] ~ cauchy(0, 5);
  baseline_treatment_mean[1] ~ normal(0, 10);
  baseline_treatment_sd[1] ~ cauchy(0, 5);
}
treatment_effect_mean ~ normal(treatment_effect_mean_prior_mean, treatment_effect_mean_prior_sd);
treatment_effect_sd ~ cauchy(0, treatment_effect_sd_prior_scale);

if (is_correlation_coefficient_hierarchical) {
  mu_z ~ normal(mu_z_prior_mean, mu_z_prior_sd);
  tau_z ~ normal(tau_z_prior_mean, tau_z_prior_sd);
}

if (is_student_t_heterogeneity) {
  nu_treatment_vec ~ gamma(nu_prior_shape, nu_prior_rate);
}

if (is_design_effect) {
  delta_rct_raw ~ normal(0, delta_rct_prior_sd);
  delta_pp_raw  ~ normal(0, delta_pp_prior_sd);
}

beta_cov ~ normal(0, beta_cov_prior_sd);

// Multiplicative covariates share a single prior across all K_mult elements,
// mirroring how beta_cov shares one prior across all additive covariates.
// When K_mult == 0 this is a no-op on a zero-length vector.
gamma_mult ~ normal(gamma_mult_prior_mean, gamma_mult_prior_sd);

if (is_correlated_effects) {
  L_corr_theta_beta[1] ~ lkj_corr_cholesky(lkj_eta_prior);
}
