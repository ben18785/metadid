
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

baseline_difference_mean ~ normal(baseline_difference_mean_prior_mean,
                                  baseline_difference_mean_prior_sd);
baseline_difference_sd   ~ cauchy(0, baseline_difference_sd_prior_scale);

beta_cov ~ normal(0, beta_cov_prior_sd);

if (n_effect_multipliers > 0) {
  effect_multiplier ~ lognormal(effect_multiplier_prior_meanlog, effect_multiplier_prior_sdlog);
}
if (n_effect_multipliers2 > 0) {
  effect_multiplier2 ~ lognormal(effect_multiplier_prior_meanlog, effect_multiplier_prior_sdlog);
}

if (is_correlated_effects) {
  L_corr_theta_beta[1] ~ lkj_corr_cholesky(lkj_eta_prior);
}
