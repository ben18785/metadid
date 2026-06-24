
time_trend_mean ~ normal(time_trend_mean_prior_mean, time_trend_mean_prior_sd);
time_trend_sd ~ cauchy(0, time_trend_sd_prior_scale);
if (!is_baseline_normalised) {
  if (report_baseline_fraction) {
    // Shared-constant normalisation divides every study by the grand-mean
    // baseline, so the POPULATION mean baseline is ~1 by construction. Pin the
    // mean tightly at 1 and keep the between-study SD light-tailed. Wide priors
    // here admit a degenerate bimodal mode where a chain sends baseline -> 0
    // (inflating sigma to compensate) and the fractional ratio explodes to
    // nonsense (observed Rhat ~2, baseline ~0.5 = average of the 1 and 0 modes).
    baseline_control_mean[1]   ~ normal(1, 0.1);
    baseline_control_sd[1]     ~ normal(0, 0.3);
    baseline_treatment_mean[1] ~ normal(1, 0.1);
    baseline_treatment_sd[1]   ~ normal(0, 0.3);
  } else {
    // Raw / absolute mode: data are in the user's original units, so keep
    // weakly-informative wide priors.
    baseline_control_mean[1]   ~ normal(0, 10);
    baseline_control_sd[1]     ~ cauchy(0, 5);
    baseline_treatment_mean[1] ~ normal(0, 10);
    baseline_treatment_sd[1]   ~ cauchy(0, 5);
  }
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
// baseline_difference is the per-study fractional imbalance (b_T - b_C)/b_C,
// which is ~0 on any scale. In shared mode the per-study baselines are
// estimated (looser identification than the fixed-baseline per-study path), so
// the heavy-tailed cauchy lets baseline_difference_sd run away (observed
// blowing up to ~4000, dragging the whole posterior to Rhat ~2). Use a
// light-tailed half-normal there; keep the cauchy for the per-study/raw paths.
if (report_baseline_fraction) {
  baseline_difference_sd ~ normal(0, 0.25);
} else {
  baseline_difference_sd ~ cauchy(0, baseline_difference_sd_prior_scale);
}

beta_cov ~ normal(0, beta_cov_prior_sd);

if (is_correlated_effects) {
  L_corr_theta_beta[1] ~ lkj_corr_cholesky(lkj_eta_prior);
}
