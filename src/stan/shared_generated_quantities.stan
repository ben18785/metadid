// shared_generated_quantities.stan
//
// Two quantities that let external tooling work on a fit without refitting it:
//
//   lprior   the log density of the priors the USER chose, following the brms
//            convention. priorsense power-scales this to report how much each
//            posterior depends on those choices, from a single fit, by
//            importance sampling. Power-scaling by alpha is equivalent to
//            refitting with every prior scale multiplied by 1/sqrt(alpha), so
//            "widen all priors by a factor of 2" is alpha = 0.25.
//
//   log_lik  per-study log-likelihood, for loo() -- leave-one-STUDY-out
//            cross-validation, the right unit for a meta-analysis -- and for
//            the likelihood half of a power-scaling check.
//
// Deliberately excluded from lprior: the std_normal() terms on the non-centred
// raw parameters. Those encode the hierarchical structure rather than a prior
// anyone chose, and scaling them would quietly rescale the shrinkage itself.
// So the sensitivity reported is to the hyperpriors -- treatment_effect_mean,
// treatment_effect_sd, kappa, the multipliers and so on -- which is where the
// prior-driven behaviour in this model actually lives.
//
// Normalising constants are dropped where a distribution is truncated by its
// <lower=0> declaration. Power-scaling uses differences in lprior across
// draws, so any constant cancels.

// Partitions, so priorsense can power-scale a SUBSET of the priors:
// powerscale(..., selection = "effect") reads lprior_effect, while the default
// reads the total in lprior. Partitioning matters for validation as well as
// interpretation. Raising a Cauchy prior to the power alpha gives a Student-t
// with nu = 2*alpha - 1, not another Cauchy, so for a Cauchy prior there is no
// refit that reproduces power-scaling and the two routes cannot be compared.
// For a NORMAL prior, scaling by alpha is exactly a scale of 1/sqrt(alpha), so
// a partition containing only normal priors can be checked against a full
// refit. lprior_rho is such a partition, and so is lprior_effect once
// set_priors(treatment_effect_sd = normal(...)) is used.
real lprior_effect = 0;     // treatment effect: population mean and SD
real lprior_trend = 0;      // time trend: population mean and SD
real lprior_imbalance = 0;  // baseline imbalance: mean, SD, kappa
real lprior_rho = 0;        // hierarchical pre-post correlation (both normal)
real lprior_other = 0;      // everything else
real lprior = 0;

lprior_trend += normal_lpdf(time_trend_mean | time_trend_mean_prior_mean,
                                              time_trend_mean_prior_sd);
lprior_trend += is_time_trend_sd_normal
  ? normal_lpdf(time_trend_sd | 0, time_trend_sd_prior_scale)
  : cauchy_lpdf(time_trend_sd | 0, time_trend_sd_prior_scale);

lprior_effect += normal_lpdf(treatment_effect_mean | treatment_effect_mean_prior_mean,
                                                     treatment_effect_mean_prior_sd);
lprior_effect += is_treatment_effect_sd_normal
  ? normal_lpdf(treatment_effect_sd | 0, treatment_effect_sd_prior_scale)
  : cauchy_lpdf(treatment_effect_sd | 0, treatment_effect_sd_prior_scale);

lprior_imbalance += is_baseline_difference_sd_normal
  ? normal_lpdf(baseline_difference_sd | 0, baseline_difference_sd_prior_scale)
  : cauchy_lpdf(baseline_difference_sd | 0, baseline_difference_sd_prior_scale);

if (!is_baseline_normalised) {
  lprior_other += normal_lpdf(baseline_control_mean[1] | 0, 10);
  lprior_other += cauchy_lpdf(baseline_control_sd[1] | 0, 5);
  lprior_other += normal_lpdf(baseline_treatment_mean[1] | 0, 10);
  lprior_other += cauchy_lpdf(baseline_treatment_sd[1] | 0, 5);
}
if (is_correlation_coefficient_hierarchical) {
  lprior_rho += normal_lpdf(mu_z[1]  | mu_z_prior_mean,  mu_z_prior_sd);
  lprior_rho += normal_lpdf(tau_z[1] | tau_z_prior_mean, tau_z_prior_sd);
}
if (is_student_t_heterogeneity) {
  lprior_other += gamma_lpdf(nu_treatment_vec[1] | nu_prior_shape, nu_prior_rate);
}
if (is_design_effect) {
  lprior_other += normal_lpdf(delta_rct_raw[1] | 0, delta_rct_prior_sd);
  lprior_other += normal_lpdf(delta_pp_raw[1]  | 0, delta_pp_prior_sd);
}
if (is_mu_gamma_estimated) {
  lprior_imbalance += normal_lpdf(mu_gamma_vec[1] | baseline_difference_mean_prior_mean,
                                                    baseline_difference_mean_prior_sd);
}
if (is_kappa_estimated) {
  lprior_imbalance += normal_lpdf(kappa_vec[1] | 0, kappa_prior_scale);
}
if (K_cov > 0) {
  lprior_other += normal_lpdf(beta_cov | 0, beta_cov_prior_sd);
}
if (n_effect_multipliers > 0) {
  lprior_other += lognormal_lpdf(effect_multiplier | effect_multiplier_prior_meanlog,
                                                     effect_multiplier_prior_sdlog);
}
if (n_effect_multipliers2 > 0) {
  lprior_other += lognormal_lpdf(effect_multiplier2 | effect_multiplier_prior_meanlog,
                                                      effect_multiplier_prior_sdlog);
}
if (is_correlated_effects) {
  lprior_other += lkj_corr_cholesky_lpdf(L_corr_theta_beta[1] | lkj_eta_prior);
}
// Observation SDs for the individual-level designs. Zero-length vectors when
// those designs are absent, so these terms then contribute nothing.
lprior_other += cauchy_lpdf(sigma_control_before_did   | 0, sigma_prior_scale);
lprior_other += cauchy_lpdf(sigma_control_after_did    | 0, sigma_prior_scale);
lprior_other += cauchy_lpdf(sigma_treatment_before_did | 0, sigma_prior_scale);
lprior_other += cauchy_lpdf(sigma_treatment_after_did  | 0, sigma_prior_scale);
lprior_other += cauchy_lpdf(sigma_control_after_rct    | 0, sigma_prior_scale);
lprior_other += cauchy_lpdf(sigma_treatment_after_rct  | 0, sigma_prior_scale);
lprior_other += cauchy_lpdf(sigma_treatment_before_pp  | 0, sigma_prior_scale);
lprior_other += cauchy_lpdf(sigma_treatment_after_pp   | 0, sigma_prior_scale);

// The total priorsense reads when no selection is given. It must equal the sum
// of the partitions, which test-sensitivity.R checks draw by draw.
lprior = lprior_effect + lprior_trend + lprior_imbalance + lprior_rho
         + lprior_other;

// ---------------------------------------------------------------------------
// log_lik, one entry per study
// ---------------------------------------------------------------------------
// Summary-level designs only. An individual-level study's contribution would
// need its arm-level data re-segmented here, and a log_lik that silently
// omitted some studies would give loo() and any likelihood power-scaling the
// wrong answer -- so when individual-level studies are present this is emitted
// at length zero instead, and the tooling fails loudly rather than quietly.
// lprior is unaffected, so prior sensitivity still works for any fit.

// Sizes must be data-only expressions, so the counts are inlined here rather
// than read from locals.
vector[(n_studies_did + n_studies_rct + n_studies_pp) == 0
       ? (n_studies_did_summary + n_studies_rct_summary
          + n_studies_pp_summary + n_studies_did_change_only)
       : 0] log_lik;

if ((n_studies_did + n_studies_rct + n_studies_pp) == 0) {
  int pos = 1;

  // --- DiD summary ---------------------------------------------------------
  for (i in 1:n_studies_did_summary) {
    real rho_i;
    // rho_eff is a local in the model block; rebuild it the same way.
    rho_i = 0;
    for (k in 1:n_rho_known_did_summary)
      if (idx_rho_known_did_summary[k] == i) rho_i = rho_known_did_summary[k];
    for (k in 1:n_rho_missing_did_summary)
      if (idx_rho_missing_did_summary[k] == i)
        rho_i = tanh(z_rho_missing_did_summary[k]);

    if (!is_differenced_likelihood_did_summary && is_baseline_normalised) {
      log_lik[pos] = did_summary_study_normalised_lpdf_from_data(
        x_bar_control_after_did_summary[i],
        x_bar_treatment_before_did_summary[i],
        x_bar_treatment_after_did_summary[i],
        baseline_difference_did_summary[i],
        time_trend_did_summary[i],
        treatment_effect_did_summary[i],
        sd_control_before_did_summary[i], sd_control_after_did_summary[i],
        sd_treatment_before_did_summary[i], sd_treatment_after_did_summary[i],
        rho_i,
        sample_size_control_did_summary[i], sample_size_treatment_did_summary[i]);
    } else if (!is_differenced_likelihood_did_summary) {
      log_lik[pos] = did_summary_study_lpdf_from_data(
        x_bar_control_before_did_summary[i], x_bar_control_after_did_summary[i],
        x_bar_treatment_before_did_summary[i], x_bar_treatment_after_did_summary[i],
        baseline_control_did_summary[i],
        baseline_control_did_summary[i] * (1 + baseline_difference_did_summary[i]),
        time_trend_did_summary[i], treatment_effect_did_summary[i],
        sd_control_before_did_summary[i], sd_control_after_did_summary[i],
        sd_treatment_before_did_summary[i], sd_treatment_after_did_summary[i],
        rho_i,
        sample_size_control_did_summary[i], sample_size_treatment_did_summary[i]);
    } else {
      log_lik[pos] = did_summary_study_lpdf_from_data_differenced_form(
        x_bar_control_before_did_summary[i], x_bar_control_after_did_summary[i],
        x_bar_treatment_before_did_summary[i], x_bar_treatment_after_did_summary[i],
        treatment_effect_did_summary[i],
        sd_control_before_did_summary[i], sd_control_after_did_summary[i],
        sd_treatment_before_did_summary[i], sd_treatment_after_did_summary[i],
        rho_i,
        sample_size_control_did_summary[i], sample_size_treatment_did_summary[i]);
    }
    pos += 1;
  }

  // --- DiD change-only -----------------------------------------------------
  for (i in 1:n_studies_did_change_only) {
    log_lik[pos] = did_summary_study_lpdf_from_change_data(
      x_bar_change_control_did_change_only[i],
      x_bar_change_treatment_did_change_only[i],
      treatment_effect_did_change_only[i],
      sd_change_control_did_change_only[i], sd_change_treatment_did_change_only[i],
      sample_size_control_did_change_only[i],
      sample_size_treatment_did_change_only[i]);
    pos += 1;
  }

  // --- RCT summary ---------------------------------------------------------
  for (i in 1:n_studies_rct_summary) {
    if (is_baseline_normalised && !is_time_trend_rct_summary_zero) {
      log_lik[pos] = rct_summary_study_normalised_lpdf_from_data(
        x_bar_treatment_after_rct_summary[i],
        apparent_effect_rct_summary[i],
        sd_treatment_after_rct_summary[i], sample_size_treatment_rct_summary[i],
        sd_control_after_rct_summary[i], sample_size_control_rct_summary[i]);
    } else if (is_baseline_normalised) {
      log_lik[pos] = rct_summary_study_normalised_lpdf_from_data(
        x_bar_treatment_after_rct_summary[i],
        baseline_difference_rct_summary[i] + treatment_effect_rct_summary[i],
        sd_treatment_after_rct_summary[i], sample_size_treatment_rct_summary[i],
        sd_control_after_rct_summary[i], sample_size_control_rct_summary[i]);
    } else {
      log_lik[pos] = rct_summary_study_lpdf_from_data(
        x_bar_control_after_rct_summary[i], x_bar_treatment_after_rct_summary[i],
        baseline_control_rct_summary[i], baseline_treatment_rct_summary[i],
        is_time_trend_rct_summary_zero ? 0 : time_trend_rct_summary[i],
        treatment_effect_rct_summary[i],
        sd_control_after_rct_summary[i], sd_treatment_after_rct_summary[i],
        sample_size_control_rct_summary[i], sample_size_treatment_rct_summary[i]);
    }
    pos += 1;
  }

  // --- Pre-post summary ----------------------------------------------------
  for (i in 1:n_studies_pp_summary) {
    real rho_i = 0;
    for (k in 1:n_rho_known_pp_summary)
      if (idx_rho_known_pp_summary[k] == i) rho_i = rho_known_pp_summary[k];
    for (k in 1:n_rho_missing_pp_summary)
      if (idx_rho_missing_pp_summary[k] == i)
        rho_i = tanh(z_rho_missing_pp_summary[k]);

    real tt_i = is_time_trend_pp_summary_zero ? 0 : time_trend_pp_summary[i];

    if (!is_differenced_likelihood_pp_summary) {
      log_lik[pos] = prepost_summary_study_lpdf_from_data(
        x_bar_treatment_before_pp_summary[i], x_bar_treatment_after_pp_summary[i],
        is_baseline_normalised ? 1.0 : baseline_treatment_pp_summary[i],
        tt_i, treatment_effect_pp_summary[i],
        sd_treatment_before_pp_summary[i], sd_treatment_after_pp_summary[i],
        rho_i, sample_size_treatment_pp_summary[i]);
    } else {
      log_lik[pos] = prepost_summary_study_lpdf_from_data_differenced_form(
        x_bar_treatment_before_pp_summary[i], x_bar_treatment_after_pp_summary[i],
        tt_i, treatment_effect_pp_summary[i],
        sd_treatment_before_pp_summary[i], sd_treatment_after_pp_summary[i],
        rho_i, sample_size_treatment_pp_summary[i]);
    }
    pos += 1;
  }
}

// ---------------------------------------------------------------------------
// log_lik_marginal, one entry per study
// ---------------------------------------------------------------------------
// The study-level parameters integrated out, which is what makes a per-study
// PSIS diagnostic interpretable (see did_summary_study_marginal_lpdf). Emitted
// only where the integral is EXACT, and at length zero otherwise, so the R side
// refuses rather than quietly reporting a first-order approximation as if it
// were the real thing. Exactness needs:
//
//   * summary-level data only (an individual study's per-arm sigmas sit in the
//     variance, not the mean, so linearity does not help);
//   * baseline normalisation, so the baselines are pinned and gamma enters
//     additively rather than multiplying an estimated baseline;
//   * a normal heterogeneity distribution (a Student-t would need quadrature);
//   * correlated effects are handled: the beta/theta covariance enters the
//     latent covariance directly;
//   * every study reporting rho, since an imputed rho_i is itself informed only
//     by its own study and would need integrating out too;
//   * post-only studies with an ESTIMATED trend are included but only to first
//     order: the apparent-effect reparameterisation divides by (1 + beta_tilde),
//     so the marginal mean is a ratio of normals rather than a normal. The
//     delta-method expression below is used instead, and marginal_exact is set
//     to 0 so the R side warns. The neglected terms go as the fourth power of
//     tau_beta / (1 + mu_beta), a ratio of about 0.05 in practice;
//   * pre-post studies using the differenced likelihood (the default).

vector[(n_studies_did + n_studies_rct + n_studies_pp) == 0
       && is_baseline_normalised == 1
       && is_student_t_heterogeneity == 0
       && n_rho_missing_did_summary == 0
       && n_rho_missing_pp_summary == 0
       && is_differenced_likelihood_did_summary == 0
       && (n_studies_pp_summary == 0 || is_differenced_likelihood_pp_summary == 1)
       ? (n_studies_did_summary + n_studies_rct_summary
          + n_studies_pp_summary + n_studies_did_change_only)
       : 0] log_lik_marginal;

// 1 when every entry of log_lik_marginal is exact, 0 when any study used the
// first-order approximation. study_influence() warns on 0 rather than refusing:
// an approximate diagnostic that says so beats no diagnostic.
int marginal_exact = (n_studies_rct_summary == 0
                      || is_time_trend_rct_summary_zero == 1) ? 1 : 0;

if (size(log_lik_marginal) > 0) {
  int mpos = 1;
  real tau_theta = treatment_effect_sd;
  real tau_beta  = time_trend_sd;
  // Covariance between a study's trend and its effect, zero unless the
  // correlated-effects prior is in use.
  real cov_bt = is_correlated_effects
    ? L_corr_theta_beta[1][2, 1] * tau_theta * tau_beta : 0;

  for (i in 1:n_studies_did_summary) {
    real rho_i = 0;
    for (k in 1:n_rho_known_did_summary)
      if (idx_rho_known_did_summary[k] == i) rho_i = rho_known_did_summary[k];
    // gamma's population differs by mode: pinned at zero, a fitted population,
    // or zero-centred at kappa * s_i for a randomised study.
    real mu_g = gamma_mode_did_summary[i] == 1 ? baseline_difference_mean : 0;
    real var_g = gamma_mode_did_summary[i] == 0
      ? 0
      : (gamma_mode_did_summary[i] == 1
         ? square(baseline_difference_sd)
         : square(kappa * gamma_scale_did_summary[i]));
    real mu_th = overall_mult(effect_multiplier, x_mult_did_summary[i],
                              effect_multiplier2, x_mult2_did_summary[i])
                 * (treatment_effect_mean + X_cov_did_summary[i] * beta_cov);
    log_lik_marginal[mpos] = did_summary_study_marginal_lpdf_from_data(
      x_bar_control_after_did_summary[i],
      x_bar_treatment_before_did_summary[i],
      x_bar_treatment_after_did_summary[i],
      mu_g, var_g, time_trend_mean, tau_beta, mu_th, tau_theta, cov_bt,
      sd_control_before_did_summary[i], sd_control_after_did_summary[i],
      sd_treatment_before_did_summary[i], sd_treatment_after_did_summary[i],
      rho_i,
      sample_size_control_did_summary[i], sample_size_treatment_did_summary[i]);
    mpos += 1;
  }

  // Change-only: the double difference estimates theta alone, so the marginal
  // is the textbook random-effects form, N(mu_theta, s^2 + tau_theta^2).
  for (i in 1:n_studies_did_change_only) {
    real s2 = square(sd_change_control_did_change_only[i])
                / sample_size_control_did_change_only[i]
              + square(sd_change_treatment_did_change_only[i])
                / sample_size_treatment_did_change_only[i];
    real mu_th = overall_mult(effect_multiplier, x_mult_did_change_only[i],
                              effect_multiplier2, x_mult2_did_change_only[i])
                 * (treatment_effect_mean + X_cov_did_change_only[i] * beta_cov);
    log_lik_marginal[mpos] = normal_lpdf(
      x_bar_change_treatment_did_change_only[i]
        - x_bar_change_control_did_change_only[i]
      | mu_th, sqrt(s2 + square(tau_theta)));
    mpos += 1;
  }

  // Post-only. With the trend pinned the normalised treatment mean estimates
  // 1 + gamma + theta exactly, so gamma's population variance simply adds to
  // theta's. With the trend estimated the model's data measures the apparent
  // effect (theta + gamma)/(1 + beta), a ratio of normals, so the delta method
  // stands in: the numerator's variance is divided by (1 + mu_beta)^2 and the
  // denominator contributes (mu_theta + mu_gamma)^2 / (1 + mu_beta)^4 times
  // tau_beta^2. That is the same expansion used for the normalisation
  // denominator elsewhere in the model.
  for (i in 1:n_studies_rct_summary) {
    real se2 = square(sd_treatment_after_rct_summary[i])
                 / sample_size_treatment_rct_summary[i]
               + square(x_bar_treatment_after_rct_summary[i])
                 * square(sd_control_after_rct_summary[i])
                 / sample_size_control_rct_summary[i];
    real mu_g = gamma_mode_rct_summary[i] == 1 ? baseline_difference_mean : 0;
    real var_g = gamma_mode_rct_summary[i] == 0
      ? 0
      : (gamma_mode_rct_summary[i] == 1
         ? square(baseline_difference_sd)
         : square(kappa * gamma_scale_rct_summary[i]));
    real mu_th = overall_mult(effect_multiplier, x_mult_rct_summary[i],
                              effect_multiplier2, x_mult2_rct_summary[i])
                 * (treatment_effect_mean_rct + X_cov_rct_summary[i] * beta_cov);
    real mu_i;
    real var_i;
    if (is_time_trend_rct_summary_zero) {
      mu_i  = mu_g + mu_th;
      var_i = var_g + square(tau_theta);
    } else {
      real denom = 1 + time_trend_mean;
      mu_i  = (mu_th + mu_g) / denom;
      var_i = (square(tau_theta) + var_g) / square(denom)
              + square(mu_th + mu_g) / pow(denom, 4) * square(tau_beta)
              - 2 * cov_bt * (mu_th + mu_g) / pow(denom, 3);
    }
    log_lik_marginal[mpos] = normal_lpdf(
      x_bar_treatment_after_rct_summary[i] | 1 + mu_i, sqrt(se2 + var_i));
    mpos += 1;
  }

  // Pre-post, differenced: the change estimates beta + theta, so both
  // population variances add. pp_trend_shift moves the trend mean.
  for (i in 1:n_studies_pp_summary) {
    real rho_i = 0;
    for (k in 1:n_rho_known_pp_summary)
      if (idx_rho_known_pp_summary[k] == i) rho_i = rho_known_pp_summary[k];
    real s2 = (square(sd_treatment_before_pp_summary[i])
               + square(sd_treatment_after_pp_summary[i])
               - 2 * rho_i * sd_treatment_before_pp_summary[i]
                 * sd_treatment_after_pp_summary[i])
              / sample_size_treatment_pp_summary[i];
    real mu_th = overall_mult(effect_multiplier, x_mult_pp_summary[i],
                              effect_multiplier2, x_mult2_pp_summary[i])
                 * (treatment_effect_mean_pp + X_cov_pp_summary[i] * beta_cov);
    real mu_b = is_time_trend_pp_summary_zero
      ? 0 : time_trend_mean + pp_trend_shift;
    // The change estimates beta + theta, so a positive correlation between them
    // widens the marginal by twice their covariance.
    real var_b = is_time_trend_pp_summary_zero
      ? 0 : square(tau_beta) + 2 * cov_bt;
    log_lik_marginal[mpos] = normal_lpdf(
      x_bar_treatment_after_pp_summary[i] - x_bar_treatment_before_pp_summary[i]
      | mu_b + mu_th, sqrt(s2 + var_b + square(tau_theta)));
    mpos += 1;
  }
}
