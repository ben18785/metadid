
// shared_data.stan

// When is_baseline_normalised == 1, pre-treatment baselines are fixed at 1
// for all designs with pre-treatment data (DiD, Pre-Post).
// Data should be divided by each group's pre-treatment mean before fitting.
int<lower=0, upper=1> is_baseline_normalised;

// When is_correlation_coefficient_hierarchical == 1, model rho hierarchically:
//   z_i ~ normal(mu_z, sqrt(tau_z^2 + 1/(n_i - 3)))
//   rho_i = tanh(z_i)
// Studies with known rho contribute as data; missing rho studies are inferred.
// Individual-level models (did, prepost) also participate in the hierarchy.
int<lower=0, upper=1> is_correlation_coefficient_hierarchical;

// Prior hyperparameters for hierarchical rho (only used when flag == 1)
real mu_z_prior_mean;
real<lower=0> mu_z_prior_sd;
real tau_z_prior_mean;
real<lower=0> tau_z_prior_sd;

// Prior hyperparameters for population-level treatment effect
real treatment_effect_mean_prior_mean;
real<lower=0> treatment_effect_mean_prior_sd;
real<lower=0> treatment_effect_sd_prior_scale;

// Prior hyperparameters for population-level time trend
real time_trend_mean_prior_mean;
real<lower=0> time_trend_mean_prior_sd;
real<lower=0> time_trend_sd_prior_scale;

// Prior hyperparameters for nu (only used when is_student_t_heterogeneity == 1)
real<lower=0> nu_prior_shape;
real<lower=0> nu_prior_rate;

// Prior hyperparameters for design offsets (only used when is_design_effect == 1)
real<lower=0> delta_rct_prior_sd;
real<lower=0> delta_pp_prior_sd;

// Baseline imbalance (gamma). Each DiD and RCT study carries a per-study
// gamma_mode_*[i] (declared alongside that design's data) selecting how its
// baseline difference is modelled:
//   0 = fixed at zero
//   1 = non-randomised: gamma_i ~ normal(mu_gamma, baseline_difference_sd)
//   2 = randomised:     gamma_i ~ normal(0, kappa * gamma_scale_i)
// PP and change-only studies have no gamma (no control arm / it cancels).
//
// When is_mu_gamma_estimated == 0 (the default), mu_gamma is pinned at zero:
// the MAGNITUDE of selection-driven imbalance is pooled across non-randomised
// studies, but its DIRECTION is not transported between them. Selection
// direction is a property of each study's targeting rule, not of the outcome,
// so a literature has no common sign to borrow. Set to 1 only when the studies
// plausibly share a targeting mechanism.
int<lower=0, upper=1> is_mu_gamma_estimated;
real baseline_difference_mean_prior_mean;
real<lower=0> baseline_difference_mean_prior_sd;
real<lower=0> baseline_difference_sd_prior_scale;

// kappa: excess-imbalance factor for RANDOMISED studies. The observed baseline
// contrast of a randomised study has total variance (1 + kappa^2) * s_i^2, so
// kappa^2 is the variance inflation beyond simple random sampling:
// kappa^2 = DEFF - 1 = (m - 1) * ICC for cluster randomisation. kappa = 0 is
// perfect individual randomisation (the realised imbalance is then already
// carried by the likelihood's sigma^2/n terms and needs no extra parameter).
//
// kappa is identified only by randomised studies that carry PRE-treatment data.
// For post-only randomised studies gamma_i is unidentified and the mechanism
// acts purely as a sample-size-dependent variance inflation on that study's
// effect. When nothing anchors kappa it is fixed at kappa_fixed instead of
// sampled (R refuses to estimate it in that case).
int<lower=0, upper=1> is_kappa_estimated;
real<lower=0> kappa_fixed;
real<lower=0> kappa_prior_scale;

// Prior hyperparameters for study-level observation SDs (shared across all designs)
real<lower=0> sigma_prior_scale;

// Study-level covariates (meta-regression on treatment effect)
int<lower=0> K_cov;               // number of covariates (0 = no meta-regression)
real<lower=0> beta_cov_prior_sd;   // prior SD for covariate coefficients

// Optional categorical multiplicative covariate(s): up to two, whose per-study
// level codes (x_mult_*, x_mult2_*; 0 = reference) select factors whose product
// scales the effect. n_effect_multipliers* = 0 turns a covariate off; the
// log-normal prior hyperparameters are on the log scale.
int<lower=0> n_effect_multipliers;
int<lower=0> n_effect_multipliers2;
real effect_multiplier_prior_meanlog;
real<lower=0> effect_multiplier_prior_sdlog;

// When is_student_t_heterogeneity == 1, study-level treatment effects are
// drawn from a Student-t rather than a normal. The degrees-of-freedom
// parameter nu_treatment (declared as a length-1 vector in the parameters
// block) is estimated with a gamma(2, 0.1) prior.
// This prior is given here (by Aki Vehtari): https://statmodeling.stat.columbia.edu/2015/05/17/do-we-have-any-recommendations-for-priors-for-student_ts-degrees-of-freedom-parameter/
// When 0, a normal prior is used and nu_treatment is not sampled.
int<lower=0, upper=1> is_student_t_heterogeneity;

// When is_design_effect == 1, RCT and Pre-Post studies have additive offsets
// on the population treatment effect mean (delta_rct, delta_pp) relative to
// DiD (the reference). Offsets are estimated with normal(0, 10) priors.
// When 0, the offsets are not sampled and all designs share treatment_effect_mean.
int<lower=0, upper=1> is_design_effect;

// When is_correlated_effects == 1, study-level treatment effects and time
// trends are drawn jointly from a bivariate normal with a shared correlation
// parameter. The correlation is parameterised via a Cholesky factor of the
// 2x2 correlation matrix with an LKJ prior.
// When 0, treatment effects and time trends have independent priors.
int<lower=0, upper=1> is_correlated_effects;
real<lower=0> lkj_eta_prior;
