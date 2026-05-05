
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

// Prior hyperparameters for study-level observation SDs (shared across all designs)
real<lower=0> sigma_prior_scale;

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
