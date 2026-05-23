
// shared_data.stan

// baseline_latent_mode controls how per-study baselines are handled.
//
// 1 = "treatment": modelled mode with treatment-pre baseline as the per-study
//     latent (wide uniform prior). Control-pre baseline derived via the
//     baseline-difference relation. Hierarchical pooling on the treatment-pre
//     canonical scale (treatment_effect_mean, time_trend_mean, and
//     baseline_difference_mean all interpreted as fractions of treatment-pre).
//
// 2 = "control": modelled mode with control-pre baseline as the per-study
//     latent (wide uniform prior). Treatment-pre baseline derived. Hierarchical
//     pooling stays on the treatment-pre canonical scale; parameter meanings
//     are mode-invariant between modes 1 and 2.
//
// 3 = "none": no per-study baseline latent; absolute-scale pooling. The
//     existing unnormalised mode behaviour is preserved (pop-level
//     baseline_control_mean / baseline_control_sd come back into use).
//
// In modelled modes (1 and 2) raw data are passed through and Stan performs
// the normalisation via the per-study latent baselines.
int<lower=1, upper=3> baseline_latent_mode;

// Wide uniform upper bound on the per-study latent baseline parameters.
// Computed in R as a large multiple of the observed baseline scale so that
// the prior is data-vague but proper. Only used in modelled modes.
real<lower=0> baseline_prior_upper;

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

// When is_baseline_difference_estimated == 1, each DiD and RCT study has its own
// baseline_difference[i], hierarchically pooled across studies with population
// (baseline_difference_mean, baseline_difference_sd). DiD studies identify
// baseline_difference[i] per-study from the pre-treatment vs pre-control means;
// RCT studies are not per-study identifiable and rely on the hierarchical prior.
// When 0, all baseline differences are fixed to zero and the treatment-arm
// baseline equals the control-arm baseline. PP studies are unaffected.
int<lower=0, upper=1> is_baseline_difference_estimated;
real baseline_difference_mean_prior_mean;
real<lower=0> baseline_difference_mean_prior_sd;
real<lower=0> baseline_difference_sd_prior_scale;

// Prior hyperparameters for study-level observation SDs (shared across all designs)
real<lower=0> sigma_prior_scale;

// Study-level covariates (meta-regression on treatment effect)
int<lower=0> K_cov;               // number of covariates (0 = no meta-regression)
real<lower=0> beta_cov_prior_sd;   // prior SD for covariate coefficients

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
