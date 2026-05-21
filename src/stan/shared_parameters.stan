
real time_trend_mean;
real<lower=0> time_trend_sd;

// Pop-level baselines used only in "none" mode for the hierarchical pooling
// of per-study baselines on the absolute scale. In modelled modes (1 and 2)
// these are sized 0 — per-study baselines are inferred as latent parameters
// in each design's parameters file instead.
vector[is_none_mode] baseline_control_mean;
vector<lower=0>[is_none_mode] baseline_control_sd;

vector[is_none_mode] baseline_treatment_mean;
vector<lower=0>[is_none_mode] baseline_treatment_sd;

// Population-level baseline imbalance (treatment vs control) on the normalised
// fractional scale. DiD studies always contribute (per-study identification);
// RCT studies contribute only when is_baseline_difference_estimated == 1. PP
// does not contribute (no control arm).
real baseline_difference_mean;
real<lower=0> baseline_difference_sd;

real treatment_effect_mean;
real<lower=0> treatment_effect_sd;

// Hierarchical rho parameters (always declared; unused when flag is 0)
real mu_z;
real<lower=0> tau_z;

// Degrees of freedom for Student-t between-study heterogeneity.
// Length 1 when is_student_t_heterogeneity == 1, length 0 otherwise (not sampled).
vector<lower=2>[is_student_t_heterogeneity] nu_treatment_vec;

// Design-level offsets on treatment_effect_mean (RCT and Pre-Post vs DiD reference).
// Length 1 when is_design_effect == 1, length 0 otherwise (not sampled).
vector[is_design_effect] delta_rct_raw;
vector[is_design_effect] delta_pp_raw;

// Study-level covariate regression coefficients (meta-regression).
// Length K_cov; when K_cov == 0, not sampled.
vector[K_cov] beta_cov;

// Cholesky factor of the 2x2 correlation matrix between treatment effects
// and time trends. Zero-length array when is_correlated_effects == 0 (not sampled).
array[is_correlated_effects] cholesky_factor_corr[2] L_corr_theta_beta;
