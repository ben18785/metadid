
real time_trend_mean;
real<lower=0> time_trend_sd;

vector[1 - is_baseline_normalised] baseline_control_mean;
vector<lower=0>[1 - is_baseline_normalised] baseline_control_sd;

vector[1 - is_baseline_normalised] baseline_treatment_mean;
vector<lower=0>[1 - is_baseline_normalised] baseline_treatment_sd;

real treatment_effect_mean;
real treatment_effect_sd;

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
