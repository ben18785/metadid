
int<lower=0> n_studies_rct_summary;
vector[n_studies_rct_summary] x_bar_control_after_rct_summary;
vector[n_studies_rct_summary] x_bar_treatment_after_rct_summary;
array[n_studies_rct_summary] int sample_size_control_rct_summary;
array[n_studies_rct_summary] int sample_size_treatment_rct_summary;
vector<lower=0>[n_studies_rct_summary] sd_control_after_rct_summary;
vector<lower=0>[n_studies_rct_summary] sd_treatment_after_rct_summary;
int<lower=0, upper=1> is_time_trend_rct_summary_zero;
matrix[n_studies_rct_summary, K_cov] X_cov_rct_summary;
array[n_studies_rct_summary] int<lower=0, upper=n_effect_multipliers> x_mult_rct_summary;
array[n_studies_rct_summary] int<lower=0, upper=n_effect_multipliers2> x_mult2_rct_summary;

// Baseline-imbalance mode per study (0 fixed zero, 1 non-randomised,
// 2 randomised -- see shared_data.stan) and s_i, the sampling SD of the
// observed baseline contrast. For summary data s_i is computed in R from the
// reported post-treatment SDs and sample sizes, AFTER any baseline normalisation, so
// it is on the same scale as gamma. It is only consulted for mode-2 studies.
array[n_studies_rct_summary] int<lower=0, upper=2> gamma_mode_rct_summary;
vector<lower=0>[n_studies_rct_summary] gamma_scale_rct_summary;
