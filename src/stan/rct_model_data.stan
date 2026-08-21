// rct_model.stan


int<lower=0> n_studies_rct;
array[n_studies_rct] int sample_size_control_rct;
array[n_studies_rct] int sample_size_treatment_rct;
array[n_studies_rct] int<lower=1> study_start_control_rct;
array[n_studies_rct] int<lower=1> study_end_control_rct;
array[n_studies_rct] int<lower=1> study_start_treatment_rct;
array[n_studies_rct] int<lower=1> study_end_treatment_rct;
vector[sum(sample_size_control_rct)] x_control_after_rct;
vector[sum(sample_size_treatment_rct)] x_treatment_after_rct;
int<lower=0, upper=1> is_time_trend_rct_zero;
matrix[n_studies_rct, K_cov] X_cov_rct;
array[n_studies_rct] int<lower=0, upper=n_effect_multipliers> x_mult_rct;
array[n_studies_rct] int<lower=0, upper=n_effect_multipliers2> x_mult2_rct;

// Baseline-imbalance mode per study (0 fixed zero, 1 non-randomised,
// 2 randomised -- see shared_data.stan). Unlike the summary-data blocks there
// is no gamma_scale here: the observation SDs are parameters, so s_i is built
// in the transformed parameters block instead.
array[n_studies_rct] int<lower=0, upper=2> gamma_mode_rct;
