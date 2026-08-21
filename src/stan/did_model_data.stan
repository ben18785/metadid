// did_model.stan

int<lower=0> n_studies_did;
array[n_studies_did] int sample_size_control_did;
array[n_studies_did] int sample_size_treatment_did;
array[n_studies_did] int<lower=1> study_start_control_did;
array[n_studies_did] int<lower=1> study_end_control_did;
array[n_studies_did] int<lower=1> study_start_treatment_did;
array[n_studies_did] int<lower=1> study_end_treatment_did;
vector[sum(sample_size_control_did)] x_control_before_did;
vector[sum(sample_size_control_did)] x_control_after_did;
vector[sum(sample_size_treatment_did)] x_treatment_before_did;
vector[sum(sample_size_treatment_did)] x_treatment_after_did;
matrix[n_studies_did, K_cov] X_cov_did;
array[n_studies_did] int<lower=0, upper=n_effect_multipliers> x_mult_did;
array[n_studies_did] int<lower=0, upper=n_effect_multipliers2> x_mult2_did;

// Baseline-imbalance mode per study (0 fixed zero, 1 non-randomised,
// 2 randomised -- see shared_data.stan). Unlike the summary-data blocks there
// is no gamma_scale here: the observation SDs are parameters, so s_i is built
// in the transformed parameters block instead.
array[n_studies_did] int<lower=0, upper=2> gamma_mode_did;
