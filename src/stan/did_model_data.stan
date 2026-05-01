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
