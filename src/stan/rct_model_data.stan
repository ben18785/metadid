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
int<lower=0, upper=1> is_baseline_control_rct_zero;
int<lower=0, upper=1> is_baseline_treatment_rct_zero;
int<lower=0, upper=1> is_time_trend_rct_zero;
