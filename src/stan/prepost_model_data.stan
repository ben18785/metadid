// prepost_model.stan

int<lower=0> n_studies_pp;
array[n_studies_pp] int sample_size_treatment_pp;
array[n_studies_pp] int<lower=1> study_start_treatment_pp;
array[n_studies_pp] int<lower=1> study_end_treatment_pp;
vector[sum(sample_size_treatment_pp)] x_treatment_before_pp;
vector[sum(sample_size_treatment_pp)] x_treatment_after_pp;
int<lower=0, upper=1> is_time_trend_pp_zero;
