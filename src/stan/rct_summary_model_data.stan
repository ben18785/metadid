
int<lower=0> n_studies_rct_summary;
vector[n_studies_rct_summary] x_bar_control_after_rct_summary;
vector[n_studies_rct_summary] x_bar_treatment_after_rct_summary;
array[n_studies_rct_summary] int sample_size_control_rct_summary;
array[n_studies_rct_summary] int sample_size_treatment_rct_summary;
vector<lower=0>[n_studies_rct_summary] sd_control_after_rct_summary;
vector<lower=0>[n_studies_rct_summary] sd_treatment_after_rct_summary;
int<lower=0, upper=1> is_baseline_control_equal_treatment_rct_summary;
int<lower=0, upper=1> is_time_trend_rct_summary_zero;
