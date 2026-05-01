// prepost_summary_model_data.stan

int<lower=0> n_studies_pp_summary;
vector[n_studies_pp_summary] x_bar_treatment_before_pp_summary;
vector[n_studies_pp_summary] x_bar_treatment_after_pp_summary;
array[n_studies_pp_summary] int sample_size_treatment_pp_summary;
vector<lower=0>[n_studies_pp_summary] sd_treatment_before_pp_summary;
vector<lower=0>[n_studies_pp_summary] sd_treatment_after_pp_summary;
int<lower=0, upper=1> is_time_trend_pp_summary_zero;
int<lower=0, upper=1> is_differenced_likelihood_pp_summary;

// Rho: split into known (data) and missing (to be inferred) studies.
// When is_correlation_coefficient_hierarchical == 0, all rho are known:
//   n_rho_known = n_studies, n_rho_missing = 0,
//   idx_rho_known = {1, 2, ..., n_studies}, rho_known = {rho_1, ..., rho_n}.
// When is_correlation_coefficient_hierarchical == 1, partition studies:
//   idx_rho_known and idx_rho_missing are disjoint indices into 1:n_studies
//   identifying which studies have reported vs unreported rho.
int<lower=0> n_rho_known_pp_summary;
int<lower=0> n_rho_missing_pp_summary;
array[n_rho_known_pp_summary] int<lower=1> idx_rho_known_pp_summary;
array[n_rho_missing_pp_summary] int<lower=1> idx_rho_missing_pp_summary;
vector<lower=-1, upper=1>[n_rho_known_pp_summary] rho_known_pp_summary;

