// did_summary_model_data.stan

int<lower=0> n_studies_did_summary;
vector[n_studies_did_summary] x_bar_control_before_did_summary;
vector[n_studies_did_summary] x_bar_control_after_did_summary;
vector[n_studies_did_summary] x_bar_treatment_before_did_summary;
vector[n_studies_did_summary] x_bar_treatment_after_did_summary;
array[n_studies_did_summary] int sample_size_control_did_summary;
array[n_studies_did_summary] int sample_size_treatment_did_summary;
vector<lower=0>[n_studies_did_summary] sd_control_before_did_summary;
vector<lower=0>[n_studies_did_summary] sd_control_after_did_summary;
vector<lower=0>[n_studies_did_summary] sd_treatment_before_did_summary;
vector<lower=0>[n_studies_did_summary] sd_treatment_after_did_summary;
int<lower=0, upper=1> is_differenced_likelihood_did_summary;

// Change-only studies: only change means and SDs available (no pre/post split).
int<lower=0> n_studies_did_change_only;
vector[n_studies_did_change_only] x_bar_change_control_did_change_only;
vector[n_studies_did_change_only] x_bar_change_treatment_did_change_only;
array[n_studies_did_change_only] int sample_size_control_did_change_only;
array[n_studies_did_change_only] int sample_size_treatment_did_change_only;
vector<lower=0>[n_studies_did_change_only] sd_change_control_did_change_only;
vector<lower=0>[n_studies_did_change_only] sd_change_treatment_did_change_only;

// Rho: split into known (data) and missing (to be inferred) studies.
// When is_correlation_coefficient_hierarchical == 0, all rho are known:
//   n_rho_known = n_studies, n_rho_missing = 0,
//   idx_rho_known = {1, 2, ..., n_studies}, rho_known = {rho_1, ..., rho_n}.
// When is_correlation_coefficient_hierarchical == 1, partition studies:
//   idx_rho_known and idx_rho_missing are disjoint indices into 1:n_studies
//   identifying which studies have reported vs unreported rho.
int<lower=0> n_rho_known_did_summary;
int<lower=0> n_rho_missing_did_summary;
array[n_rho_known_did_summary] int<lower=1> idx_rho_known_did_summary;
array[n_rho_missing_did_summary] int<lower=1> idx_rho_missing_did_summary;
vector<lower=-1, upper=1>[n_rho_known_did_summary] rho_known_did_summary;

