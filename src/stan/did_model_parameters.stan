// did_model.stan

vector[n_studies_did] treatment_effect_did;
vector[n_studies_did * (1 - is_baseline_normalised)] baseline_control_did;
vector[n_studies_did * (1 - is_baseline_normalised)] baseline_treatment_did;
vector[n_studies_did] time_trend_did;
vector<lower=-1, upper=1>[n_studies_did] rho_did;
vector<lower=0>[n_studies_did] sigma_control_before_did;
vector<lower=0>[n_studies_did] sigma_control_after_did;
vector<lower=0>[n_studies_did] sigma_treatment_before_did;
vector<lower=0>[n_studies_did] sigma_treatment_after_did;
