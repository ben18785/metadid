// prepost_model.stan

vector[n_studies_pp] treatment_effect_pp;
vector[n_studies_pp * (1 - is_baseline_normalised)] baseline_treatment_pp;
vector[n_studies_pp * (1 - is_time_trend_pp_zero)] time_trend_pp;
vector<lower=-1, upper=1>[n_studies_pp] rho_pp;
vector<lower=0>[n_studies_pp] sigma_treatment_before_pp;
vector<lower=0>[n_studies_pp] sigma_treatment_after_pp;
