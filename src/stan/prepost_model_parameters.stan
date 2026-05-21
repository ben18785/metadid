// prepost_model_parameters.stan

vector[n_studies_pp] treatment_effect_pp_raw;
vector[n_studies_pp * (1 - is_time_trend_pp_zero)] time_trend_pp_raw;

// Non-centered raw for the hierarchical pop-level treatment baseline. Only
// present in none mode with the full (non-differenced) likelihood.
vector[n_studies_pp * (1 - is_differenced_likelihood_pp) * is_none_mode] baseline_treatment_pp_raw;

// Per-study latent baseline parameter for modelled normalisation modes.
// Sized > 0 whenever is_modelled == 1, independent of the likelihood form.
//
// Critically: even when the differenced likelihood is in use, this latent
// is needed to bridge from the canonical fractional scale (where the
// hierarchical pooling lives) to the absolute scale of the data — the
// differenced likelihood doesn't see the baseline in its mean structure,
// but the canonical→absolute multiplication of θ_T and γ_T does. In the
// differenced+modelled branch the latent is anchored by an explicit
// likelihood term on the individual pre-treatment observations in
// prepost_model.stan; without that anchor the per-study scale would be
// non-identifiable.
vector<lower=0, upper=baseline_prior_upper>[n_studies_pp * is_modelled] baseline_per_study_latent_pp;

vector<lower=-1, upper=1>[n_studies_pp * (1 - is_differenced_likelihood_pp)] rho_pp;
vector<lower=0>[n_studies_pp * (1 - is_differenced_likelihood_pp)] sigma_treatment_before_pp;
vector<lower=0>[n_studies_pp * (1 - is_differenced_likelihood_pp)] sigma_treatment_after_pp;
vector<lower=0>[n_studies_pp * is_differenced_likelihood_pp] sigma_d_pp;
