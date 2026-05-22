// prepost_summary_model_parameters.stan

vector[n_studies_pp_summary] treatment_effect_pp_summary_raw;
vector[n_studies_pp_summary * (1 - is_time_trend_pp_summary_zero)] time_trend_pp_summary_raw;

// Non-centered raw for the hierarchical pop-level treatment baseline. Only
// present in none mode with the full (non-differenced) likelihood. In
// none-mode + differenced the baseline cancels and nothing identifies it,
// so we declare it size-0 in that combination.
vector[n_studies_pp_summary * (1 - is_differenced_likelihood_pp_summary) * is_none_mode] baseline_treatment_pp_summary_raw;

// Per-study latent baseline parameter for modelled normalisation modes.
// Sized > 0 whenever is_modelled == 1, *independent* of the likelihood form.
//
// In treatment-latent mode this is directly b_T_pre[i], informed by
// x_bar_treatment_before_pp_summary[i] via an explicit likelihood term
// (added in prepost_summary_model.stan regardless of whether the
// effect-side likelihood is full or differenced).
//
// In control-latent mode this is the per-study b_C_pre[i]; for PP studies
// b_C_pre is not directly observed, so the latent's posterior is
// hierarchy-driven (via baseline_difference_mean).
//
// Critically: even when the differenced likelihood is in use, this latent
// is still needed to bridge from the canonical fractional scale that
// hierarchical pooling lives on to the absolute scale of the data — the
// differenced likelihood doesn't see the baseline, but the
// canonical-to-absolute multiplication of θ_T and β_T does.
//
// The clinically natural choice for PP is `normalise = TRUE,
// baseline_latent_arm = "treatment"`; selecting control-latent for PP is
// meaningful only when you specifically want to express the per-study
// latent on the control reference scale, accepting that it is
// hierarchy-driven for PP.
vector<lower=0, upper=baseline_prior_upper>[n_studies_pp_summary * is_modelled] baseline_per_study_latent_pp_summary;

// Fisher z values for studies with missing rho (used in the hierarchical
// rho machinery; mapped to per-study rho via tanh in the model block).
vector[n_rho_missing_pp_summary] z_rho_missing_pp_summary;
