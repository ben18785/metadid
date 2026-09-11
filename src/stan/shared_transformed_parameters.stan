// shared_transformed_parameters.stan
//
// Compute effective treatment effect means for RCT and Pre-Post designs.
// DiD is the reference category (treatment_effect_mean_did == treatment_effect_mean).
// When is_design_effect == 1, delta_rct_raw[1] and delta_pp_raw[1] are
// additive offsets on the mean; when 0 they are not sampled and
// treatment_effect_mean_rct / treatment_effect_mean_pp equal treatment_effect_mean.

real treatment_effect_mean_rct = treatment_effect_mean;
real treatment_effect_mean_pp  = treatment_effect_mean;
if (is_design_effect) {
  treatment_effect_mean_rct += delta_rct_raw[1];
  treatment_effect_mean_pp  += delta_pp_raw[1];
}

// Stable reporting names for the baseline-imbalance hyperparameters. These
// exist whether or not the underlying parameter is sampled, so posterior
// summaries keep the same shape across configurations.
real baseline_difference_mean = is_mu_gamma_estimated ? mu_gamma_vec[1] : 0.0;
real kappa = is_kappa_estimated ? kappa_vec[1] : kappa_fixed;
