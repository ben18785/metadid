// shared_transformed_data.stan
//
// Indicator flags derived from baseline_latent_mode, used for conditional
// vector sizing in per-design parameter / transformed-parameter blocks.
//
//   baseline_latent_mode == 1 → "treatment_baseline" (treatment-pre is latent)
//   baseline_latent_mode == 2 → "control_baseline"   (control-pre is latent)
//   baseline_latent_mode == 3 → "none"               (absolute-scale pooling)

int<lower=0, upper=1> is_modelled_treatment = baseline_latent_mode == 1 ? 1 : 0;
int<lower=0, upper=1> is_modelled_control   = baseline_latent_mode == 2 ? 1 : 0;
int<lower=0, upper=1> is_none_mode          = baseline_latent_mode == 3 ? 1 : 0;
int<lower=0, upper=1> is_modelled           = 1 - is_none_mode;
