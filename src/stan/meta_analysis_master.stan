// master_model.stan

functions {
  #include "did_model_functions.stan"
  #include "rct_model_functions.stan"
  #include "prepost_model_functions.stan"
  #include "did_summary_model_functions.stan"
  #include "prepost_summary_model_functions.stan"
  #include "rct_summary_model_functions.stan"
}


data {
  #include "shared_data.stan"
  #include "did_model_data.stan"
  #include "rct_model_data.stan"
  #include "prepost_model_data.stan"
  #include "did_summary_model_data.stan"
  #include "prepost_summary_model_data.stan"
  #include "rct_summary_model_data.stan"
}

parameters {
  #include "did_model_parameters.stan"
  #include "rct_model_parameters.stan"
  #include "prepost_model_parameters.stan"
  #include "did_summary_model_parameters.stan"
  #include "prepost_summary_model_parameters.stan"
  #include "rct_summary_model_parameters.stan"
  #include "shared_parameters.stan"
}

transformed parameters {
  // 1. Compute design-effect means (needed by design-specific reconstructions)
  #include "shared_transformed_parameters.stan"
  // 2. Reconstruct study-level parameters from non-centered raw values
  #include "did_model_transformed_parameters.stan"
  #include "did_summary_model_transformed_parameters.stan"
  #include "prepost_model_transformed_parameters.stan"
  #include "prepost_summary_model_transformed_parameters.stan"
  // 3. RCT: reconstruct raw → actual, then derive treatment_effect from apparent_effect
  #include "rct_model_transformed_parameters.stan"
  #include "rct_summary_model_transformed_parameters.stan"
}

model {
  #include "did_model.stan"
  #include "rct_model.stan"
  #include "prepost_model.stan"
  #include "did_summary_model.stan"
  #include "prepost_summary_model.stan"
  #include "rct_summary_model.stan"
  #include "shared_priors.stan"
}

generated quantities {
  // Predictive draw for a new study at the covariate reference point
  // (covariate mean when centered, zero when not; ignores covariate variation).
  if(is_student_t_heterogeneity == 0) {
    real treatment_effect_sim = normal_rng(treatment_effect_mean, treatment_effect_sd);
  } else {
    real treatment_effect_sim = student_t_rng(nu_treatment_vec[1], treatment_effect_mean, treatment_effect_sd);
  }
}
