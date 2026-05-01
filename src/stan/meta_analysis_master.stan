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
  // handles special case for equal baseline parameters i.e. diff is zero
  #include "rct_summary_model_transformed_parameters.stan"
  // computes treatment_effect_mean_rct and treatment_effect_mean_pp
  #include "shared_transformed_parameters.stan"
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
  real treatment_effect_sim = normal_rng(treatment_effect_mean, treatment_effect_sd);
}
